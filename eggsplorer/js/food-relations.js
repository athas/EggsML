// Settings.
var bonds_url = 'data/bonds.json';
var densities_url = 'data/densities.json';
var temporal_url = 'data/temporal.json';
var CHARGE = -400;
var DISTANCE = 600;
var WIDTH = window.innerWidth;
var HEIGHT = window.innerHeight;
var TEMPORAL_PLOT_HEIGHT = 200;
var TEXT_LABEL_WIDTH_FACTOR = 6;

var WIDESCREEN_FIX = (16/9);


// Globals.
var wrapper;
var max_density;
var infobox;
var infobox_current_ware;
var ware_divs_lookup;
var graph;

function setup_page() {
  json_to_object(bonds_url, function(bonds) {
    json_to_object(densities_url, function(densities) {
      json_to_object(temporal_url, function(temporal) {
        build_page(bonds, densities, temporal);
        setTimeout(function() {
            $('#bagtaeppe').animate({top : '-2000px'}, 3000, function() {
                $('#bagtaeppe').remove();
            });
            $('#loading').animate({opacity: '0'}, 2000, function() {
                $('#loading').remove();
            });
        }, 1999);
      });
    });
  });
}

function build_page(bonds, densities, temporal) {
    graph = new Object();
    graph['nodes'] = new Array();
    graph['links'] = new Array();

    wrapper = d3.select('#wrapper');

    var nodeById = d3.map();

    max_density = find_max_value(densities);

    $.each(densities, function(i, d) {
        var ware_name, density;
        ware_name = d[0];
        density = d[1];
        var ware = new Object();
        nodeById.set(i, ware_name);
        ware['id'] = nodeById.set(ware_name, i);
        ware['ware_name'] = ware_name;
        ware['density'] = density;
        graph['nodes'].push(ware);
    });

    $.each(bonds, function(i, d) {
        var ware, other_ware, probability;
        ware = d[0];
        other_ware = d[1];
        probability = d[2];

        var link = new Object();
        link['source'] = nodeById.get(ware);
        link['target'] = nodeById.get(other_ware);
        link['probability'] = probability;
        link['stroke'] = probability_to_color(probability);
        link['strokewidth'] = probability_to_strokewidth(probability);
        graph['links'].push(link);
    });


    force = d3.layout.force()
        .charge(CHARGE)
        .linkDistance(DISTANCE)
        .size([WIDTH, HEIGHT]);

    var panel = wrapper.append('svg')
        .attr('width', WIDTH)
        .attr('height', HEIGHT);

    var filter_text_bg = panel
        .append('defs')
        .append('filter')
        .attr('x', 0)
        .attr('y', 0)
        .attr('width', 1)
        .attr('height', 1)
        .attr('id', 'text-background');
    filter_text_bg
        .append('feFlood')
        .attr('flood-color', 'rgba(255, 255, 255, 0.8)');
    filter_text_bg
        .append('feComposite')
        .attr('in', 'SourceGraphic');

    force
        .nodes(graph['nodes'])
        .links(graph['links'])
        .start();

    var link = panel.selectAll('line')
        .data(graph.links)
        .enter()
        .append('line')
        .attr('class', function(d) {
            var out = ['link', d.target.id , d.source.id];
            return out.join(' ');
        })
        .style('stroke', function(d) {
            return d.stroke;
        })
        .style('stroke-width', function(d) {
            return d.strokewidth;
        })
        .style('display', 'none');

    var node = panel.selectAll('g')
        .data(graph.nodes)
        .enter()
        .append('g');

    node
        .append('circle')
        .attr('class', 'node')
        .attr('x', WIDTH / 2)
        .attr('y', HEIGHT / 2)
        .attr('r', function(d) {
            return area_to_radius(200 * d.density);
        })
        .style('fill', function(d) {
            return random_circle_color();
        })
        .style('stroke', 'black')
        .style('stroke-width', '1px')
        .on('dblclick' , function(d) {
            show_ware_info(d['ware_name']);
        })
        .on('mouseover', function(d) {
            $('.'+d.id.toString()).css('display', 'inline');
        })
        .on('mouseout', function() {
            $('.link').css('display', 'none');
        })
        .call(force.drag);

    var node_textbox = node
        .append('svg')
        .attr('x', WIDTH / 2)
        .attr('y', HEIGHT / 2)
        .attr('width', function(d) {
            return TEXT_LABEL_WIDTH_FACTOR * 2 * area_to_radius(200 * d.density);
        })
        .attr('height', function(d) {
            return TEXT_LABEL_WIDTH_FACTOR * 2 * area_to_radius(200 * d.density);
        });

    node_textbox
        .append('text')
        .attr('class', 'label')
        .attr('filter', 'url(#text-background)')
        .text(function(d) {
            return nodeById.get(d['id']);
        });

      force.on('tick', function() {
          link
              .attr('x1', function(d) { return d.source.x; })
              .attr('y1', function(d) { return d.source.y; })
              .attr('x2', function(d) { return d.target.x; })
              .attr('y2', function(d) { return d.target.y; });

          node.selectAll('circle')
              .attr('cx', function(d) {
                  d.x = Math.max(node.attr('r'), Math.min(WIDTH - node.attr('r'), d.x));
                  return d.x;
              })
              .attr('cy', function(d) {
                  d.y = Math.max(node.attr('r'), Math.min(HEIGHT - node.attr('r'), d.y));
                  return d.y;
              });

          node.selectAll('svg')
              .attr('x', function(d) {
                  return d.x - TEXT_LABEL_WIDTH_FACTOR * area_to_radius(200 * d.density);
              })
              .attr('y', function(d) {
                  return d.y - TEXT_LABEL_WIDTH_FACTOR * area_to_radius(200 * d.density);
              });

          node.selectAll('svg').selectAll('text')
              .attr('x', function(d) {
                  return TEXT_LABEL_WIDTH_FACTOR * area_to_radius(200 * d.density);
              })
              .attr('y', function(d) {
                  return TEXT_LABEL_WIDTH_FACTOR * area_to_radius(200 * d.density);
              });
      });

    infobox = wrapper.append('div')
        .attr('id', 'infobox');

    infobox.append('p')
        .attr('class', 'close-button')
        .text('Close info box')
        .on('click', close_ware_info);

    // The temporal graph for each ware.
    var time_min = temporal[0];
    var time_max = temporal[1];

    var wares_orig = temporal[2];
    var wares = [];

    for (var i in wares_orig) {
        var pair = wares_orig[i];
        wares.push({
            'title': pair[0],
            'timestamps': pair[1],
            'color': choose_ware_color(Math.sin(i)) ,
            'links': links_for_ware(pair[0], graph.links)
        });
    }

    var temporal_plot_width = WIDTH - 40;

    var time_range = [20, temporal_plot_width - 20];

    var years_elapsed = unix_to_year(time_max) - unix_to_year(time_min);

    var time_scale = d3.scale.linear()
        .domain([time_min, time_max])
        .range(time_range);


    var scale_year = d3.scale.linear()
        .domain([unix_to_year(time_min), unix_to_year(time_max)])
        .range(time_range);

    var ware_divs = infobox.selectAll('div')
        .data(wares)
        .enter()
        .append('div')
        .attr('class', 'ware');

    ware_divs_lookup = {};
    for (var i in wares) {
        ware_divs_lookup[wares[i]['title']] = d3.select(ware_divs[0][i]);
    }

    var info_titles = ware_divs.append('h1');
    info_titles.text(function(d) {
        return d['title'];
    });

    ware_divs.append('h2')
        .text('Requests for this product');

    var line_plots = ware_divs.append('svg');
    line_plots.attr('width', temporal_plot_width);
    line_plots.attr('height', TEMPORAL_PLOT_HEIGHT);

    var lines = line_plots.selectAll('line')
        .data(function(d) {
            return d['timestamps'];
        })
        .enter()
        .append('line');

    lines.attr('y1', 30);
    lines.attr('y2', TEMPORAL_PLOT_HEIGHT);
    lines.attr('x1', function(d) {
        return time_scale(d);
    });
    lines.attr('x2', function(d) {
        return time_scale(d);
    });

    var axis = d3.svg.axis()
        .scale(scale_year)
        .tickValues([2013, 2014, 2015, 2016])
        .tickFormat(d3.format('d'))
        .orient('bottom');
    line_plots.append('g')
        .attr('class', 'axis')
        .call(axis);

    ware_divs.append('h2')
        .text('Connections with other products');

    ware_divs
        .append('ul')
        .selectAll('li')
        .data(function(d) {
            return d['links'];
        })
        .enter()
        .append('li')
        .text(function(link) {
            return (link.target.ware_name + ': '
                    + (link.probability * 100).toFixed(0) + '%');
        });
}

function show_ware_info(ware_name) {
    infobox_current_ware = ware_name;
    infobox.style('visibility', 'visible');
    ware_divs_lookup[ware_name].style('display', 'block');
}

function close_ware_info() {
    infobox.style('visibility', 'hidden');
    ware_divs_lookup[infobox_current_ware].style('display', 'none');
}


// Helper functions.

function json_to_object(link, callback) {
    d3.json(link, function(loaded_data) {
        callback(loaded_data);
    });
}

function links_for_ware(ware, links){
    var out = [];
    for (var i in links) {
        if (links[i].source.ware_name == ware){
            out.push(links[i]);
        }
    }

    out = out.sort(function(a, b) {
        return b.probability - a.probability;
    });
    return out;
}

function probability_to_color(probability){
    if (probability > 0.6) {
        return 'green';
    }
    else if (probability > 0.4) {
        return 'yellow';
    }
    else if (probability > 0.2) {
        return 'orange';
    }
    else {
        return 'red';
    }
}

function probability_to_strokewidth(probability){
    if (probability > 0.6) {
        return '40px';
    }
    else if (probability > 0.4) {
        return '30px';
    }
    else if (probability > 0.2) {
        return '20px';
    }
    else {
        return '10px';
    }
}

// [string, int] => int
function find_max_value(list){
    var highest = 0;

    $.each(list, function(i, item) {
        var number = item[1];
        if (number > highest) {
            highest = number;
        }
    });
    return highest;
}

function random_circle_color(s) {
    return ('hsl(' +
            Math.floor(Math.random() * 360) + ', ' +
            (35 + Math.floor(Math.random() * 50)) + '%, ' +
            (70 + Math.floor(Math.random() * 10)) + '%)');
}

function choose_ware_color(f) {
    var h = f * 360;
    var s = 50;
    var l = 70;

    return ('hsl(' +
            h + ', ' +
            s + '%, ' +
            l + '%)');
}

function unix_to_year(u) {
    // Approx.
    return 1970 + u / (365.24 * 24 * 60 * 60);
}

function area_to_radius(a) {
    return Math.sqrt(a / Math.PI);
}

function toggle_div(div) {
    if ($(div).css('display') != 'none') {
        $(div).css('display', 'none');
    }
    else {
        $(div).css('display', 'block');
    }
}


// Build the entire document.
setup_page();
