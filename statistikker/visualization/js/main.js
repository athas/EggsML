var bonds_url = 'data/bonds.json';
var densities_url = 'data/densities.json';
var wrapper;
var base_scale = 39; // magic base scale
var max_density;

var CHARGE = -200;
var DISTANCE = 40;
var WIDTH = 800;
var HEIGHT = 600;



function setup_page() {
    json_to_object(bonds_url, function(bonds) {
        json_to_object(densities_url, function(densities) {
            d3.select('#loading').transition().style('display' , 'none');
            build_page(bonds, densities);
        });
    });
}


// [string, int] => int
function find_max_value(list){
    var highest = 0;

    $.each(list, function(i, item){
        var number = item[1];
        if (number > highest){
            highest = number;
        }
    });
    return highest;
    }

function build_page(bonds, densities) {
    var graph = new Object();
    graph["nodes"] = new Array();
    graph["links"] = new Array();

    wrapper = d3.select('#wrapper');

    var nodeById = d3.map();

    max_density = find_max_value(densities);

    $.each(densities, function(i, d ){
        var ware_name, density;
        ware_name = d[0];
        density = d[1];
        var ware = new Object();
        ware["id"] = nodeById.set(ware_name, i);
        ware["density"] = density;
        graph["nodes"].push(ware);
    });

    $.each(bonds, function(i, d){
        var ware, other_ware, probability;
        ware = d[0];
        other_ware = d[1];
        probability = d[2];

        var link = new Object();
        link["source"] = nodeById.get(ware);
        link["target"] = nodeById.get(other_ware);
        link["probability"] = probability;
        graph["links"].push(link);
    });


    force = d3.layout.force()
        .charge(CHARGE)
        .linkDistance(DISTANCE)
        .size([WIDTH, HEIGHT]);

    var panel = wrapper.append('svg')
        .attr('width', WIDTH)
        .attr('height', HEIGHT);

    force
        .nodes(graph["nodes"])
        .links(graph["links"])
        .start();


    var link = panel.selectAll(".link")
        .data(graph.links)
      .enter().append("line")
        .attr("class", "link");

    var node = panel.selectAll(".node")
        .data(graph.nodes)
        .enter().append("circle")
        .attr("class", "node")
        .attr("r", 6)
        .style("fill", function(d) {return d.id; })
        .call(force.drag);

    force.on("tick", function() {
      link.attr("x1", function(d) { return d.source.x; })
          .attr("y1", function(d) { return d.source.y; })
          .attr("x2", function(d) { return d.target.x; })
          .attr("y2", function(d) { return d.target.y; });

      node.attr("cx", function(d) { return d.x; })
          .attr("cy", function(d) { return d.y; });
    });

    // Text box for crime descriptions and crime districts.
}


function hash_grey(i) {
    // 10 districts => i in [0..9]
    return '#' + (50 + i * 10).toString(16).repeat(3);
}

function hash_color(s) {
    var n = 0;
    for (var i = 0; i < s.length; i++) {
        n += s.charCodeAt(i);
    }
    return ('hsl(' + Math.floor(Math.abs(Math.cos(n)) * 360) + ', '
            + (15 + Math.floor(Math.abs(Math.sin(n)) * 60)) + '%, 80%)');
}


String.prototype.capitalize = function() {
    return this.charAt(0).toUpperCase() + this.slice(1).toLowerCase();
}

String.prototype.simplify = function() {
    return this.replace(/[^A-Z0-9]/ig, '');
}

function json_to_object(link, callback) {
    d3.json(link, function(loaded_data) {
        callback(loaded_data);
    });
}

// Build the entire document.
setup_page();
