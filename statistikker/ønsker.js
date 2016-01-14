// Settings

var PLOT_HEIGHT = 400;

var SUBPLOT_WIDTH = 400;
var SUBPLOT_HEIGHT = 20;


// Startup

d3.json('ønsker.json', function(error, json) {
    if (error) {
        document.write(error);
    }
    else {
        visualize(json);
    }
});


// Helpers

function choose_color(f) {
    var h = f * 360;
    var s = 50;
    var l = 70;
    
    return ('hsl(' +
            h + ', ' +
            s + '%, ' +
            l + '%)');
}

function unix_to_year(u) {
    return 1970 + u / (365.24 * 24 * 60 * 60);
}

// Visualizer

function visualize(json_data) {
    var update_composite = visualize_composite_prepare(json_data);
    d3.select('body').append('hr');
    visualize_parts(json_data, update_composite);
}

function visualize_composite_prepare(json_data) {
    var time_min = json_data[0]
    var time_max = json_data[1]

    var wrapper = d3.select('body')
        .append('div')
        .attr('id', 'composite');

    var line_plot = wrapper.append('svg');
    line_plot.attr('height', PLOT_HEIGHT);

    function update_composite(timestamps, n_groups) {
        var plot_width = window.innerWidth;
        line_plot.attr('width', plot_width);

        var scale = d3.scale.linear()
            .domain([time_min, time_max])
            .range([40, plot_width - 40]);

        var scale_year = d3.scale.linear()
            .domain([unix_to_year(time_min), unix_to_year(time_max)])
            .range([40, plot_width - 40]);

        var scale_row = d3.scale.linear()
            .domain([0, n_groups])
            .range([30, PLOT_HEIGHT]);
    
        var axis = d3.svg.axis()
            .scale(scale_year)
            .tickValues([2013, 2014, 2015, 2016])
            .tickFormat(d3.format('d'))
            .orient('bottom');
        line_plot.append('g')
            .attr('class', 'axis')
            .call(axis);

        line_plot.selectAll('line').remove();
        var lines = line_plot.selectAll('line')
            .data(timestamps)
            .enter()
            .append('line');

        lines.style('stroke', function(d) {
            return d['color'];
        });

        lines.attr('y1', function(d) {
            return scale_row(d['row']);
        });
        lines.attr('y2', function(d) {
            return scale_row(d['row'] + 1);
        });
        lines.attr('x1', function(d) {
            return scale(d['time']);
        });
        lines.attr('x2', function(d) {
            return scale(d['time']);
        });
    }

    update_composite([]);

    return update_composite;
}

function visualize_parts(json_data, update_composite) {
    var time_min = json_data[0]
    var time_max = json_data[1]

    var wares_orig = json_data[2];
    var wares = [];
    for (var i in wares_orig) {
        var pair = wares_orig[i];
        wares.push({
            'title': pair[0],
            'timestamps': pair[1],
            'active': false,
            'color': choose_color(Math.sin(i))
        });
    }

    var scale = d3.scale.linear()
        .domain([time_min, time_max])
        .range([0, SUBPLOT_WIDTH]);
    
    var wrapper = d3.select('body')
        .append('div')
        .attr('id', 'parts');

    wrapper.style('height', (window.innerHeight
                             - document.getElementById('parts').offsetTop - 40) + 'px');

    var ware_divs = wrapper.selectAll('div')
        .data(wares)
        .enter()
        .append('div');

    ware_divs.style('background-color', function(d) {
        return d['color'];
    });

    var titles = ware_divs.append('p');
    titles.text(function(d) {
        return d['title'];
    });
    titles.style('width', SUBPLOT_WIDTH + 'px');


    var line_plots = ware_divs.append('svg');
    line_plots.attr('width', SUBPLOT_WIDTH);
    line_plots.attr('height', SUBPLOT_HEIGHT);

    var lines = line_plots.selectAll('line')
        .data(function(d) {
            return d['timestamps'];
        })
        .enter()
        .append('line');

    ware_divs.on('click', function(d) {
        var elem = d3.select(this);
        d['active'] = !d['active'];
        if (d['active']) {
            elem.attr('class', 'clicked');
        }
        else {
            elem.attr('class', '');
        }
        var wares_active = [];
        var n_wares_active = 0;
        for (var i in wares) {
            if (wares[i]['active']) {
                Array.prototype.push.apply(
                    wares_active,
                    wares[i]['timestamps'].map(function(t) {
                        return {
                            'time': t,
                            'color': wares[i]['color'],
                            'row': n_wares_active
                        };
                    }));
                n_wares_active++;
            }
        }
        update_composite(wares_active, n_wares_active);
    });
    
    lines.attr('y1', 0);
    lines.attr('y2', SUBPLOT_HEIGHT);
    lines.attr('x1', function(d) {
        return scale(d);
    });
    lines.attr('x2', function(d) {
        return scale(d);
    });
}
