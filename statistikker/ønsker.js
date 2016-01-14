// Settings

PLOT_WIDTH = 800;
PLOT_HEIGHT = 100;


// Startup

d3.json('ønsker.json', function(error, json) {
    if (error) {
        document.write(error);
    }
    else {
        visualize(json);
    }
});


// Visualizer

function visualize(json_data) {
    var time_min = json_data[0]
    var time_max = json_data[1]

    var wares_orig = json_data[2];
    var wares = [];
    for (var i in wares_orig) {
        var pair = wares_orig[i];
        wares.push({
            'title': pair[0],
            'timestamps': pair[1]
        });
    }

    var scale = d3.scale.linear()
        .domain([time_min, time_max])
        .range([0, PLOT_WIDTH]);
    
    var ware_divs = d3.select('body').selectAll('div')
        .data(wares)
        .enter()
        .append('div');

    var titles = ware_divs.append('p');
    titles.text(function(d) {
        return d['title'];
    });

    var line_plots = ware_divs.append('svg');
    line_plots.attr('width', PLOT_WIDTH);
    line_plots.attr('height', PLOT_HEIGHT);

    var lines = line_plots.selectAll('line')
        .data(function(d) {
            return d['timestamps'];
        })
        .enter()
        .append('line')

    lines.attr('y1', 0);
    lines.attr('y2', PLOT_HEIGHT);
    lines.attr('x1', function(d) {
        return scale(d);
    });
    lines.attr('x2', function(d) {
        return scale(d);
    });
}
