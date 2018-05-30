// Functions to manage the 1D visualzier

// Draw the axes for 1D
var drawAxes1D = function(faces, viewbox, domainViewbox) {
    var width = viewbox.max - viewbox.min;
    var domainMax = domainViewbox.max;
    var domainMin = domainViewbox.min;
    // Add the number line with the calculated scale
    canvas.append('g')
          .attr('transform', 'translate(' +
                constants.oneD.axisMarginLeft +
                ',' + constants.oneD.axisMarginTop + ')')
          .call(d3.axisBottom(d3.scaleLinear()
                                .domain([domainMax, domainMin])
                                .range([width -
                                        constants.oneD.axisMarginLeft * 2,
                                        0])));
};

// Generate a viewbox object for 1D
// returns {min: 0, max: width}
// Sets viewbox attribute of svg
var generateViewbox1D = function() {
    var width = $('svg').width();
    var height = constants.oneD.canvasHeight;
    $('svg').height(height);

    $('svg').attr('viewBox', '0 0 ' + width + ' ' + height);

    return {
        min: 0,
        max: width
    };
};

// Generates a domain viewbox and returns it
var generateViewboxDomain1D = function(faces) {
    var min = _.min(faces);
    var max = _.max(faces);
    // Here, 'domain' means 'in the units of the number line'
    var domainMin = min - (max - min) * constants.oneD.domainPadding;
    var domainMax = max + (max - min) * constants.oneD.domainPadding;
    return {min: domainMin, max: domainMax};
};

// Draw the faces (and lines between them) to the svg for 1D
var drawFaces1D = function (faces, viewbox, viewboxDomain) {
    // Sort the points so the flag handles the norms correctly
    _.sortBy(faces, function(a) { return a; });
    var lastPoint;
    // Iff flag is true, draw a line between lastPoint and x
    var flag = false;
    for (var i = 0; i < faces.length; i++) {
        var x = faces[i];
        // Transform the face into svg coordinates
        var transformedX = transformFace(1, viewboxDomain, x, viewbox);

        // Draw the face
        canvas.append('circle')
              .attr('cx', transformedX)
              .attr('cy', constants.oneD.axisMarginTop)
              .attr('r', constants.oneD.circleRadius);

        // Draw the lines showing what is in the mesh
        if (flag) {
            var transformedLastPoint = transformFace(1, viewboxDomain,
                                                         lastPoint, viewbox);
            canvas.append('line')
                  .attr('x1', transformedLastPoint)
                  .attr('y1', constants.oneD.axisMarginTop)
                  .attr('x2', transformedX)
                  .attr('y2', constants.oneD.axisMarginTop)
                  .attr('stroke-width', constants.oneD.circleRadius)
                  .attr('stroke', 'black');
        }
        lastPoint = x;
        // Flip the flag so we only draw the lines every other face
        flag = !flag;
    }
};

// Convert a number in number line coordinates to svg coordinates for 1D
var transformFace1D = function (viewboxDomain, face, viewbox) {
    var domainMin = viewboxDomain.min;
    var domainMax = viewboxDomain.max;
    var axisWidth = viewbox.max - viewbox.min -
                    constants.oneD.axisMarginLeft * 2;
    return constants.oneD.axisMarginLeft +
           ((face - domainMin) / (domainMax - domainMin)) * axisWidth;
};

// Turn the ocaml_of_js ide output into a javascript object for the visualizer
var toObject1D = function(ideOut) {
    ideOut = ideOut.replace(/;/g, ',');
    return JSON.parse(ideOut);
};

var visualize1D = function() {
    var facesText = editors['mesh'].getValue();
    drawVisualizer(toObject1D(facesText));
};
