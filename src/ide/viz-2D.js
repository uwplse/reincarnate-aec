// Functions to manage the 2D visualzier

// Draw the axes for 2D
var drawAxes2D = function(faces, viewbox, domainViewbox) {
    var width = viewbox.x.max - viewbox.x.min;
    var height = viewbox.y.max - viewbox.y.min;
    // Add the x axis and offset it so it appears at the bottom
    canvas.append('g')
          .attr('transform', 'translate(' +
                constants.twoD.xAxisMarginLeft + ',' +
                (height - constants.twoD.xAxisMarginBottom) + ')')
          .call(d3.axisBottom(d3.scaleLinear()
                                .domain([domainViewbox.x.max,
                                         domainViewbox.x.min])
                                .range([width -
                                        constants.twoD.xAxisMarginLeft * 2,
                                        0])));
    // Add the y axis and offset it so it appears at the left side
    canvas.append('g')
          .attr('transform', 'translate(' +
                constants.twoD.yAxisMarginLeft + ',' +
                (constants.twoD.yAxisMarginBottom) + ')')
          .call(d3.axisLeft(d3.scaleLinear()
                              .domain([domainViewbox.y.max,
                                       domainViewbox.y.min])
                              .range([0,
                                      height -
                                      constants.twoD.yAxisMarginBottom * 2])));
};

// Generates a viewbox object for 2D
// returns {
//     x: {
//         min: <x min>,
//         max: <x max>,
//     y: {
//         min: <y min>,
//         max: <y max>
//     }
// }
// Sets the viewbox of the svg to the returned viewbox
var generateViewbox2D = function() {
    var width = $('svg').width();
    var height = constants.twoD.canvasHeight;
    $('svg').height(height);

    var size = Math.min(width, height);
    $('svg').attr('viewBox', '0 0 ' + size + ' ' + size);

    return {
        x: {
            min: 0,
            max: size
        },
        y: {
            min: 0,
            max: size
        }
    };
};

// Generates a domain viewbox for 2D and returns it
// The viewbox looks like {
//     x: {
//         min: <x min>,
//         max: <x max>,
//     y: {
//         min: <y min>,
//         max: <y max>
//     }
// }
var generateViewboxDomain2D = function(faces) {
    // Find the absolute maximum and minimum points to be drawn
    // in each dimension
    var xMin = _.min(faces, function(a) {
        return a.x1 < a.x2 ? a.x1 : a.x2;
    });
    xMin = xMin.x1 < xMin.x2 ? xMin.x1 : xMin.x2;
    var xMax = _.max(faces, function(a){
        return a.x1 > a.x2 ? a.x1 : a.x2;
    });
    xMax = xMax.x1 > xMax.x2 ? xMax.x1 : xMax.x2;
    var yMin = _.min(faces, function(a){
        return a.y1 < a.y2 ? a.y1 : a.y2;
    });
    yMin = yMin.y1 < yMin.y2 ? yMin.y1 : yMin.y2;
    var yMax = _.max(faces, function(a){
        return a.y1 > a.y2 ? a.y1 : a.y2;
    });
    yMax = yMax.y1 > yMax.y2 ? yMax.y1 : yMax.y2;

    var xDomainMin = xMin - (xMax - xMin) * constants.twoD.xDomainPadding;
    var xDomainMax = xMax + (xMax - xMin) * constants.twoD.xDomainPadding;
    var yDomainMin = yMin - (yMax - yMin) * constants.twoD.yDomainPadding;
    var yDomainMax = yMax + (yMax - yMin) * constants.twoD.yDomainPadding;

    // Return the viewbox
    return {
        x: {
            min: xDomainMin,
            max: xDomainMax
        },
        y: {
            min: yDomainMin,
            max: yDomainMax
        }
    };
};

// Draw faces in 2D to svg
var drawFaces2D = function(faces, viewbox, viewboxDomain) {
    // Draw the faces
    for (var i = 0; i < faces.length; i++) {
        // Transform the face into svg coordinates
        var face = faces[i];
        var transformedFace = transformFace(2, viewboxDomain, face, viewbox);

        // Adds a circle for the first point in the face
        canvas.append('circle')
              .attr('cx', transformedFace.x1)
              .attr('cy', transformedFace.y1)
              .attr('r', constants.twoD.circleRadius);

        // Adds a circle for the last point in the face
        canvas.append('circle')
              .attr('cx', transformedFace.x2)
              .attr('cy', transformedFace.y2)
              .attr('r', constants.twoD.circleRadius);

        // Adds a line between the two points showing where the face is
        canvas.append('line')
              .attr('x1', transformedFace.x1)
              .attr('x2', transformedFace.x2)
              .attr('y1', transformedFace.y1)
              .attr('y2', transformedFace.y2)
              .attr('stroke-width', constants.twoD.circleRadius)
              .attr('stroke', 'black');
    }

    // Find and shade the inside of each closed mesh part.
    // Each closed loop of mesh faces will be called a "part."

    // Number of faces that have been included in a completed part
    var numFilled = 0;
    // Index for number of faces processed
    var i;
    // Start of closed loop for comparison to find end of loop
    // At beginning, the start is the first face
    var base = faces[0];
    // Last face to match with next face to form loop
    // At beginning, the last face is the first one
    var last = base;
    // Buffer for storing the closed loop of faces being constructed
    // Starts with base as the only entry
    var buffer = [base];
    // Array for storing which of the faces have already been used in a
    // loop
    var used = new Array(faces.length);
    // The first base has been used
    used[0] = true;
    // All other faces have yet to be used
    for (var i = 1; i < faces.length; i++) {
        used[i] = false;
    }
    // Start iterating assuming we have already processed the first face
    i = 1;

    // Continue looping until we have looked at every element or
    // all faces have been included in a closed loop.
    // Because of the use of an index, this loop will
    // exit iff the mesh is invalid, i.e. there are faces
    // which do not fit in a closed loop
    while (numFilled < faces.length && i < faces.length) {
        // Look for the next face which fits in with the loop we are forming
        for (var j = 0; j < faces.length; j++) {
            // If this face is the next in the loop, process it
            if (used[j] == false && faces[j].x1 == last.x2 &&
                faces[j].y1 == last.y2) {
                // Add this face to the list of added faces for this loop
                buffer.push(faces[j]);
                // Mark that we have used this face in a loop
                used[j] = true;
                // Mark this face as the one to match the next face with
                last = faces[j];
                // If we have closed the loop, end if out of faces or start
                // a new one if possible.
                if (faces[j].x2 == base.x1 && faces[j].y2 == base.y1) {
                    // Add the fill polygon matching the found points on the loop
                    canvas.insert('polygon', 'circle')
                          .attr('points', _.map(buffer, function(a) {
                              return a.x1 + ',' + a.y1 +
                                     '\n' + a.x2 + ',' + a.y2 + '\n';
                          }))
                          .attr('fill','#e7e7e7');
                    // If there are no more unused points, we are done drawing
                    if (_.filter(used, function(a) {
                                     return a;
                                 }).length == faces.length) {
                        return;
                    }
                    // Otherwise, look for another face that has not been used
                    // and try to find another closed loop off of it.
                    for (var k = 0; k < faces.length; k++) {
                        if (used[k] == false) {
                            // Reinitialize variables for a new loop
                            base = faces[k];
                            last = base;
                            used[k] = true;
                            numFilled += buffer.length;
                            buffer = [base];
                            break;
                        }
                    }
                }
                // If we have already found the next face, stop looking for it
                break;
            }
        }
        // We processed one more
        i++;
    }

    // The loop exited without all faces being added to a close loop,
    // so the mesh was invalid. Print this error to the console and
    // stop execution.
    if (numFilled < faces.length) {
        throw new Error('Mesh was invalid');
    }
};

// Convert a point in 2D into svg coordinates
var transformFace2D = function (viewboxDomain, face, viewbox) {
    // Calculate the viewbox and viewboxDomain dimensions
    var viewboxDomainWidth = viewboxDomain.x.max - viewboxDomain.x.min;
    var viewboxDomainHeight = viewboxDomain.y.max - viewboxDomain.y.min;
    var viewboxWidth = viewbox.x.max - viewbox.x.min;
    var viewboxHeight = viewbox.y.max - viewbox.y.min;

    // Transform an x coordinate
    var transformX = function(x) {
        return constants.twoD.xAxisMarginLeft +
               ((x - viewboxDomain.x.min) /
                viewboxDomainWidth *
                (viewboxWidth - constants.twoD.xAxisMarginLeft * 2));
    };
    var transformY = function(y) {
        // Y coordinate as measured from the bottom of the screen
        var fromBottom = constants.twoD.yAxisMarginBottom +
                         (y - viewboxDomain.y.min) /
                         viewboxDomainHeight *
                         (viewboxHeight - constants.twoD.yAxisMarginBottom * 2);
        // Y-coord from top, in svg coordinates
        return viewboxHeight - fromBottom;
    };

    // Transform each coordinate in the face
    face.x1 = transformX(face.x1);
    face.x2 = transformX(face.x2);
    face.y1 = transformY(face.y1);
    face.y2 = transformY(face.y2);

    // Return the transformed face
    return face;
};

var toObject2D = function(ideOut) {
    // Get the array of face strings by removing the [] and \n and splitting by ;
    // Then, turn each face into an array of x and y coordinates, and parse them
    // into the face object. Return the resulting array
    return ideOut.replace(/[\[\]\n]/g, '')
                 .split(';')
                 .map(function(s) {
                   return s.split(',')
                           .map(function(ss) {
                             return ss.replace(/[\(\)]/g, '')
                           });
                 }).map(function(coords) {
                   return { "x1": parseFloat(coords[0])
                          , "y1": parseFloat(coords[1])
                          , "x2": parseFloat(coords[2])
                          , "y2": parseFloat(coords[3])
                          };
                 });
}

var visualize2D = function() {
    var facesText = editors['mesh'].getValue();
    drawVisualizer(toObject2D(facesText));
};
