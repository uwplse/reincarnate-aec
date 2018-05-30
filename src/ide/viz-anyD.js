// Functions for splitting control to the appropriate dimention

// Constants for each dimention, mostly for display purposes
var constants = {
    oneD: {
        canvasHeight: 100,
        domainPadding: 0.2,
        axisMarginLeft: 30,
        axisMarginTop: 40,
        circleRadius: 3
    },
    twoD: {
        canvasHeight: 500,
        xDomainPadding: 0.2,
        yDomainPadding: 0.2,
        xAxisMarginLeft: 30,
        xAxisMarginBottom: 30,
        yAxisMarginLeft: 30,
        yAxisMarginBottom: 30,
        circleRadius: 2
    },
    threeD: {
    },
    marginTop: 40
};

// Make constants a constant
constants = Object.freeze(constants);

// Calls sepearte functions with specified arguments depending on the
// input dimention.
var splitDim = function (dim, oneD, twoD, threeD, arguments) {
    if (arguments == undefined) {
        arguments = [];
    }

    if (dim == 1) {
        return oneD.apply(undefined, arguments);
    } else if (dim == 2) {
        return twoD.apply(undefined, arguments);
    } else if (dim == 3) {
        return threeD.apply(undefined, arguments);
    } else {
        throw new Error('Dimention was not valid');
    }
};

// Find the dimention accociated with the input faces
var findDim = function(faces) {
    if (isJson(faces)) {
        faces = getJson(faces).faces;
    } else {
        return 3;
    }

    if (faces && faces[0] && faces[0].x1 != undefined &&
        !faces[0].x3 != undefined) {
        return 2;
    } else if (faces && faces[0] != undefined &&
               typeof(faces[0]) == 'number') {
        return 1;
    } else {
        return 3;
    }
};

// Initialize the svg (for 2D and 1D)
var initializeSvg = function() {
    canvas = d3.select('#visualizer').append('svg')
}

// Initialize a canvas for drawing
var initializeCanvas = function(dim) {
    splitDim(dim, initializeSvg, initializeSvg, initializeCanvas3D);
};

// Clear the canvas (svg) for 1D and 2D
var clearSvg = function(dim) {
    $('svg').remove();
};

// Clear the canvas
var clear = function (dim) {
    splitDim(dim, clearSvg, clearSvg, clear3D);
};

// Draw the axes
var drawAxes = function(dim, faces, viewbox, domainViewbox) {
    splitDim(dim, drawAxes1D, drawAxes2D,
             drawAxes3D, [faces, viewbox, domainViewbox]);
};

// Generate a viewbox object and return it
var generateViewbox = function(dim) {
    return splitDim(dim, generateViewbox1D,
                    generateViewbox2D, generateViewbox3D)
};

// Returns a domain viewbox
var generateViewboxDomain = function(dim, faces) {
    return splitDim(dim, generateViewboxDomain1D, generateViewboxDomain2D,
                    generateViewboxDomain3D, [faces]);
};

// Draw faces
var drawFaces = function(dim, faces, viewbox, viewboxDomain) {
    splitDim(dim, drawFaces1D, drawFaces2D, drawFaces3D,
             [faces, viewbox, viewboxDomain]);
};

// Converts a point from axes coordinates into draw coordinates
var transformFace = function(dim, viewboxDomain, face, viewbox) {
    return splitDim(dim, transformFace1D, transformFace2D,
                    transformFace3D, [viewboxDomain, face, viewbox]);
};

var toObject = function(dim, ideOut) {
    return splitDim(dim, toObject1D, toObject2D, toObject3D, [ideOut]);
};

// Returns true iff string contains a valid json string
var isJson = function(string) {
    try {
        var obj = JSON.parse(string);
        return true;
    } catch (err) {
        return false;
    }
};

// If string is json, returns the json object accociated with that string.
// Otherwise, returns null.
var getJson = function(string) {
    try {
        var obj = JSON.parse(string);
        return obj;
    } catch (err) {
        return null;
    }
};
