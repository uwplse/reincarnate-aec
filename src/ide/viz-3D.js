// This is a comment.

var scene;
var renderer;
var controls;
var camera;
var points;
var lines;

var groups;
var pointer;
var clock;

var velocity;
var raycaster;
var intersection;

var controlsEnabled;
var moveForward;
var moveBackward;
var moveLeft;
var moveRight;
var moveUp;
var moveDown;

var render = function() {
    if (currentDim == 3 && canvas) {
        requestAnimationFrame(render);

        pointer.position.copy(controls.getObject().position.clone());
        pointer.position.addScaledVector(controls.getDirection(), 5.0);

        if (controlsEnabled) {
            var delta = clock.getDelta();

            velocity.x -= velocity.x * 10.0 * delta;
            velocity.y -= velocity.y * 10.0 * delta;
            velocity.z -= velocity.z * 10.0 * delta;

            if ( moveForward ) velocity.z += 25.0 * delta;
            if ( moveBackward ) velocity.z -= 25.0 * delta;
            if ( moveLeft ) velocity.x -= 25.0 * delta;
            if ( moveRight ) velocity.x += 25.0 * delta;
            if ( moveUp ) velocity.y += 25.0 * delta;
            if ( moveDown ) velocity.y -= 25.0 * delta;


            var xdir = controls.getXDirection();
            var ydir = controls.getYDirection();
            var zdir = controls.getDirection();

            controls.getObject().position.addScaledVector(xdir, velocity.x * delta);
            controls.getObject().position.addScaledVector(ydir, velocity.y * delta);
            controls.getObject().position.addScaledVector(zdir, velocity.z * delta);
        }


        renderer.render(canvas, camera);
    }
}

var initGroup = function(nm) {
    if (!groups.hasOwnProperty(nm)) {
        console.log("init", nm);

        var g = {};

        g.points = new THREE.Group();
        g.points.visible = true;
        points.add(g.points);

        g.lines = new THREE.Group();
        lines.add(g.lines);
        g.lines.visible = true;

        g.faces = new THREE.Geometry();
        var material = new THREE.MeshBasicMaterial( {color: 0x666666} );
        var mesh = new THREE.Mesh(g.faces, material);
        scene.add(mesh);


        groups[nm] = g;
    }
}

var getGroup = function (nm) {
    initGroup(nm);
    return groups[nm];
}

var setGroupVisible = function(nm, v) {
    if (!groups.hasOwnProperty(nm)) { console.log("bad group", nm); return; }

    groups[nm].points.visible = v;
    groups[nm].lines.visible = v;
    groups[nm].faces.visible = v;
}

var clearGroupVisibility = function() {
    for (var nm in groups) {
        if (!groups.hasOwnProperty(nm)) { continue; }
        setGroupVisible(nm, false);
    }
}

var switchToGroup = function(nm) {
    clearGroupVisibility();
    setGroupVisible(nm, true);
}

var addLine = function(nm, a, b, m) {
    if (m === undefined) {
        m = new THREE.LineBasicMaterial({ color: 0x000000 });
    }

    var g = new THREE.Geometry();
    g.vertices.push(a.clone());
    g.vertices.push(b.clone());

    var line = new THREE.Line(g, m);
    line._start = a.clone();
    line._end = b.clone();

    getGroup(nm).lines.add(line);
}

var sphere = function(r, center, m) {
    if (m === undefined) {
        m = new THREE.MeshBasicMaterial( {color: 0x000000} );
    }

    var g = new THREE.SphereGeometry(r, 8, 8);
    g.translate(center.x, center.y, center.z);

    var mesh = new THREE.Mesh(g, m);

    mesh._center = center.clone();

    return mesh;
}

var addPoint = function(nm, a, m) {
    getGroup(nm).points.add(sphere(0.005, a, m));
}

var addFace = function(nm, p1, p2, p3) {
    var geom = getGroup(nm).faces;

    var n = geom.vertices.length;

    geom.vertices.push(p1, p2, p3);
    geom.faces.push(new THREE.Face3(n, n+1, n+2));
    geom.verticesNeedUpdate = true;
    geom.elementsNeedUpdate = true;
}

var initializePointerLock = function() {
    var blocker = document.getElementById( 'blocker' );
    var instructions = document.getElementById( 'instructions' );
    var havePointerLock =
        'pointerLockElement' in document ||
        'mozPointerLockElement' in document ||
        'webkitPointerLockElement' in document;
    if ( havePointerLock ) {
        var element = document.body;
        var pointerlockchange = function ( event ) {
            if ( document.pointerLockElement === element ||
                 document.mozPointerLockElement === element ||
                 document.webkitPointerLockElement === element ) {
                controlsEnabled = true;
                controls.enabled = true;
                blocker.style.display = 'none';
            } else {
                controlsEnabled = false;
                controls.enabled = false;
                blocker.style.display = '';
                instructions.style.display = '';
    	    }
        };
        var pointerlockerror = function ( event ) {
            instructions.style.display = '';
        };
        // Hook pointer lock state change events
        document.addEventListener( 'pointerlockchange', pointerlockchange, false );
        document.addEventListener( 'mozpointerlockchange', pointerlockchange, false );
        document.addEventListener( 'webkitpointerlockchange', pointerlockchange, false );
        document.addEventListener( 'pointerlockerror', pointerlockerror, false );
        document.addEventListener( 'mozpointerlockerror', pointerlockerror, false );
        document.addEventListener( 'webkitpointerlockerror', pointerlockerror, false );
        $("#visualizer").get(0).addEventListener( 'click', function ( event ) {
            instructions.style.display = 'none';
            // Ask the browser to lock the pointer
            element.requestPointerLock =
                element.requestPointerLock ||
                element.mozRequestPointerLock ||
                element.webkitRequestPointerLock;
    	element.requestPointerLock();
        }, false );
    } else {
        instructions.innerHTML = 'Your browser doesn\'t seem to support Pointer Lock API';
    }
}

var onKeyDown = function ( event ) {
    switch ( event.keyCode ) {
    case 38: // up
    case 87: // w
	moveForward = true;
	break;
    case 37: // left
    case 65: // a
	moveLeft = true; break;
    case 40: // down
    case 83: // s
	moveBackward = true;
	break;
    case 39: // right
    case 68: // d
	moveRight = true;
        break;

    case 80: // p
        pointer.visible = !pointer.visible;
        break;

    case 81: // q
        moveUp = true;
        break;

    case 90: // z
        moveDown = true;
        break;

    case 49: // 1
        // switchToGroup("foo");
        break;

    case 50: // 2
        // switchToGroup("bar");
        break;

    case 51: // 3
        //setGroupVisible("foo", true);
        //setGroupVisible("bar", true);
        break;

    }
};
var onKeyUp = function ( event ) {
    switch( event.keyCode ) {
    case 38: // up
    case 87: // w
	moveForward = false;
	break;
    case 37: // left
    case 65: // a
	moveLeft = false;
	break;
    case 40: // down
    case 83: // s
	moveBackward = false;
	break;
    case 39: // right
    case 68: // d
	moveRight = false;
        break;

    case 81: // q
        moveUp = false;
        break;
    case 90: // z
        moveDown = false;
        break;


    }
};

var onClick = function(event) {
    if (controlsEnabled) {
        raycaster.set(controls.getObject().position, controls.getDirection());
        var intersects = raycaster.intersectObject( points, true);
        if (intersects.length > 0) {
            intersection = intersects[0].object;
            console.log("point", intersection._center);
        } else {
            intersects = raycaster.intersectObject( lines, true);
            if (intersects.length > 0) {
                intersection = intersects[0].object;
                console.log("line", intersection._start, intersection._end);
            } else {
                intersection = null;
            }
        }
    }
};

var getVizSize = function() {
    var main = $('#mesh .main').get(0);

    return { width: main.clientWidth,
             height:main.clientHeight };
};

var onWindowResize = function(){
    var rde = renderer.domElement;
    rde.parentElement.removeChild(rde);

    var size = getVizSize();
    var width = size.width;
    var height = size.height;

    camera.aspect = width / height;
    camera.updateProjectionMatrix();

    renderer.setSize(width, height);

    $("#visualizer").append(renderer.domElement);
}

var initializeCanvas3D = function() {

    var size = getVizSize();
    var width = size.width;
    var height = size.height;

    // For 3D, we pretend the scene is the "canvas"
    canvas = new THREE.Scene();
    canvas.background = new THREE.Color(0xeeeeee)
    scene = canvas;
    camera = new THREE.PerspectiveCamera(60,
                 width / height, 0.01, 1000);

    renderer = new THREE.WebGLRenderer();

    points = new THREE.Group();
    scene.add(points);

    lines = new THREE.Group();
    scene.add(lines);

    groups = {};

    controls = new THREE.PointerLockControls( camera );
    scene.add( controls.getObject() );

    pointer = sphere(0.1, new THREE.Vector3(0,0,0));
    scene.add(pointer);
    pointer.visible = false;

    document.addEventListener( 'keydown', onKeyDown, false );
    document.addEventListener( 'keyup', onKeyUp, false );

    window.addEventListener( 'resize', onWindowResize, false );

    clock = new THREE.Clock();

    controls.getObject().position.y = 0;
    controls.getObject().position.z = 5;

    velocity = new THREE.Vector3(0,0,0);

    raycaster = new THREE.Raycaster();
    raycaster.linePrecision = 0.02;

    intersection = null;

    controlsEnabled = false;
    moveForward = false;
    moveBackward = false;
    moveLeft = false;
    moveRight = false;
    moveUp = false;
    moveDown = false;

    renderer.domElement.addEventListener('click', onClick, false);

    initializePointerLock();

    renderer.setSize(width, height, true);
    $("#visualizer").append(renderer.domElement);
    $('canvas').attr('height', height).attr('width', width)
               .css({'width': width, 'height': height});
    render();
};

var clear3D = function() {
    $('canvas').remove();

    renderer = undefined;
    camera = undefined;
    canvas = undefined;
    scene = undefined;
    controls = undefined;

    points = undefined;
    lines = undefined;

    groups = undefined;
    pointer = undefined;
    clock = undefined;

    velocity = undefined;
    raycaster = undefined;
    intersection = undefined;

    controlsEnabled = undefined;
    moveForward = undefined;
    moveBackward = undefined;
    moveLeft = undefined;
    moveRight = undefined;
    moveUp = undefined;
    moveDown = undefined;
};

var drawAxes3D = function(faces, viewbox, domainViewbox) { };

var generateViewbox3D = function() { }

var generateViewboxDomain3D = function(faces) { };

var drawFaces3D = function(faces, viewbox, viewboxDomain) {
    var the_mesh = editors['mesh'].getValue();

    for(var i = 0; i < faces.length; i++) {
        var face = faces[i];
        var p1 = new THREE.Vector3(face.x1, face.y1, face.z1);
        var p2 = new THREE.Vector3(face.x2, face.y2, face.z2);
        var p3 = new THREE.Vector3(face.x3, face.y3, face.z3);

        addFace("foo", p1, p2, p3);

        addPoint("the-group", p1);
        addPoint("the-group", p2);
        addPoint("the-group", p3);
        addLine("the-group", p1, p2);
        addLine("the-group", p2, p3);
        addLine("the-group", p3, p1);
    }
};

var transformFace3D = function (viewboxDomain, face, viewbox) {
    throw new Error("transformFace3D is not implemented")
};

var toObject3D = function(ideOut) {
    throw new Error("3D Object not implemented")
};

var visualize3D = function() {
    throw new Error("visualize3D is not implemented")
};
