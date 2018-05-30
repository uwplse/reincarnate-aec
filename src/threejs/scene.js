var scene = new THREE.Scene();
scene.background = new THREE.Color(0xeeeeee);

var points = new THREE.Group();
scene.add(points);

var lines = new THREE.Group();
scene.add(lines);



var camera = new THREE.PerspectiveCamera( 60, window.innerWidth / window.innerHeight, 0.01, 1000 );

var renderer = new THREE.WebGLRenderer();
renderer.setSize( window.innerWidth , window.innerHeight );
document.body.appendChild( renderer.domElement );

var groups = {};

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
        g.mesh = new THREE.Mesh(g.faces, material);
        scene.add(g.mesh);

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
    groups[nm].mesh.visible = v;
}

var clearGroupVisibility = function() {
    for (var nm in groups) {
        if (!groups.hasOwnProperty(nm)) { continue; }
        setGroupVisible(nm, false);
    }
}

var allGroupsVisible = function() {
    for (var nm in groups) {
        if (!groups.hasOwnProperty(nm)) { continue; }
        setGroupVisible(nm, true);
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

var addFace = function(nm, p1, p2, p3) {
    var geom = getGroup(nm).faces;

    var n = geom.vertices.length;

    geom.vertices.push(p1, p2, p3);
    geom.faces.push(new THREE.Face3(n, n+1, n+2));
    geom.verticesNeedUpdate = true;
    geom.elementsNeedUpdate = true;
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
    getGroup(nm).points.add(sphere(0.0025, a, m));
}

var controlsEnabled = false;
var moveForward = false;
var moveBackward = false;
var moveLeft = false;
var moveRight = false;
var moveUp = false;
var moveDown = false;


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
	    blocker.style.display = '-webkit-box';
	    blocker.style.display = '-moz-box';
	    blocker.style.display = 'box';
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
    instructions.addEventListener( 'click', function ( event ) {
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

var controls = new THREE.PointerLockControls( camera );
scene.add( controls.getObject() );

var pointer = sphere(0.1, new THREE.Vector3(0,0,0));
scene.add(pointer);
pointer.visible = false;

var group_names;

var onKeyDown = function ( event ) {
    var k = event.keyCode;

    switch ( k ) {
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

    default:
        if (49 <= k && k <= 48 + 9) {
            var i = k - 49;
            if (0 <= i && i < group_names.length) {
                switchToGroup(group_names[i]);
            }
        }
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

document.addEventListener( 'keydown', onKeyDown, false );
document.addEventListener( 'keyup', onKeyUp, false );

var clock = new THREE.Clock();

controls.getObject().position.y = 0;
controls.getObject().position.z = 5;

//controls.getObject().lookAt(new THREE.Vector3(0,0,0));

var velocity = new THREE.Vector3(0,0,0);

var raycaster = new THREE.Raycaster();
raycaster.linePrecision = 0.02;

var intersection = null;

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

document.addEventListener('click', onClick, false);

var animate = function() {
    requestAnimationFrame( animate );

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

    renderer.render( scene, camera );
}
animate();


function loadScene(name) {
    var newScript = document.createElement("script");
    newScript.src = "../" + name + ".js";
    document.documentElement.appendChild(newScript);
}

// group_names = ["cube0", "cube18", "cube36", "cube54"];

// group_names = ["cube0-18A", "cube0-18B", "cube0-18", "cube0", "cube18"];

// group_names = ["cube0-18-36.mpfr1024"];
group_names = ["tri"];

for (var i = 0; i < group_names.length; i++) {
    loadScene(group_names[i]);
}
