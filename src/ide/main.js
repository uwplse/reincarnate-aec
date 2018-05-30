// sucks to lose unsaved changes
window.onbeforeunload = function() {
  return 'Are you sure you want to leave?';
}

function elem(id) {
  return document.getElementById(id);
}

function elems(klass) {
  return document.getElementsByClassName(klass);
}

var views = ['lc', 'cad', 'mesh', 'slicer', 'tp', 'gcode', 'log'];
var editors = {};
var __log = [];

function ide_log(msg) {
  __log.push(msg);
}

function reset() {
  for(var i = 0; i < views.length; i++) {
    var v = views[i];
    elem(v).style.display = 'none';
    elem('nav-' + v).classList.remove('cur');
  }
}

function viz() {
  var m = editors['mesh'].getValue();
  if(empty_mesh(m)) {
    console.log('view: empty mesh');
  } else {
    var pm = parse_mesh(m);
    visualize(pm);
  }
}

function view(id) {
  reset();

  // update navbar and content
  elem('nav-' + id).classList.add('cur');
  elem(id).style.display = 'block';

  // update editor
  if(id === 'log') {
    editors[id].setValue(__log.join(''));
  } else {
    editors[id].refresh();
  }

  if(id === 'mesh') {
    viz();
  }
  if(id === 'log') {
    elem('rand-seed').value = get_rand_seed('');
    elem('disp-prec').value = get_disp_prec('');
    elem('tag-filter').value = get_tag_filter('');
  }
}

function init() {
  for(var i = 0; i < views.length; i++) {
    var v = views[i];
    editors[v] =
      CodeMirror.fromTextArea(elem(v + '-code'),
        { matchBrackets : true
        , lineNumbers   : true
        });

    // tab should indent by 2 spaces ;)
    editors[v].setOption('extraKeys', {
      Tab: function(cm) {
        var spaces = Array(cm.getOption('indentUnit') + 1).join(' ');
        cm.replaceSelection(spaces);
      }
    });
  }

  editors['lc'].setOption('mode', 'text/x-ocaml');
  editors['log'].setOption('readOnly', true);

  view('lc');
}

function upload_aux(id) {
  elem(id + '-upload').click();
}

function upload(id) {
  var file = elem(id + '-upload').files[0];
  var reader = new FileReader();
  reader.onload = function(e) {
    editors[id].setValue(reader.result);
    elem(id + '-download').value = file.name;
    view(id);
  }
  reader.readAsText(file);
}

function download(id) {
  var text = editors[id].getValue();
  var fname = elem(id + '-download').value;
  var blob  = new Blob([text], {type: 'text/plain;charset=utf-8'});
  saveAs(blob, fname);
}

function compile(src_id) {
  var src = editors[src_id].getValue();

  switch(src_id) {
    case 'lc':
      compile_lc(src);
      return;
    case 'cad':
      compile_cad(src);
      return;
    case 'mesh':
      compile_mesh(src);
      return;
    case 'slicer':
      compile_slicer(src);
      return;
    case 'tp':
      compile_tp(src);
      return;
    default:
      alert('Bogus src_id ' + src_id);
  }
}

function synth(src_id) {
  var src = editors[src_id].getValue();

  switch(src_id) {
    case 'cad':
      synth_lc(src);
      return;
    case 'mesh':
      synth_cad(src);
      return;
    case 'tp':
      synth_mesh(src);
      return;
    case 'gcode':
      synth_tp(src);
    default:
      alert('Bogus src_id ' + src_id);
  }
}

function empty_mesh(m) {
  if(m) {
    /^\s*$/.test(m)
  } else {
    return true;
  }
}

function parse_mesh(m) {
  function face_coords(s) {
    return s.replace(/[\[\]\n]/g, '')
            .split(';')
            .map(function(f) {
              return f.split(',')
                      .map(function(x) {
                        return x.replace(/[\(\)]/g, '')
                      });
            });
  }
  function valid_1D(faces) {
    for(var i = 0; i < faces.length; i++) {
      if(!($.isNumeric(faces[i]))) {
        throw new Error('invalid mesh1');
      }
    }
    return { 'dim'   : 1
           , 'faces' : faces
           };
  }
  function pm1(m) {
    var faces =
      face_coords(m).map(function(coords) {
        return parseFloat(coords[0])
      });
    return valid_1D(faces);
  }
  function pm1_json(m) {
    var mo = JSON.parse(m);
    return valid_1D(mo.faces);
  }
  function valid_2D(faces) {
    for(var i = 0; i < faces.length; i++) {
      if(!($.isNumeric(faces[i].x1) &&
           $.isNumeric(faces[i].y1) &&
           $.isNumeric(faces[i].x2) &&
           $.isNumeric(faces[i].y2))) {
        throw new Error('invalid mesh2');
      }
    }
    return { 'dim'   : 2
           , 'faces' : faces
           };
  }
  function pm2(m) {
    var faces =
      face_coords(m).map(function(coords) {
        return { "x1": parseFloat(coords[0])
               , "y1": parseFloat(coords[1])
               , "x2": parseFloat(coords[2])
               , "y2": parseFloat(coords[3])
               };
      });
    return valid_2D(faces);
  }
  function pm2_json(m) {
    var mo = JSON.parse(m);
    return valid_2D(mo.faces);
  }
  function valid_3D(faces) {
    for(var i = 0; i < faces.length; i++) {
      if(!($.isNumeric(faces[i].x1) &&
           $.isNumeric(faces[i].y1) &&
           $.isNumeric(faces[i].z1) &&
           $.isNumeric(faces[i].x2) &&
           $.isNumeric(faces[i].y2) &&
           $.isNumeric(faces[i].z2) &&
           $.isNumeric(faces[i].x3) &&
           $.isNumeric(faces[i].y3) &&
           $.isNumeric(faces[i].z3))) {
        throw new Error('invalid mesh3');
      }
    }
    return { 'dim'   : 3
           , 'faces' : faces
           };
  }
  function pm3(m) {
    var faces =
      face_coords(m).map(function(coords) {
        return { "x1": parseFloat(coords[0])
               , "y1": parseFloat(coords[1])
               , "z1": parseFloat(coords[2])
               , "x2": parseFloat(coords[3])
               , "y2": parseFloat(coords[4])
               , "z2": parseFloat(coords[5])
               , "x3": parseFloat(coords[6])
               , "y3": parseFloat(coords[7])
               , "z3": parseFloat(coords[8])
               };
      });
    return valid_3D(faces);
  }
  function pm3_json(m) {
    var mo = JSON.parse(m);
    return valid_3D(mo.faces);
  }
  function pm3_stl(m) {
    // TODO thread this back into viz to avoid reparsing
    var loader = new THREE.STLLoader();
    var stl = loader.parse(m);
    return { 'dim'   : 3
           , 'faces' : m
           };
  }

  // work from 3D down since the code
  // above will accept a (k + 1)D mesh
  // as a valid kD mesh
  try {
    return pm3_stl(m);
  } catch(e) { try {
    return pm3(m);
  } catch(e) { try {
    return pm3_json(m);
  } catch(e) { try {
    return pm2(m);
  } catch(e) { try {
    return pm2_json(m);
  } catch(e) { try {
    return pm1(m);
  } catch(e) { try {
    return pm1_json(m);
  } catch(e) {
    throw new Error('unrecognized mesh:\n' + m);
  }}}}}}}
}

/*
**
** from viz main
**
*/

// d3 svg object to hold number line
var canvas;

// Current dimension of the drawn canvas
var currentDim;

function visualize(parsed_mesh) {
  if(currentDim != undefined) {
      clear(currentDim);
  }
  currentDim = parsed_mesh.dim;
  faces = parsed_mesh.faces;

  if (currentDim == 3) {
    elem("3d-blocker").style.display = "block";
  } else {
    elem("3d-blocker").style.display = "none";
  }

  // Initialize the canvas
  initializeCanvas(currentDim);
  // Generate the viewboxes
  var viewbox = generateViewbox(currentDim);
  var viewboxDomain = generateViewboxDomain(currentDim, faces);
  // Render the axes
  drawAxes(currentDim, faces, viewbox, viewboxDomain);
  // Draw faces into the number line
  drawFaces(currentDim, faces, viewbox, viewboxDomain);
}

function edit_mode() {
  var mode = elem('edit-mode').value;

  for(var i = 0; i < views.length; i++) {
    var e = editors[views[i]];
    e.setOption('keyMap', mode);
  }
}

function log_tag(id) {
  if(elem(id + '-tag').checked) {
    user_tags_add(id);
  } else {
    user_tags_rem(id);
  }
}

function updt_rand_seed() {
  var s = elem('rand-seed').value;
  try {
    set_rand_seed(s);
  } catch(e) {
    elem('rand-seed').value = get_rand_seed('');
  }
}

function updt_disp_prec() {
  var s = elem('disp-prec').value;
  var i = parseInt(s);
  if(0 <= i && i <= 20) {
    set_disp_prec(s);
  } else {
    alert('Display precision must be between 0 and 20.');
    elem('disp-prec').value = get_disp_prec('');
  }
}

function updt_tag_filter() {
  var s = elem('tag-filter').value;
  try {
    set_tag_filter(s);
  } catch(e) {
    elem('tag-filter').value = get_tag_filter('');
  }
}

function reset_log() {
  __log = [];
  editors['log'].setValue('');
  editors['log'].refresh();
}
