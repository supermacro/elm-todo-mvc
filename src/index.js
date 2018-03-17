'use strict';

require('sanitize.css');
require('./init.css');

var Elm = require('./Main.elm');

var mountNode = document.getElementById('app');

// .embed() can take an optional second argument.
// This would be an object describing the data we need to start a program
// i.e. a userID or some token
var app = Elm.Main.embed(mountNode);
