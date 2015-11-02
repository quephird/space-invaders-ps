/* global exports */
"use strict";

// module Audio

exports.loadSound = function(url) {
  return function() {
    var newSound = new Audio(url);
    return newSound;
  };
};

exports.playSound = function(sound) {
  return function() {
    sound.load();
    sound.play();
  };
};
