/* global exports */
"use strict";

// module Helpers.Audio

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

exports.loopSound = function(sound) {
  return function() {
    sound.load();
    sound.loop = true;
    sound.play();
  };
};

exports.stopSound = function(sound) {
  return function() {
    sound.pause();
  };
};
