/* global exports */
"use strict";

// module Graphics.Canvas.Image

exports.makeCanvasImageSource = function(url) {
  return function() {
    var newImage = new Image();
    newImage.src = url;
    return newImage;
  };
};

exports.getWidth = function(img) {
  return img.width;
}

exports.getHeight = function(img) {
  return img.height;
}
