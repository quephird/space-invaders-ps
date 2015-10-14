/* global exports */
"use strict";

// module Graphics.Canvas.Image

exports.makeCanvasImageSource = function(url) {
  return function() {
    var newImage;
    newImage = new Image();
    newImage.src = url;
    return newImage;
  };
};
