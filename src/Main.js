"use strict";

exports.requestAnimationFrame = function(f) {
    return function() {
        window.requestAnimationFrame(function (dt) {
            f(dt)();
        });
    }
};

exports.onKeyDown = function(f) {
    return function() {
        document.addEventListener ("keydown", function(event) {
            f(event.which)();
        });
    }
};