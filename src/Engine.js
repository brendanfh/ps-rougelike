"use strict";

exports.runGame = function(canvasName) {
    return function(events) {
        return function (game) {
            return function () {
                var canvas = document.getElementById(canvasName);
                if (canvas == null) {
                    throw "Failed to get canvas: " + canvasName;
                }
                var ctx = canvas.getContext("2d");

                var g = {
                    state : game.init(),
                    events : []
                };
                
                for (var i = 0; i < events.length; i++) {
                    events[i](g)();
                }

                function loop(dt) {
                    var redraw = false;
                    var i, event;
                    for (i = 0; i < g.events.length; i++) {
                        event = g.events[i]; 
                        g.state = game.update(event)(g.state)();
                        redraw = true;
                    }
                    g.events.length = 0;

                    if (redraw) {
                        game.draw(ctx)(g.state)();
                    }

                    window.requestAnimationFrame(loop);
                }
                game.draw(ctx)(g.state)();
                window.requestAnimationFrame(loop);

                return g; 
            }
        }
    }
};

exports.addGameEvent = function (event) {
    return function (gm) {
        return function () {
            gm.events.push(event);
        }
    }
};

exports.getState = function (gm) {
    return function () {
        return gm.state;
    }
};

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