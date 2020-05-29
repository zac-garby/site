---
out: 2d/main.js
---
var fxInput = document.getElementById('fx')
var fyInput = document.getElementById('fy')
var ppuInput = document.getElementById('ppu')
var trxInput = document.getElementById('trx')
var tryInput = document.getElementById('try')
var canvas = document.getElementById('out')
var plotButton = document.getElementById('plot-btn')
var ctx = canvas.getContext('2d')

var functions = ["sin", "cos", "tan", "sinh", "cosh", "tanh", "asin", "acos", "atan", "asinh", "acosh", "atanh", "log", "log10", "sign", "pow", "PI", "E"]
var functionsActuals = functions.map(key => Math[key])

plotButton.onclick = plot

function plot() {
    console.log("plotting...")
    var fx = new Function("x", "y", ...functions, "return " + fxInput.value)
    var fy = new Function("x", "y", ...functions, "return " + fyInput.value)
    var calc = []
    var maxMagnitude = 0
    var scaleFactor = Number.parseFloat(ppuInput.value)
    var trx = Number.parseFloat(trxInput.value)
    var tray = Number.parseFloat(tryInput.value)

    for (var x = -256; x <= 256; x += 1) {
        var col = []

        for (var y = -256; y <= 256; y += 1) {
            var sx = x / scaleFactor + trx
            var sy = y / scaleFactor - tray
            var rx = fx(sx, sy, ...functionsActuals)
            var ry = fy(sx, sy, ...functionsActuals)
            var angle = (Math.atan2(ry, rx) * (180 / Math.PI)) % 360
            var magnitude = Math.sqrt(rx*rx + ry*ry)

            if (magnitude > maxMagnitude) {
                maxMagnitude = magnitude
            }

            col.push([angle, magnitude])
        }

        calc.push(col)
    }

    for (var x = 0; x < 512; x++) {
        for (var y = 0; y < 512; y++) {
            var hmm = calc[x][y]
            var angle = hmm[0] / 360
            var magnitude = (hmm[1] / maxMagnitude)
            var hsl = hsv2hsl(angle, 1, magnitude)
            var rgb = hslToRgb(hsl[0], hsl[1], hsl[2])
            var colour = `rgb(${rgb[0]}, ${rgb[1]}, ${rgb[2]})`
            ctx.fillStyle = colour
            ctx.fillRect(x, y, 1, 1)
        }
    }

    console.log("done!")
}

// https://gist.github.com/xpansive/1337890
function hsv2hsl(a,b,c) { return [a,b*c/((a=(2-b)*c)<1?a:2-a),a/2] }

// https://stackoverflow.com/questions/2353211/hsl-to-rgb-color-conversion#9493060
function hslToRgb(h, s, l){
    var r, g, b;

    if (s == 0) {
        r = g = b = l; // achromatic
    } else {
        var hue2rgb = function hue2rgb(p, q, t){
            if(t < 0) t += 1;
            if(t > 1) t -= 1;
            if(t < 1/6) return p + (q - p) * 6 * t;
            if(t < 1/2) return q;
            if(t < 2/3) return p + (q - p) * (2/3 - t) * 6;
            return p;
        }

        var q = l < 0.5 ? l * (1 + s) : l + s - l * s;
        var p = 2 * l - q;
        r = hue2rgb(p, q, h + 1/3);
        g = hue2rgb(p, q, h);
        b = hue2rgb(p, q, h - 1/3);
    }

    return [Math.round(r * 255), Math.round(g * 255), Math.round(b * 255)];
}
