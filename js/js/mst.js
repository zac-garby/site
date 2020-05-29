---
out: mst/main.js
---
var pointRadius = 3

var points = []
var canvas = document.getElementById('canvas')
var ctx = canvas.getContext('2d')

/* INPUT */

canvas.addEventListener('mousedown', click)

function randomPoints() {
    for (var i = 0; i < 20; i++) {
        points.push({
            x: Math.random() * 512,
            y: Math.random() * 512,
        })
    }

    render()
}

function click(evt) {
    points.push({
        x: evt.offsetX,
        y: evt.offsetY,
    })

    render()
}


/* PROCESSING */

function makeGraph(points) {
    var graph = []

    for (var f = 0; f < points.length; f++) {
        var row = []

        for (var t = 0; t < points.length; t++) {
            if (t === f) {
                row.push(null)
                continue
            }

            var from = points[f]
            var to = points[t]
            var dx = to.x - from.x
            var dy = to.y - from.y

            row.push(Math.sqrt(dx*dx + dy*dy))
        }

        graph.push(row)
    }

    return graph
}

function findMST(graph) {
    // Contains the indices of the labelled columns
    var labelled = [0]

    // Contains the indices of the deleted rows
    var deleted = []

    // Output is the resultant tree, in a adjacency matrix form
    var output = []

    for (var i = 0; i < graph.length; i++) {
        var row = []

        for (var j = 0; j < graph.length; j++) {
            row.push(false)
        }

        output.push(row)
    }

    while (deleted.length < graph.length) {
        var min = findMinimum(graph, labelled, deleted)

        // Set the edge to true
        output[min.from][min.to] = true

        // Delete the edge's row
        deleted.push(min.from)

        // Label the edge's column
        labelled.push(min.from)
    }

    return output
}

// Returns the minimum possible edge in {from: index, to: index} form
function findMinimum(graph, labelled, deleted) {
    var minEdge = null
    var minDistance = Infinity

    for (var f = 0; f < graph.length; f++) {
        if (deleted.indexOf(f) >= 0) {
            continue
        }

        for (var t = 0; t < graph.length; t++) {
            if (labelled.indexOf(t) < 0) {
                continue
            }

            var dist = graph[f][t]

            if (dist < minDistance) {
                minEdge = {
                    from: f,
                    to: t,
                }

                minDistance = dist
            }
        }
    }

    return minEdge
}


/* OUTPUT */

function render() {
    var graph = makeGraph(points)
    var mst = findMST(graph)

    canvas.width = canvas.width

    ctx.beginPath()
    for (var f = 0; f < graph.length; f++) {
        for (var t = 0; t < graph.length; t++) {
            if (mst[f][t]) {
                var from = points[f]
                var to = points[t]

                ctx.moveTo(from.x, from.y)
                ctx.lineTo(to.x, to.y)
            }
        }
    }
    ctx.stroke()
    ctx.closePath()

    ctx.fillStyle = 'red'
    for (var point of points) {
        ctx.beginPath()
        ctx.arc(point.x, point.y, pointRadius, 0, 2 * Math.PI)
        ctx.closePath()
        ctx.fill()
    }
}

function writeMatrix(graph) {
    var elem = document.getElementById('matrix')

    elem.innerHTML = "  distance matrix:\n\n"

    for (var r = 0; r < graph.length; r++) {
        if (graph.length === 1) {
            elem.innerHTML += "["
        } else if (r === 0) {
            elem.innerHTML += "⎡"
        } else if (r === graph.length - 1) {
            elem.innerHTML += "⎣"
        } else {
            elem.innerHTML += "⎢"
        }

        var row = graph[r]

        for (var cell of row) {
            if (cell === null) {
                elem.innerHTML += "   -"
                continue
            }

            var str = Math.round(cell).toString()

            while (str.length < 4) {
                str = " " + str
            }

            elem.innerHTML += str
        }

        if (graph.length === 1) {
            elem.innerHTML += "]"
        } else if (r === 0) {
            elem.innerHTML += "⎤"
        } else if (r === graph.length - 1) {
            elem.innerHTML += "⎦"
        } else {
            elem.innerHTML += "⎥"
        }

        elem.innerHTML += "\n"
    }
}
