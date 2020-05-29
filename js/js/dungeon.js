---
out: dungeon/main.js
---
var roadWidth = 4
var roomRadius = 8
var gridSpacing = 32
var roomRadiusVariance = 4 // either way
var nodeChance = 0.5
var roomProbCoefficient = -1.1

var points = []
var canvas = document.getElementById('canvas')
var ctx = canvas.getContext('2d')

/* INPUT */

canvas.addEventListener('mousedown', click)

function randomGrid() {
    parseOptions()

    points = []

    for (var i = gridSpacing; i < 512; i += gridSpacing) {
        for (var j = gridSpacing; j < 512; j += gridSpacing) {
            if (Math.random() < nodeChance) {
                points.push({
                    x: i,
                    y: j,
                })
            }
        }
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

function parseOptions() {
    var form = document.getElementById('options')
    
    var options = [
        'roadWidth',
        'roomRadius',
        'gridSpacing',
        'roomRadiusVariance',
        'nodeChance',
        'roomProbCoefficient',
    ]

    for (var key of options) {
        var val = parseFloat(form[key].value)
        window[key] = val
    }
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
        output[min.to][min.from] = true

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

// Returns the number of edges connected to a node in an adjacency matrix
function numConnected(graph, index) {
    var row = graph[index]
    var total = 0

    for (var c = 0; c < graph.length; c++) {
        if (c === index) {
            continue
        }

        if (row[c]) {
            total++
        }
    }

    return total
}

// Returns the probability that a room should appear, based on the number of edges
// incident to a node
function roomProbability(n) {
    return Math.exp(roomProbCoefficient * n)
}


/* OUTPUT */

function render() {
    parseOptions()

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
    ctx.lineWidth = roadWidth
    ctx.lineCap = 'round'
    ctx.stroke()
    ctx.closePath()

    for (var p = 0; p < points.length; p++) {
        var point = points[p]
        var conn = numConnected(mst, p)

        if (Math.random() <= roomProbability(conn - 1)) {
            var radius = roomRadius + (Math.random() - 0.5) * roomRadiusVariance
            ctx.fillStyle = 'red'
            ctx.fillRect(point.x - radius, point.y - radius, radius*2, radius*2)
        }
    }
}

function writeMatrix(graph) {
    var elem = document.getElementById('matrix')

    elem.innerHTML = "  MST adjacency matrix:\n\n"

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
                elem.innerHTML += " -"
                continue
            }

            var str = Math.round(cell).toString()

            while (str.length < 2) {
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
