---
out: boggle/main.js
---
var letterFrequencies = [
    ['e', 0.12702],
    ['t', 0.09056],
    ['a', 0.08167],
    ['o', 0.07507],
    ['i', 0.06966],
    ['n', 0.06749],
    ['s', 0.06327],
    ['h', 0.06094],
    ['r', 0.05987],
    ['d', 0.04253],
    ['l', 0.04025],
    ['c', 0.02782],
    ['u', 0.02758],
    ['m', 0.02406],
    ['w', 0.02360],
    ['f', 0.02228],
    ['g', 0.02015],
    ['y', 0.01974],
    ['p', 0.01929],
    ['b', 0.01492],
    ['v', 0.00978],
    ['k', 0.00772],
    ['j', 0.00153],
    ['x', 0.00150],
    ['qu', 0.00095],
    ['z', 0.00074],
]

var wordRegex = /[a-z]{3,}/
var board = []
var words = []
var secondsRemaining = 120
var playing = true

letterFrequencies.reverse()

function chooseLetter() {
    var random = Math.random()

    for (var pair of letterFrequencies) {
        var prob = pair[1]
        if (random < prob) {
            return pair[0]
        }
        random -= prob
    }
}

function loadBoard() {
    board = []

    for (var y = 0; y < 4; y++) {
        var row = []
        for (var x = 0; x < 4; x++) {
            var letter = chooseLetter()
            row.push(letter)
            letter = letter[0].toUpperCase() + letter.substring(1)
            setCell(x, y, letter)
        }
        board.push(row)
    }
}

function setCell(x, y, str) {
    var cell = document.getElementById(`cell-${x}${y}`)
    cell.innerHTML = str
}

function validateWord(word) {
    return wordRegex.test(word) && wordList.indexOf(word) >= 0 && findWordAnywhere(word)
}

function findWordAnywhere(word) {
    for (var y = 0; y < 4; y++) {
        for (var x = 0; x < 4; x++) {
            if (findWordFrom(x, y, word, [])) {
                return true
            }
        }
    }

    return false
}

// Attempts to find a word starting from a given coordinate. Won't use previous coordinates
function findWordFrom(x, y, word, prev) {
    if (word.length === 0) { return true }

    var first = word[0]
    if (word.length > 1 && word[0] == "q" && word[1] == "u") {
        first = "qu"
    }

    if (board[y][x] !== first) { return false }

    prev.push([x, y])

    var neighbours =
        [[x-1, y-1], [x, y-1], [x+1, y-1], [x-1, y], [x+1, y], [x-1, y+1], [x, y+1], [x+1, y+1]]
        .filter(n => n[0] >= 0 && n[0] < 4 && n[1] >= 0 && n[1] < 4)
        .filter(n => !containsCoord(prev, n))
        .filter(n => findWordFrom(n[0], n[1], word.slice(first.length), prev))

    return neighbours.length > 0
}

function allPossibleWords() {
    return wordList.filter(findWordAnywhere)
}

function containsCoord(list, coord) {
    for (const c of list) {
        if (c[0] == coord[0] && c[1] == coord[1]) {
            return true
        }
    }

    return false
}

function enterWord(word) {
    if (!playing) { return false }

    word = word.toLowerCase()
    if (!validateWord(word)) { return false }

    words.push(word)
    document.getElementById('words').innerHTML += `<li id="word-${word}">${word}</li>`

    return true
}

function tick() {
    if (secondsRemaining === 0) {
        window.clearInterval(timer)
        finish()
        return
    }

    var mins = Math.floor(secondsRemaining / 60).toString()
    if (mins.length == 1) { mins = "0" + mins }

    var secs = (secondsRemaining % 60).toString()
    if (secs.length == 1) { secs = "0" + secs }

    document.getElementById('time').innerHTML = `${mins}:${secs}`
    secondsRemaining -= 1
}

function finish() {
    playing = false
    
    var possible = allPossibleWords()
    possible.sort()

    document.getElementById('results').style.display = 'block'
    document.getElementById('word-input').style.display = 'none'
    document.getElementById('time').innerHTML = 'Your words:'
    document.getElementById('n-words').innerHTML = words.length
    document.getElementById('p-words').innerHTML = possible.length
    document.getElementById('percentage').innerHTML = Math.ceil((words.length / possible.length) * 100)

    var ul = document.getElementById('all-words')
    for (const word of possible) {
        var cssClass = (words.indexOf(word) === -1) ? "didnt-get" : "did-get"
        ul.innerHTML += `<li class=${cssClass}>${word}</li>`
    }
}

var timer = window.setInterval(tick, 1000)

loadBoard()

document.getElementById('word-input').addEventListener('keypress', evt => {
    if (evt.key == "Enter") {
        enterWord(evt.target.value)
        evt.target.value = ''
    }
})
