---
out: gkbd/main.js
---
if (navigator.mediaDevices === undefined) {
    navigator.mediaDevices = {}
}

if (navigator.mediaDevices.getUserMedia === undefined) {
    navigator.mediaDevices.getUserMedia = function(constraints) {
        var getUserMedia = navigator.webkitGetUserMedia || navigator.mozGetUserMedia || navigator.msGetUserMedia

        if (!getUserMedia) {
            return Promise.reject(new Error('getUserMedia is not implemented in this browser'))
        }

        return new Promise(function(resolve, reject) {
            getUserMedia.call(navigator, constraints, resolve, reject)
        })
    }
}

var ctx = new (window.AudioContext || window.webkitAudioContext)()
var note = document.getElementById('note')
var out = document.getElementById('out')
out.value = ""
var source, stream
var noteBuf = []
var noteBufN = 16
var notes = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
var canWrite = true

var analyser = ctx.createAnalyser()
analyser.fftSize = 4096
var bufferSize = analyser.frequencyBinCount
var dataArray = new Float32Array(bufferSize)
var working = false
analyser.getFloatTimeDomainData(dataArray)

if (navigator.mediaDevices.getUserMedia) {
    console.log('getUserMedia is supported :)')
    var constraints = { audio: true }

    navigator.mediaDevices.getUserMedia(constraints)
        .then(function(stream) {
            working = true
            source = ctx.createMediaStreamSource(stream)
            source.connect(analyser)
        })
        .catch(function(err) { console.log('The following error ocurred: ' + err) })
} else {
    console.log('getUserMedia is not supported on your browser. Get Firefox or something!');
}

function render() {
    requestAnimationFrame(render)

    if (!working) { return }
    analyser.getFloatTimeDomainData(dataArray)

    var ac = autoCorrelate(dataArray, ctx.sampleRate)
    var ni = getIndex(ac)
    noteBuf.push(ni)

    if (noteBuf.length >= noteBufN) {
        /* var total = 0
        var n = 0

        for (var nt of noteBuf) {
            if (nt !== -1 && nt !== NaN) {
                total += nt
                n++
            }
        }

        if (n === 0) { return }

        var average = Math.round(total / n) */
        var average = mode(noteBuf)
        if (average && canWrite) {
            note.innerHTML = average // notes[average % 12]
            out.value += String.fromCharCode(average)
            canWrite = false
        } else if (!average) {
            note.value = "—"
            canWrite = true
        } else {
            note.value = "—"
        }

        noteBuf = []
    }
}

requestAnimationFrame(render)

var MIN_SAMPLES = 0;  // will be initialized when AudioContext is created.
var GOOD_ENOUGH_CORRELATION = 0.95; // this is the "bar" for how close a correlation needs to be

function autoCorrelate( buf, sampleRate ) {
	var SIZE = buf.length;
	var MAX_SAMPLES = Math.floor(SIZE/2);
	var best_offset = -1;
	var best_correlation = 0;
	var rms = 0;
	var foundGoodCorrelation = false;
	var correlations = new Array(MAX_SAMPLES);

	for (var i=0;i<SIZE;i++) {
		var val = buf[i];
		rms += val*val;
	}
	rms = Math.sqrt(rms/SIZE);
	if (rms<0.01) // not enough signal
		return -1;

	var lastCorrelation=1;
	for (var offset = MIN_SAMPLES; offset < MAX_SAMPLES; offset++) {
		var correlation = 0;

		for (var i=0; i<MAX_SAMPLES; i++) {
			correlation += Math.abs((buf[i])-(buf[i+offset]));
		}
		correlation = 1 - (correlation/MAX_SAMPLES);
		correlations[offset] = correlation; // store it, for the tweaking we need to do below.
		if ((correlation>GOOD_ENOUGH_CORRELATION) && (correlation > lastCorrelation)) {
			foundGoodCorrelation = true;
			if (correlation > best_correlation) {
				best_correlation = correlation;
				best_offset = offset;
			}
		} else if (foundGoodCorrelation) {
			// short-circuit - we found a good correlation, then a bad one, so we'd just be seeing copies from here.
			// Now we need to tweak the offset - by interpolating between the values to the left and right of the
			// best offset, and shifting it a bit.  This is complex, and HACKY in this code (happy to take PRs!) -
			// we need to do a curve fit on correlations[] around best_offset in order to better determine precise
			// (anti-aliased) offset.

			// we know best_offset >=1, 
			// since foundGoodCorrelation cannot go to true until the second pass (offset=1), and 
			// we can't drop into this clause until the following pass (else if).
			var shift = (correlations[best_offset+1] - correlations[best_offset-1])/correlations[best_offset];  
			return sampleRate/(best_offset+(8*shift));
		}
		lastCorrelation = correlation;
	}
	if (best_correlation > 0.01) {
		// console.log("f = " + sampleRate/best_offset + "Hz (rms: " + rms + " confidence: " + best_correlation + ")")
		return sampleRate/best_offset;
	}
	return -1;
}

function getIndex(ac) {
    return Math.round(12 * (Math.log(ac / 440) / Math.log(2))) + 75
}

function mode(arr) {
    var numMapping = {}
    var greatestFreq = 0
    var mode
    arr.forEach(function findMode(number) {
        numMapping[number] = (numMapping[number] || 0) + 1

        if (greatestFreq < numMapping[number]) {
            greatestFreq = numMapping[number]
            mode = number
        }
    })

    return +mode
}
