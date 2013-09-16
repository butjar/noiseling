var	rec;
var	mediaStreamSource;
var	audioContext;
var	canvas;
var	video;
var	ctx;
var	ws;
var audio;
var recording = false;
var serverAddress = "141.64.162.226:8080";

$(document).ready(function() {
	canvas = $("#canvas");
	ctx = canvas.get()[0].getContext('2d');
	ws = openWebsockets(init());
	setInterval(function(){
		if(recording){
			captureAudio();
		};
	}, 1);
});

var init = function() {
	window.URL = window.URL || window.webkitURL;
	navigator.getUserMedia  = navigator.getUserMedia || navigator.webkitGetUserMedia ||
								navigator.mozGetUserMedia || navigator.msGetUserMedia;
	window.AudioContext = window.AudioContext || window.webkitAudioContext;
	audioContext = new AudioContext();
	startWebRtcSession();
};
				
var openWebsockets = function(callback){
	var WebSockets = new WebSocket("ws://" + serverAddress + "/stream");
    WebSockets.onopen = function() { callback };
	return WebSockets;
};

var startWebRtcSession = function(){
  	navigator.getUserMedia({video: false, audio: true}, function(localMediaStream) {
    	mediaStreamSource = audioContext.createMediaStreamSource(localMediaStream);
    	rec = new Recorder(mediaStreamSource);
    	rec.record();
    	recording = true;
	}, onGetUserMediaFailed);
};
	            

var onGetUserMediaFailed = function(e) {
	alert("Your Browser doesn't support WebRTC");
};

var captureAudio = function(){
	rec.exportWAV(function(blob) {
   	 	ws.send(blob);
	});
	rec.clear();
};