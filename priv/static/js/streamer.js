//-------------------------------------------------------------------
// @author Martin Fleischer <butjar@butjar-ThinkPad-X1-Carbon>
// @copyright (C) 2013, Martin Fleischer
//-------------------------------------------------------------------
var	rec;
var	mediaStreamSource;
var	audioContext;
var webSocket;
var streaming = false;
var serverAddress = this.Address;
var websocketPort = this.websocketPort;

// adds functionality for the start stream button opens websockets and
// starts the loop for audio streaming in an intervall
$(document).ready(function() {
	$("#streamer_input_form").submit(function(event){ 
		event.preventDefault();
		streamingButtonOnClick();
	});
	ws = openWebsocket(init());
	setInterval(function(){
		if(streaming){
			captureAudio();
		};
	}, 1);
});

// sets getUserMedia for different browsers creates an audio context
var init = function() {
	window.URL = window.URL || window.webkitURL;
	navigator.getUserMedia  = navigator.getUserMedia || navigator.webkitGetUserMedia ||
								navigator.mozGetUserMedia || navigator.msGetUserMedia;
	window.AudioContext = window.AudioContext || window.webkitAudioContext;
	audioContext = new AudioContext();
};
		
// opens a websocket and adds its callbacks. returns the websocket.		
var openWebsocket = function(callback){
	webSocket = new WebSocket("ws://" + location.hostname + ":" + websocketPort + "/record");
    webSocket.onopen = function() { callback };
    webSocket.onmessage = function(e) { handleWebSocketMessage(e.data) };
    webSocket.onerror = function(e) { console.log("Error on opening Websocket connection:");
									  console.log(e);
									};
	return webSocket;
};

// sends start message if the form has valid data
var startStream = function(){
	if(isValidForm()){
		disableFormAndButton();
		$("#submit_streamer_btn").removeClass("btn-primary")
							   	 .addClass("btn-danger")
							   	 .text("stop streaming");
		webSocket.send(getStreamerAsJsonString());
	}
};

// sends the message for stopping the stream
var stopStream = function(){
	$("#submit_streamer_btn").removeClass("btn-danger")
							 .addClass("btn-primary")
							 .text("Start streaming");
	disableFormAndButton();
	webSocket.send("stop_streamer");
}; 

// handles errors when microphone isn't available
var onGetUserMediaFailed = function(e) {
	stopStream();
	alert("Your Browser doesn't support WebRTC");
};

// uses recorder.js object for recording audio blobs
var captureAudio = function(){
	rec.exportWAV(function(blob) {
   	 	ws.send(blob);
	});
	rec.clear();
};

// toggles functionality of the button
var streamingButtonOnClick = function(){
	if(!streaming){
		startStream();
	}else {
		stopStream();
	};
};

// handles messages from the server
var handleWebSocketMessage = function(message){
		switch(message){
		case "streamer_started" : 
			onStreamStarted(); break;
		case "streamer_stopped" : 
			onStreamStopped(); break;
		default: 
			console.log("Can't handle Messag");
			console.log(messageObj);
	};
};

// starts streaming in the client and adjusts the page on client side after server has responded that the stream has started
var onStreamStarted = function(){
  	navigator.getUserMedia({video: false, audio: true}, function(localMediaStream) {
	mediaStreamSource = audioContext.createMediaStreamSource(localMediaStream);
	streaming = true;
	rec = new Recorder(mediaStreamSource);
	rec.record();
	enableButton();
	}, onGetUserMediaFailed);
};

// stops streaming in the client and adjusts the page after server responded that the stream has stopped
var onStreamStopped = function(){
	streaming = false;
	if(rec){
    	rec.stop();
    };
    mediaStreamSource = null;
	streaming = false;
	enableFormAndButton();
};

// reads fields of the form and creates the start_streamer message
var getStreamerAsJsonString = function(){
    var stream_name = $("#name_input").val();
    var lat = $("#lat_input").val();
    var lon = $("#long_input").val();
    var desc = $("#desc_input").val();
    var streamerObj = {start_streamer:{
    						stream_name:stream_name,
    						lat: lat,
    						lon: lon,
    						desc:desc
    				  }};
    return JSON.stringify(streamerObj, escapeJsonString);
};

// validates the form data
var isValidForm = function(){
	var bool = true;
	$(".form-group").removeClass("has-error");

	var stream_name = $("#name_input").val();
    var lat = $("#lat_input").val();
    var lon = $("#long_input").val();
    var desc = $("#desc_input").val();
	if( !(typeof stream_name === 'string') || stream_name == "" ){
		bool = false;
		$("#name-form-group").addClass("has-error");
	};
	if( !(typeof lat === 'string') || lat == "" || isNaN(parseFloat(lat)) ){
		bool = false;
		$("#lat-form-group").addClass("has-error");
	};
	if( !(typeof lon === 'string') || lon == "" || isNaN(parseFloat(lon)) ){
		bool = false;
		$("#long-form-group").addClass("has-error" );
	};
	if(!(typeof desc === 'string') || desc == "" ){
		bool = false;
		$("#desc-form-group").addClass("has-error");
	};
	return bool;
};

// enables form and button for input/click
var enableFormAndButton = function(){
	$("#streamer_input_fieldset").removeAttr('disabled');
	$("#submit_streamer_btn").removeAttr('disabled');
	$("#submit_streamer_btn").prop("disabled",false);;
};

// disables form and button for input/click
var disableFormAndButton = function(){
	$("#streamer_input_fieldset").attr('disabled','');
	$("#submit_streamer_btn").attr(disabled="disabled")
	$("#submit_streamer_btn").prop("disabled",true);
};

// enables button for click
var enableButton = function(){
	$("#submit_streamer_btn").removeAttr('disabled');
	$("#submit_streamer_btn").prop("disabled",false);;
};

// escapes the JSON string for mochijson2
var escapeJsonString = function(key, val) {
    if (typeof(val)!="string") return val;
    return val      
        .replace(/[\\]/g, '\\\\')
        .replace(/[\/]/g, '\\/')
        .replace(/[\b]/g, '\\b')
        .replace(/[\f]/g, '\\f')
        .replace(/[\n]/g, '\\n')
        .replace(/[\r]/g, '\\r')
        .replace(/[\t]/g, '\\t')
        .replace(/[\"]/g, '\\"')
        .replace(/\\'/g, "\\'"); 
};