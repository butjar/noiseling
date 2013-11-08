var	rec;
var	mediaStreamSource;
var	audioContext;
var webSockets;
var streaming = false;
var serverAddress = this.Address;

$(document).ready(function() {
	$("#streamer_input_form").submit(function(event){ 
		event.preventDefault();
		streamingButtonOnClick();
	});
	ws = openWebsockets(init());
	setInterval(function(){
		if(streaming){
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
};
				
var openWebsockets = function(callback){
	webSockets = new WebSocket("ws://" + serverAddress + "/stream");
    webSockets.onopen = function() { callback };
    webSockets.onmessage = function(e) { handleWebSocketMessage(e.data) };
	return webSockets;
};

var startWebRtcSession = function(){
  	navigator.getUserMedia({video: false, audio: true}, function(localMediaStream) {
    	mediaStreamSource = audioContext.createMediaStreamSource(localMediaStream);
    	rec = new Recorder(mediaStreamSource);
    	rec.record();
	}, onGetUserMediaFailed);
};

// var stopWebRtcSession = function(){
//     mediaStreamSource = null;
//     rec.stop();
// };

var startStream = function(){
	if(isValidForm()){
		webSocketsSendForm();
		disableFormAndButton();
		$("#submit_streamer_btn").removeClass("btn-primary")
							   	 .addClass("btn-danger")
							   	 .text("stop streaming");
	}
};

var stopStream = function(){
	$("#submit_streamer_btn").removeClass("btn-danger")
							 .addClass("btn-primary")
							 .text("Start streaming");
	disableFormAndButton();
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

var streamingButtonOnClick = function(){
	if(!streaming){
		startStream();
	}else {
		stopStream();
	};
};

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

var onStreamStarted = function(){
	startWebRtcSession();
	streaming = true;
	enableFormAndButton();
};

var onStreamStopped = function(){
	streaming = false;
	enableFormAndButton();
};

var webSocketsSendForm = function(){
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
    Msg = JSON.stringify(streamerObj, escapeJsonString);
    ws.send(Msg);
};

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

var enableFormAndButton = function(){
	$("#streamer_input_fieldset").removeAttr('disabled');
	$("#submit_streamer_btn").removeAttr('disabled');
};

var disableFormAndButton = function(){
	$("#streamer_input_fieldset").attr('disabled','');
	$("#submit_streamer_btn").attr(disabled="disabled");
};
	
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