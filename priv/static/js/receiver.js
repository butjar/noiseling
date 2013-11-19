//-------------------------------------------------------------------
// @author Martin Fleischer <butjar@butjar-ThinkPad-X1-Carbon>
// @copyright (C) 2013, Martin Fleischer
//-------------------------------------------------------------------
// see: 
// http://www.smartjava.org/content/record-audio-using-webrtc-chrome-and-speech-recognition-websockets
// http://www.html5rocks.com/en/tutorials/webaudio/intro/
// http://www.html5rocks.com/en/tutorials/getusermedia/intro/
// https://github.com/mattdiamond/Recorderjs
var serverAddress = this.Address;
var audioContext;
var webSocket;
var streaming = false;
var websocketPort = this.websocketPort;

// sets up audio context and creates a websocket connected to the application
$(document).ready(function(){
	
	window.AudioContext = window.AudioContext || window.webkitAudioContext;		
	audioContext = new AudioContext();

	webSocket = new WebSocket("ws://" + location.hostname + ":" + websocketPort + "/receive");
	webSocket.onopen = function() { 
		getStreamers();
	};
	webSocket.onmessage = function(e) { handleWebSocketMessage(e.data) };
	webSocket.onerror = function(e) { console.log("Error on opening Websocket connection:");
									  console.log(e);
									};
});

// handles messages from the server
var handleWebSocketMessage = function(data){
	if(data instanceof Blob){
			playChunk(data)
	} else if(isJsonString(data)){
			handleJsonMessage(JSON.parse(data));
	} else {
			console.log("Don't know Message:");
			console.log(data);
	};
};

// handles messages in JSON format
var handleJsonMessage = function(messageObj){
	var command = Object.keys(messageObj)[0];
	switch(command){
		case "streamers" : 
			drawStreamers(messageObj["streamers"]); break;
		case "streamer_added_event" : 
			addStreamer(messageObj["streamer_added_event"]); break;
		case "streamer_removed_event" :
			removeStreamer(messageObj["streamer_removed_event"]); break;
		default: 
			console.log("Can't handle Messag");
			console.log(messageObj);
	};
};

// decodes audio blobs received from the application and plays them
var playChunk = function(chunk){
	var reader = new FileReader();
	reader.addEventListener("loadend", function() {
		var arraybuffer = reader.result;
		audioContext.decodeAudioData(arraybuffer, function(buffer) {
			audiobuffer = buffer;
			playSound(audiobuffer);
		}, onDecodeAudioDataFailed);
	 });
	reader.readAsArrayBuffer(chunk);
};

// uses audio context for playing an audiobuffer in the browser
var playSound = function(buffer) {
	var source = audioContext.createBufferSource();
	source.buffer = buffer;
	source.connect(audioContext.destination);
	source.start(0);
};

// handles errors while decoding audio blobs
var onDecodeAudioDataFailed = function(e){
	console.log("Error on decoding Audio Data:" + e);
};

// adds the given list to the streamer view
var drawStreamers = function(streamers){
	$('#streamer_list').remove(".list-group-item");
	for(var i=0; i < streamers.length; i++){
		var _this = streamers[i];
		$('#streamer_list').append(streamerAsHtml(_this));
	};
	defineStreamersOnClickEvent();
};

// adds a single streamer to the view
var addStreamer = function(streamer){
	$('#streamer_list').append(streamerAsHtml(streamer));
	defineStreamersOnClickEvent();
};

// on click callback for streamer items in the view
var defineStreamersOnClickEvent = function(){
	$(".list-group-item").click(function(){
		$('.active').removeClass("active");
		$(this).addClass("active");
		connectToStream($(this).data("pid"));
	});
};

// removes a single streamer from the view
var removeStreamer = function(streamerPid){
	var selector = '"#'+ pidToId(streamerPid) +'"';
	$('#' + pidToId(streamerPid)).remove();
};

// returns the stramer as html String
var streamerAsHtml = function(streamer){
	return '<a href="#" id="'+pidToId(streamer["pid"])+'" class="list-group-item" data-pid="'+streamer["pid"]+'"> \
		<h4 class="list-group-item-heading">'+streamer["name"]+'@'+streamer["pid"]+'</h4> \
		<p class="list-group-item-text">'+streamer["desc"]+'</p> \
	</a>'
};

// decodes given pid for use as html element id
var pidToId = function(pid){
	return pid.replace(/\</g, "").replace(/\./g, "_").replace(/\>/g, "");
};

// sends the get_streamers message to the server
var getStreamers = function(){
	webSocket.send("get_streamers");
};

// sends the disconnect message to the server
var disconectFromStream = function(streamer_pid){
	webSocket.send("disconnect");
	streaming = false;
};

// sends connect message to the server
var connectToStream = function(streamer_pid){
	webSocket.send("{\"connect\" : \"" + streamer_pid + "\"}");
	streaming = true;
};

// validates if give string has the JSON format
var isJsonString = function(data){
	try{
		JSON.parse(data);
		return true;
	}catch (e) {
		return false;
	}
};