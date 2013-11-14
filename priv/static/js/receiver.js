var serverAddress = this.Address;
var audioContext;
var webSockets;
var streaming = false;

$(document).ready(function(){
	
	window.AudioContext = window.AudioContext || window.webkitAudioContext;		
	audioContext = new AudioContext();

	webSockets = new WebSocket("ws://" + serverAddress + "/receive");
	webSockets.onopen = function() { 
		getStreamers();
	};
	webSockets.onmessage = function(e) { handleWebSocketMessage(e.data) } 
});

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


var playSound = function(buffer) {
	var source = audioContext.createBufferSource();
	source.buffer = buffer;
	source.connect(audioContext.destination);
	source.start(0);
};

var onDecodeAudioDataFailed = function(e){
	console.log("Error on decoding Audio Data:" + e);
};

var drawStreamers = function(streamers){
	$('#streamer_list').remove(".list-group-item");
	for(var i=0; i < streamers.length; i++){
		var _this = streamers[i];
		$('#streamer_list').append(streamerAsHtml(_this));
	};
	defineStreamersOnClickEvent();
};

var addStreamer = function(streamer){
	$('#streamer_list').append(streamerAsHtml(streamer));
	defineStreamersOnClickEvent();
};

var defineStreamersOnClickEvent = function(){
	$(".list-group-item").click(function(){
		$('.active').removeClass("active");
		$(this).addClass("active");
		connectToStream($(this).data("pid"));
	});
};

var removeStreamer = function(streamerPid){
	var selector = '"#'+ pidToId(streamerPid) +'"';
	$('#' + pidToId(streamerPid)).remove();
};

var streamerAsHtml = function(streamer){
	return '<a href="#" id="'+pidToId(streamer["pid"])+'" class="list-group-item" data-pid="'+streamer["pid"]+'"> \
		<h4 class="list-group-item-heading">'+streamer["name"]+'@'+streamer["pid"]+'</h4> \
		<p class="list-group-item-text">'+streamer["desc"]+'</p> \
	</a>'
};

var pidToId = function(pid){
	return pid.replace(/\</g, "").replace(/\./g, "_").replace(/\>/g, "");
};

var getStreamers = function(){
	webSockets.send("get_streamers");
};

var disconectFromStream = function(streamer_pid){
	webSockets.send("disconnect");
	streaming = false;
};

var connectToStream = function(streamer_pid){
	webSockets.send("{\"connect\" : \"" + streamer_pid + "\"}");
	streaming = true;
};

var isJsonString = function(data){
	try{
		JSON.parse(data);
		return true;
	}catch (e) {
		return false;
	}
};