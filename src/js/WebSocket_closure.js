var ws_State = (function () {
    var state = {};
    
    return {
	get_socket : function () {
	    return state['ws'];
	},
	new_ws : function ( s) {
	    state['ws'] = new WebSocket(s);
	},
	
    }
    
} () );


/*
Message from server/open2 WebSocket_closure.js:17:11
open { target: WebSocket, isTrusted: true, srcElement: WebSocket, currentTarget: WebSocket, eventPhase: 2, bubbles: false, cancelable: false, returnValue: true, defaultPrevented: false, composed: false, … }
WebSocket_closure.js:18:11
ws_State.send('test send')
undefined
Message from server/message WebSocket_closure.js:21:11
message { target: WebSocket, isTrusted: true, data: "test send", origin: "ws://localhost:8000", lastEventId: "", ports: Restricted, srcElement: WebSocket, currentTarget: WebSocket, eventPhase: 2, bubbles: false, … }
WebSocket_closure.js:22:11
ws_State.close()
undefined
Message from server/close WebSocket_closure.js:25:11
close { target: WebSocket, isTrusted: true, wasClean: false, code: 1006, reason: "", srcElement: WebSocket, currentTarget: WebSocket, eventPhase: 2, bubbles: false, cancelable: false, … }
WebSocket_closure.js:26:11

​






*/


