window.onload = function() {
    console.log("window.onload");
    var ws = createWebSocket('/');

    ws.onopen = function() {
        console.log('onopen');
    };

    ws.onmessage = function (event) {
        var i = Number(event.data);
        var counter = document.getElementById("counter");
        counter.innerHTML = i;
        var boxes10 = document.getElementById("boxes10");
        if(i % 10 == 0) {
            boxes10.innerHTML = '';
            var boxes1 = document.getElementById("boxes1");
            if(i % 100 == 0) {
                boxes1.innerHTML = '';
            } else {
                var div = document.createElement("div"); 
                div.className = "box";
                boxes1.appendChild(div);
            }
        } else {
            var div = document.createElement("div"); 
            div.className = "box";
            boxes10.appendChild(div);
        }
    }
}

function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':8080' + path;
    return new WebSocket(uri);
}