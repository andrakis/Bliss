const https = require('https');
const http = require('http');
const fs = require('fs');

const options = {
  key: fs.readFileSync('key.pem'),
  cert: fs.readFileSync('cert.pem')
};

var active_clients = {};

function genid () { return "TODO"; }
var commands = {
	'/^id$/': () => {
		return [200, genid()]
	},
	'/^join$/': (req, capture) => {
		// TODO: verify
		active_clients[req.id] = client_join(req);
		return [200, active_clients[req.id].join_result()];
	},
	'/^client\/([^\/]+)\/request_work$/': (req, capture) => {
	},
};

function requestHandler (req, res) {
  console.log("GET");
  res.writeHead(200);
  res.end("hello world\n");
}

https.createServer(options, requestHandler).listen(8000);
http.createServer({}, requestHandler).listen(8001);

console.log("Listening on 8000, 8001");
