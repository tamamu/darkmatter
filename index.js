const http = require('http');
const url = require('url');
const path = require('path');
const fs = require('fs');
const childProcess = require('child_process');

const DEFAULT_FILE = 'static/index.html';
const LISTEN_IP = process.argv[2] || '127.0.0.1';
const LISTEN_PORT = process.argv[3] || 5000;

const ContentType = {
	'.html': 'text/html',
	'.htm': 'text/htm',
	'.css': 'text/css',
	'.js': 'text/javascript; charset=utf-8',
	'.json': 'application/json; charset=utf-8',
};

function getContentType(fname) {
	let ext = path.extname(fname).toLowerCase();
	let content_type = ContentType[ext];
	if (content_type === undefined) {
		content_type = 'text/plain';
	}

	return content_type;
}

let server = http.createServer();
server.on('request',
		(req, res) => {
			let requested_file = req.url;
			requested_file = (requested_file.substring(requested_file.length - 1, 1) === '/')
				? requested_file + DEFAULT_FILE
				: requested_file;
			fs.readFile('.' + requested_file, 'binary', (err, data) => {
				if (err) {
					res.writeHead(404, {'Content-Type': 'text/plain'});
					res.write('not found\n');
					res.end();
				} else {
					res.writeHead(200, {'Content-Type': getContentType(requested_file)});
					res.write(data, 'binary');
					res.end();
				}
			});
		}
);

let eval_process = childProcess.spawn('clackup', ['app.lisp', '--server', ':woo', '--port', '8888'], {stdio: 'inherit'});
eval_process.on('exit', (code) => {
	console.log('eval server exited.');
});
eval_process.on('error', (err) => {
	console.log(err);
	process.exit(1);
});

server.listen(LISTEN_PORT, LISTEN_IP);
console.log(`Server running at http://${LISTEN_IP}:${LISTEN_PORT}`);
