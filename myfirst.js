var http = require('http');
var dt = require('./myfirstmodule');
const { domainToASCII } = require('url');

http.createServer(function (req, res) {
    res.writeHead(200, {
        'Content-Type': 'text/html'
    });
    res.write("The date and time are currently: " + dt.myDateTime());
    res.end('Hello World. This is a new line.');
}).listen(8080);
