    var express = require('express');
    var cluster = require('./lib');

    cluster(function(worker) {
        var app = express();
        app.get('/', function(req, res) {
            res.send('hello from worker #' + worker.id);
        });
        return app.listen(0xbeef);
    }, {count:10})
