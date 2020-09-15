# express-cluster

Run an express server on multiple processes. This is meant to be dropped in
directly to your main entry point without having to setup a separate script
that manages workers.

This works with any EventListener that emits the `"close"` event and has a
`close()` method. If it's a server object (e.g. an express app, `net.Server` or
`http.Server`, ensure that you've invoked `listen` before returning it).

By default the module will spawn ''n'' processes where ''n'' is the number of
cores you have. You should configure this parameter for your environment.

## Synopsis

    var express = require('express');
    var cluster = require('express-cluster');

    cluster(function(worker) {
        var app = express();
        app.get('/', function(req, res) {
            res.send('hello from worker #' + worker.id);
        });
        return app.listen(0xbeef);
    }, {count: 5})

## API

express-cluster exports itself as a function that accepts `config` and
`workerFunctions` as arguments. These can be provided in either order:
`cluster(config, workerFunction)` or `cluster(workerFunction, config)`.

Once node executes `cluster()` the current process will be forked the specified
number of times. You should guard any code that should only be run in the
master behind a check of `process.env.NODE_UNIQUE_ID` or a call to node's
[cluster.isMaster](https://nodejs.org/api/cluster.html#cluster_cluster_ismaster)

### `workerFunction`

This function is passed a `worker` object. See the node documentation for
[Worker](https://nodejs.org/api/cluster.html#cluster_class_worker) for details.

### `config`

This object should contain zero or more of these keys. Any other key/values are
ignored.

    {
        count: 5,       // number of workers to spawn: defaults to CPU core count
        respawn: true,  // respawn process on exit: defaults to true
        verbose: false, // log what happens to console: defaults to false

        // Attach the given function to each spawned worker. The function will
        // be bound to the worker that sent the message so you can setup a two
        // way message bus if you please. See examples/messaging.js for an
        // example.
        workerListener: function(){}
    }
