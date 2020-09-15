# node.flow

A deadly simple flow control package for node.js



## Description

The asynchronous nature of javascript is what makes it so powerful. However sometimes you just need to do stuffs synchronously without blocking the event loop. Stuffs like query database in a loop with ids to assemble a hash, compressing a bunch of files in groups and compare with the old ones. You could easily end up with nested callbacks without using a flow control package.

With node.flow you can set a work flow doing things one by one or in parallel, wait for all things are done in the parallel tasks to do the next task. You can set some default arguments for all tasks, giving each task its own arguments or pass task results to the next task as its arguments. The following code shows some base usage and the syntax of this package.

    // setup db schema and connection
    require( './setup' );

    var Flow     = require( '../../lib/flow' );
    var mongoose = require( 'mongoose' );
    var User     = mongoose.model( 'User' );
    var data     = require( './data' );

    // start a new flow
    var flow  = new Flow;
    var users = {};

    // delete all users before start
    flow.series( function ( next ){
      User.remove( function ( err, count ){
        next();
      });
    });

    // insert records from source data
    data.users.forEach( function ( user ){
      flow.parallel( function ( user, ready ){
        new User( user ).save( function ( err, user ){
          ready();
        });
      }, user );
    });

    // we must set an end point for parallel tasks
    flow.join();

    // find matching records
    data.names.forEach( function ( name ){
      flow.parallel( function( name, ready ){
        User.findOne({
          name : name
        }, function ( err, user ){
          users[ name ] = user;
          ready();
        });
      }, name );
    });

    flow.join();

    // print out records and disconnect
    flow.end( function(){
      console.log( users );
      mongoose.disconnect();
    });



## Requires

Checkout `package.json` for dependencies.



## Installation

Install node.flow through npm

    npm install node.flow



## Usage

> Require the module before using

    var Flow = require( 'node.flow' );



### new Flow( arg1, arg2, ... );

Start a new flow.

#### Arguments

> arg1, arg2, ...

    type: Function | String | Array | Object | Boolean
    desc: arguments to be passed to the new flow as defaults.

#### Example code

    var flow = new Flow( 'bibi', 22, true );



### flow.series( task, arg1, arg2, ... );

Add series task to the flow stack.

#### Arguments

> task

    type: Function
    desc: Task function to be called in series.

> arg1, arg2, ...

    type: Function | String | Array | Object | Boolean
    desc: Arguments to be passed to the task function( optional ).

#### Example code

    var Flow = require( 'node.flow' );
    var flow = new Flow();

    // Add a task function, the last argument in the task callback
    // is always the next task
    flow.series( function( name, sort, next ){
      User.find({
        name : name
      }).sort( sort, -1 ).run( function ( err, users ){
        // call the next series task
        next( users );
      });

    // 'bibi' will be passed to the task function as the first argument `name`
    // and 'created_at' will be the second argument `sort`
    // you can series as many arguments as you want
    }, 'bibi', 'created_at' );



### flow.parallel( task, arg1, arg2, ... );

Add parallel task to the flow stack.

#### Arguments

> callback

    type: Function
    desc: Task function to be called in parallel.

> arg1, arg2, ...

    type: Function | String | Array | Object | Boolean
    desc: Arguments to be passed to the task function( optional ).

#### Example code

    var Flow = require( 'node.flow' );
    var flow = new Flow();

    flow.parallel( function( name, sort, ready ){
      User.find({
        name : name
      }).sort( sort, -1 ).run( function ( err, users ){
        ready( users );
      });
    }, 'bibi', 'created_at' );



### flow.join();

Set an end point for a group of parallel tasks.

#### Example code

    var Flow = require( 'node.flow' );
    var flow = new Flow();

    flow.parallel( function( name, sort, ready ){
      User.find({
        name : name
      }).sort( sort, -1 ).run( function ( err, users ){
        ready( users );
      });
    }, 'bibi', 'created_at' );

    flow.join();



### flow.error( callback );

Error handler for in case there is any.

#### Arguments

> callback

    type: Function
    desc: Error handler to break the flow.

#### Example code

    var Flow = require( 'node.flow' );
    var flow = new Flow();

    flow.error( function ( err ){
      console.log( err );
    });



### flow.end( callback, arg1, arg2, ... );

Call the tasks one after another in the stack.

#### Arguments

> callback

    type: Function
    desc: The last callback to be called at the very end after all tasks are done

> arg1, arg2, ...

    type: Function | String | Array | Object | Boolean
    desc: Arguments to be passed to the callback function( optional )

#### Example code

    var Flow = require( 'node.flow' );
    var flow = new Flow();
    var users = {};

    // find users with the given names
    [ 'fifi', 'jenny', 'steffi' ].forEach( function ( name ){
      // assign 3 parallel tasks searching for users
      flow.parallel( function( users, name, ready ){
        User.findOne({
          name : name
        }, function ( err, user ){
          users[ name ] = user;
          ready();
        });
      }, users, name )
    });

    flow.join();

    // print out the search results
    flow.end( function( users ){
      console.log( users );
    });



## Arguments merge and overwrite

You can set some default arguments for all tasks, giving each task its own arguments or pass task results to the next task as its arguments. The priority is `argements from last task` > `argements for each task` > `default argements`. Which means `default argements` will be merge into `argements for each task` and finally merge into `argements from last task` than pass to the next task. However with parallel tasks it works a little different. Results from parallel tasks will be push to a stack, when all parallel tasks are done the result stack will be the first argument assign to `argements for each task` unless the result stack is empty. Checkout the parallel example for a clear view.



## Chainability

You can either choose to chain your methods or not up to your personal taste. Both of the following syntax works.

    // chaining all methods
    flow.series( function (){
      // do stuffs ...
    }).parallel( function (){
      // do stuffs ...
    }).parallel( function (){
      // do stuffs ...
    }).join().
    series( function (){
      // do stuffs ...
    end( function (){
      // all done callback
    });

    // seperate all methods
    flow.series( function (){
      // do stuffs ...
    });

    flow.parallel( function (){
      // do stuffs ...
    });

    flow.parallel( function (){
      // do stuffs ...
    });

    flow.join();

    flow.series( function (){
      // do stuffs ...
    });

    flow.end( function (){
      // all done callback
    });

## Examples

> Checkout the `examples` folder for more details.

### series

> Demonstrate the basic usage of series task and syntax. We use setTimeout to simulate a time consuming io operation. We can see how the arguments are merged and overwrote in the example.

    $ cd /path/to/node.flow/examples/series
    $ node run.js

### parallel

> Demonstrate the basic usage of parallel task and syntax.

    $ cd /path/to/node.flow/examples/parallel
    $ node run.js

### mongoose

> Demonstrate how to clear the documents before inserting a bunch of records then finding some records with given conditions in a loop and show them without writing nested callbacks. Use both series and parallel tasks.

    # make sure your mongoDB is on
    $ cd /path/to/node.flow/examples/mongoose
    $ npm install -lf
    $ node run.js

### node.packer

> Demonstrate how to compress a bunch of files in groups.

    $ cd /path/to/node.flow/examples/node.packer
    $ npm install -lf
    $ node app.js



## License

(The MIT License)

Copyright (c) 2011 dreamerslab &lt;ben@dreamerslab.com&gt;

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
'Software'), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
