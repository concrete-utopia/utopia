/*!
 * node.flow
 * Copyright(c) 2011 Ben Lin <ben@dreamerslab.com>
 * MIT Licensed
 *
 * @fileoverview
 * A deadly simple flow control package for node.js
 */

/**
 * @private
 */
var fs = require( 'fs' );

/**
 * @private
 * @function
 */
var extend = require( 'node.extend' );
var slice  = [].slice;

/**
 * Check array equality
 * @private
 * @function
 * @this {global}
 * @param {Array} a First array to be compared
 * @param {Array} b second array to be compared
 * @returns {Bool} Whether if these 2 array match
 */
var arrays_equal = function ( a, b ){
  return !!a && !!b && ! ( a < b || b < a );
};



/**
 * Creates a new Flow.
 * @class Represents a flow control.
 * @requires extend
 * @requires fs
 * @constructor
 */
var Flow = function (){

  /**
   * Default auguments for all task functions.
   * @type {Array}
   * @default []
   */
  this._defaults = slice.call( arguments ) || [];

  /**
   * Series augument stack
   * @type {Array}
   */
  this._series_args = [];

  /**
   * Series task stack
   * @type {Array}
   */
  this._series = [];

  /**
   * Parallel augument stack
   * @type {Array}
   */
  this._ready_args = [];

  /**
   * Parallel augument stack
   * @type {Array}
   */
  this._parallel_args = [];

  /**
   * Parallel task stack
   * @type {Array}
   */
  this._parallel = [];

  /**
   * Parallel task group create counter
   * @type {Number}
   */
  this._group = 0;

  /**
   * Parallel task group execute counter
   * @type {Number}
   */
  this._count = 0;
};



Flow.prototype = {

/**
 * Default error handler.
 * @private
 * @this {Flow}
 * @param {Error} err Error to throw.
 */
  _error : function ( err ){
    throw err;
  },



/**
 * Check if the stack handler is the `next` handler.
 * @private
 * @this {Flow}
 * @param {String} str The handler to be check.
 */
  _is_next : function ( str ){
    return /self\[ '_' \+ next_type \]\.apply\( self, slice\.call\( arguments \)\);/g.test( str );
  },



/**
 * Call the current series function and remove it.
 * @private
 * @this {Flow}
 * @param {Array} args Arguments to be passed to the current series function.
 */
  _run_series : function ( args ){
    if( args.length && args[ 0 ] instanceof Error ){
      return this._error.apply( this, args );
    }

    try{
      this._series.shift().apply( this, args );
    }catch( err ){
      args.unshift( err );
      this._error.apply( this, args );
    }
  },



/**
 * Merge the next task arguments and call the next series function.
 * @private
 * @this {Flow}
 * @param {String|Number|Array|Object|Function} [arguments[ n ]] Arguments to be merged with the current arguments.
 */
  _next : function (){
    var args              = slice.call( arguments );
    var stack_series_args = this._series_args.shift();
    var next              = stack_series_args.pop();

    if( next ){
      if( stack_series_args.length === 0 ){
        stack_series_args = args;
      }else{
        extend( true, stack_series_args, args );
      }

      stack_series_args.push( next );
      this._run_series( stack_series_args );
    }
  },



/**
 * Call the current parallel functions and remove it.
 * @private
 * @this {Flow}
 * @param {Array} args Arguments to be added to the current parallel function.
 */
  _run_parallel : function ( args ){
    var self                = this;
    var parallel_args       = this._parallel_args.shift();
    var args_form_last_task = args;

    if( parallel_args === undefined ){
      throw new Error( '[node.flow] no parallel task assigned before calling `join`' );
    }

    this._count = parallel_args.length;

    this._parallel.shift().forEach( function ( task ){
      var _args = parallel_args.shift();

      if( args_form_last_task.length > 0 ){
        _args.unshift( args_form_last_task );
      }

      task.apply( self, _args );
    });

    this._group--;
  },



/**
 * Push the arguments from parallel tasks to a global stack,
 * merge them with the next task arguments and fire `this._run_series` at the last run
 * @private
 * @this {Flow}
 * @param {String|Number|Array|Object} [arguments[ n ]] Arguments to be merged with the next task arguments.
 */
  _ready : function (){
    var arg, stack_series_args;

    if( arguments.length > 0 ){
      this._ready_args.push( arguments );
    }

    this._count--;

    if( this._count === 0 ){
      stack_series_args = this._series_args.shift();

      if( this._ready_args.length > 0 ){
        stack_series_args.unshift( this._ready_args );
      }

      arg = stack_series_args;

      this._ready_args = [];
      this._run_series( arg );
    }
  },



/**
 * Add series or parallel task to the flow stack.
 * @public
 * @this {Flow}
 * @param {Arguments} args Arguments from this.series or this.parallel.
 * @param {String} next_type Assign this._next or this._ready to be the last prop in the arguments.
 * @param {Function} callback Callback function for this.series or this.parallel.
 * @param {Bool} merge_default Whether if to merge the default args.
 */
  _add : function ( args, next_type, callback, merge_default ){
    var self  = this;
    var task  = [].shift.call( args ) || function (){};
    var _args = merge_default ? extend( true, [], this._defaults ) : [];

    extend( true, _args, slice.call( args ));
    _args.push( function (){
      self[ '_' + next_type ].apply( self, slice.call( arguments ));
    });

    callback( _args, task );
  },



/**
 * Add series task to the flow stack.
 * @public
 * @this {Flow}
 * @param {Function} arguments[ 0 ] Task function to be called in series.
 * @param {String|Number|Array|Object|Function} [arguments[ 1 ]] Arguments to be passed to the task function(optional).
 * @returns {this} Return `this` to enable chaining.
 * @example
 *
 *     var Flow = require( 'node.flow' );
 *     var flow = new Flow();
 *
 *     flow.series( function( name, sort, next ){
 *       User.find({
 *         name : name
 *       }).sort( sort, -1 ).run( function ( err, users ){
 *         next( users );
 *       });
 *     }, 'bibi', 'created_at' );
 */
  series : function (){
    var self = this;

    this._add( arguments, 'next', function ( args, task ){
      self._series_args.push( args );
      self._series.push( task );
    }, true );

    return this;
  },



/**
 * Add parallel task to the flow stack.
 * @public
 * @this {Flow}
 * @param {Function} arguments[ 0 ] Task function to be called in parallel.
 * @param {String|Number|Array|Object|Function} [arguments[ 1 ]] Arguments to be passed to the task function.
 * @returns {this} Return `this` to enable chaining.
 * @example
 *
 *     var Flow = require( 'node.flow' );
 *     var flow = new Flow();
 *
 *     flow.parallel( function( name, sort, ready ){
 *       User.find({
 *         name : name
 *       }).sort( sort, -1 ).run( function ( err, users ){
 *         ready( users );
 *       });
 *     }, 'bibi', 'created_at' );
 */
  parallel : function (){
    var self = this;

    this._add( arguments, 'ready', function ( args, task ){
      var group = self._group;

      if( self._parallel[ group ] === undefined ){
        self._parallel_args[ group ] = [];
        self._parallel[ group ]      = [];
      }

      self._parallel_args[ group ].push( args );
      self._parallel[ group ].push( task );
    }, true );

    return this;
  },



/**
 * Set an end point for a group of parallel tasks.
 * @public
 * @this {Flow}
 * @param {Bool} is_parallel Whether if to use only first prop of the arguments
 * @returns {this} Return `this` to enable chaining.
 * @example
 *
 *     var Flow = require( 'node.flow' );
 *     var flow = new Flow();
 *
 *     flow.parallel( function( name, sort, ready ){
 *       User.find({
 *         name : name
 *       }).sort( sort, -1 ).run( function ( err, users ){
 *         ready( users );
 *       });
 *     }, 'bibi', 'created_at' );
 *
 *     flow.join();
 */
  join : function ( is_parallel ){
    var self = this;

    // arguments will not merged with defaults, only the args from last stack
    // if we pass the last arg as false
    this._add([ function (){
      var args = slice.call( arguments );
      var len  = arguments.length;

      if( len === 1 ){
        if( self._is_next( args[ 0 ])){
          args = [];
        }
      }else{
        // remove the last prop if it is a `next` method
        self._is_next( args[ len - 1 ]) && [].pop.call( args );

        // if last task is parallel
        if( is_parallel === true ) args = args[ 0 ];

        if( self._defaults.length > 0 && arrays_equal( args, self._defaults )){
          args = [];
        }
      }

      self._run_parallel( args );
    }], 'next', function ( args, task ){
      self._series_args.push( args );
      self._series.push( task );
    }, false );

    this._group++;

    return this;
  },

  error : function ( callback ){
    this._error = callback;

    return this;
  },



/**
 * Call the tasks one after another in the stack.
 * @public
 * @this {Flow}
 * @param {Function} arguments[ 0 ] Callback function when all tasks are finished.
 * @param {String|Number|Array|Object|Function} [arguments[ 1 ]] Arguments to be passed to the callback.
 * @returns {this} Return `this` to enable chaining.
 * @example
 *
 * var Flow  = require( 'node.flow' );
 * var flow  = new Flow();
 * var users = {};
 *
 *    // find users with the given names
 *    [ 'fifi', 'jenny', 'steffi' ].forEach( function ( name ){
 *      // assign 3 parallel tasks searching for users
 *      flow.parallel( function( users, name, ready ){
 *        User.findOne({
 *          name : name
 *        }, function ( err, user ){
 *          users[ name ] = user;
 *          ready();
 *        });
 *      }, users, name )
 *    });
 *
 *    flow.join();
 *
 *    // print out the search results
 *    flow.end( function( users ){
 *      console.log( users );
 *    });
 */
  end : function (){
    this.series.apply( this, arguments ).
         _run_series( this._series_args.shift());

    return this;
  }
};



/**
 * @public
 */
Flow.version = JSON.parse( fs.readFileSync( __dirname + '/../package.json', 'utf8' )).version;



/**
 * Exports module.
 */
module.exports = Flow;
