/*
 * sourcemapped-stacktrace.js
 * created by James Salter <iteration@gmail.com> (2014)
 *
 * https://github.com/novocaine/sourcemapped-stacktrace
 *
 * Licensed under the New BSD license. See LICENSE or:
 * http://opensource.org/licenses/BSD-3-Clause
 */

/*global define */

// note we only include source-map-consumer, not the whole source-map library,
// which includes gear for generating source maps that we don't need
define(['source-map/lib/source-map-consumer'],
function(source_map_consumer) {

  var global_mapForUri = {};

  /**
   * Re-map entries in a stacktrace using sourcemaps if available.
   *
   * @param {str} stack - The stacktrace from the browser.
   * @param {function} done - Callback invoked with the transformed stacktrace
   *                          (an Array of Strings) passed as the first
   *                          argument
   * @param {Object} [opts] - Optional options object.
   * @param {Function} [opts.filter] - Filter function applied to each stackTrace line.
   *                                   Lines which do not pass the filter won't be processesd.
   * @param {boolean} [opts.cacheGlobally] - Whether to cache sourcemaps globally across multiple calls.
   * @param {boolean} [opts.sync] - Whether to use synchronous ajax to load the sourcemaps.
   * @param {string} [opts.traceFormat] - If `error.stack` is formatted according to chrome or
   *                                      Firefox's style.  Can be either `"chrome"`, `"firefox"`
   *                                      or `undefined` (default).  If `undefined`, this library
   *                                      will guess based on `navigator.userAgent`.
   */
  var mapStackTrace = function(stack, done, opts) {
    var lines;
    var line;
    var mapForUri = {};
    var rows = {};
    var fields;
    var uri;
    var expected_fields;
    var regex;
    var skip_lines;

    var fetcher = new Fetcher(opts);

    var traceFormat = opts && opts.traceFormat;
    if (traceFormat !== "chrome" && traceFormat !== "firefox") {
      if (traceFormat) {
        throw new Error("unknown traceFormat \"" + traceFormat + "\" :(");
      } else if (isChromeOrEdge() || isIE11Plus()) {
        traceFormat = "chrome";
      } else if (isFirefox() || isSafari()) {
        traceFormat = "firefox";
      } else {
        throw new Error("unknown browser :(");
      }
    }

    if (traceFormat === "chrome") {
      regex = /^ +at.+\((.*):([0-9]+):([0-9]+)/;
      expected_fields = 4;
      // (skip first line containing exception message)
      skip_lines = 1;
    } else {
      regex = /@(.*):([0-9]+):([0-9]+)/;
      expected_fields = 4;
      skip_lines = 0;
    }

    lines = stack.split("\n").slice(skip_lines);

    for (var i=0; i < lines.length; i++) {
      line = lines[i];
      if ( opts && opts.filter && !opts.filter(line) ) continue;
      
      fields = line.match(regex);
      if (fields && fields.length === expected_fields) {
        rows[i] = fields;
        uri = fields[1];
        if (!uri.match(/<anonymous>/)) {
          fetcher.fetchScript(uri);
        }
      }
    }

    fetcher.sem.whenReady(function() {
      var result = processSourceMaps(lines, rows, fetcher.mapForUri, traceFormat);
      done(result);
    });
  };

  var isChromeOrEdge = function() {
    return navigator.userAgent.toLowerCase().indexOf('chrome') > -1;
  };

  var isFirefox = function() {
    return navigator.userAgent.toLowerCase().indexOf('firefox') > -1;
  };  

  var isSafari = function() {
    return navigator.userAgent.toLowerCase().indexOf('safari') > -1;
  };
		
  var isIE11Plus = function() {
   	return document.documentMode && document.documentMode >= 11;
  };


  var Semaphore = function() {
    this.count = 0;
    this.pending = [];
  };

  Semaphore.prototype.incr = function() {
    this.count++;
  };

  Semaphore.prototype.decr = function() {
    this.count--;
    this.flush();
  };

  Semaphore.prototype.whenReady = function(fn) {
    this.pending.push(fn);
    this.flush();
  };

  Semaphore.prototype.flush = function() {
    if (this.count === 0) {
        this.pending.forEach(function(fn) { fn(); });
        this.pending = [];
    }
  };


  var Fetcher = function(opts) {
    this.sem = new Semaphore();
    this.sync = opts && opts.sync;
    this.mapForUri = opts && opts.cacheGlobally ? global_mapForUri : {};
  };

  Fetcher.prototype.ajax = function(uri, callback) {
    var xhr = createXMLHTTPObject();
    var that = this;
    xhr.onreadystatechange = function() {
      if (xhr.readyState == 4) {
        callback.call(that, xhr, uri);
      }
    };
    xhr.open("GET", uri, !this.sync);
    xhr.send();
  }

  Fetcher.prototype.fetchScript = function(uri) {
    if (!(uri in this.mapForUri)) {
      this.sem.incr();
      this.mapForUri[uri] = null;
    } else {
      return;
    }

    this.ajax(uri, this.onScriptLoad);
  };

  var absUrlRegex = new RegExp('^(?:[a-z]+:)?//', 'i');

  Fetcher.prototype.onScriptLoad = function(xhr, uri) {
    if (xhr.status === 200 || (uri.slice(0, 7) === "file://" && xhr.status === 0)) {
      // find .map in file.
      //
      // attempt to find it at the very end of the file, but tolerate trailing
      // whitespace inserted by some packers.
      var match = xhr.responseText.match("//# [s]ourceMappingURL=(.*)[\\s]*$", "m");
      if (match && match.length === 2) {
        // get the map
        var mapUri = match[1];

        var embeddedSourceMap = mapUri.match("data:application/json;(charset=[^;]+;)?base64,(.*)");

        if (embeddedSourceMap && embeddedSourceMap[2]) {
          this.mapForUri[uri] = new source_map_consumer.SourceMapConsumer(atob(embeddedSourceMap[2]));
          this.sem.decr();
        } else {
          if (!absUrlRegex.test(mapUri)) {
            // relative url; according to sourcemaps spec is 'source origin'
            var origin;
            var lastSlash = uri.lastIndexOf('/');
            if (lastSlash !== -1) {
              origin = uri.slice(0, lastSlash + 1);
              mapUri = origin + mapUri;
              // note if lastSlash === -1, actual script uri has no slash
              // somehow, so no way to use it as a prefix... we give up and try
              // as absolute
            }
          }

          this.ajax(mapUri, function(xhr) {
            if (xhr.status === 200 || (mapUri.slice(0, 7) === "file://" && xhr.status === 0)) {
              this.mapForUri[uri] = new source_map_consumer.SourceMapConsumer(xhr.responseText);
            }
            this.sem.decr();
          });
        }
      } else {
        // no map
        this.sem.decr();
      }
    } else {
      // HTTP error fetching uri of the script
      this.sem.decr();
    }
  };

  var processSourceMaps = function(lines, rows, mapForUri, traceFormat) {
    var result = [];
    var map;
    var origName = traceFormat === "chrome" ? origNameChrome : origNameFirefox;
    for (var i=0; i < lines.length; i++) {
      var row = rows[i];
      if (row) {
        var uri = row[1];
        var line = parseInt(row[2], 10);
        var column = parseInt(row[3], 10);
        map = mapForUri[uri];

        if (map) {
          // we think we have a map for that uri. call source-map library
          var origPos = map.originalPositionFor(
            { line: line, column: column });
          result.push(formatOriginalPosition(origPos.source,
            origPos.line, origPos.column, origPos.name || origName(lines[i])));
        } else {
          // we can't find a map for that url, but we parsed the row.
          // reformat unchanged line for consistency with the sourcemapped
          // lines.
          result.push(formatOriginalPosition(uri, line, column, origName(lines[i])));
        }
      } else {
        // we weren't able to parse the row, push back what we were given
        result.push(lines[i]);
      }
    }

    return result;
  };

  function origNameChrome(origLine) {
    var match = / +at +([^ ]*).*/.exec(origLine);
    return match && match[1];
  }

  function origNameFirefox(origLine) {
    var match = /([^@]*)@.*/.exec(origLine);
    return match && match[1];
  }

  var formatOriginalPosition = function(source, line, column, name) {
    // mimic chrome's format
    return "    at " + (name ? name : "(unknown)") +
      " (" + source + ":" + line + ":" + column + ")";
  };

  // xmlhttprequest boilerplate
  var XMLHttpFactories = [
	function () {return new XMLHttpRequest();},
	function () {return new ActiveXObject("Msxml2.XMLHTTP");},
	function () {return new ActiveXObject("Msxml3.XMLHTTP");},
	function () {return new ActiveXObject("Microsoft.XMLHTTP");}
  ];

  function createXMLHTTPObject() {
      var xmlhttp = false;
      for (var i=0;i<XMLHttpFactories.length;i++) {
          try {
              xmlhttp = XMLHttpFactories[i]();
          }
          catch (e) {
              continue;
          }
          break;
      }
      return xmlhttp;
  }

  return {
    mapStackTrace: mapStackTrace
  }
});
