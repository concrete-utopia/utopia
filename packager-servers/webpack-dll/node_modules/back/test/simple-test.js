var http = require('http');
var test = require('tape');

var Back = require('..');

test('wooo does exponential backoff work as expected?', function (t) {
  t.plan(3);
  var count = 0,
      timeouts = [1000, 4000, 9000];
  //
  // Options to use for backoff
  //
  // Remark: This object is modified so it should be cloned if you are dealing
  // with independent backoff attempts and want to use these values as a base.
  //
  var options = {
    retries: 3,
    minDelay: 1000, // Defaults to 500ms
    maxDelay: 10000, // Defaults to infinity
    // The following option is shown with its default value
    // but you will most likely never define it.
    factor: 2,
  };

  // Where we will store the backoff instance during a particular backoff attempt
  var attempt;

  function retry(err) {
    var back = attempt || (attempt = new Back(options));
    return back.backoff(function (fail) {
      if (fail) {
        // Oh noez we never reconnect :(
        return t.end();
      }

      t.ok(back.settings.timeout >= timeouts[count++], 'Successful backoff with timeout ' +
           back.settings.timeout);
    request();
    });
  }

  function request() {
    http.get('http://localhost:9000', function (res) {
      console.log('Successful Response that will not happen!');
      //
      // If we succeeded, we would set the current to null so the next error
      // generates a new instance.
      attempt = null;
    }).on('error', retry);
  }

  request();
});

