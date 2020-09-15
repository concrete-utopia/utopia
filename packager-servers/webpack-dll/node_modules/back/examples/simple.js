var http = require('http');
var back = require('../');
//
// Options to use for backoff
//
// Remark: This object is modified so it should be cloned if you are dealing
// with independent backoff attempts and want to use these values as a base.
//
var backoff = {
  retries: 3,
  minDelay: 1000, // Defaults to 500ms
  maxDelay: 10000, // Defaults to infinity
  // The following option is shown with its default value but you will most
  // likely never define it as it creates the exponential curve.
  factor: 2,
};

function retry(err) {
  return back(function (fail) {
    if (fail) {
      // Oh noez we never reconnect :(
      console.error('Retry failed with ' + err.message);
      process.exit(1);
    }
    //
    // Remark: .attempt and .timeout are added to this object internally
    //
    console.log('Retry attempt # ' + backoff.attempt +
                ' being made after ' + backoff.timeout + 'ms');
  request();
  }, backoff);
}

function request() {
  http.get('http://localhost:9000', function (res) {
    console.log('Successful Response that will not happen!');
  }).on('error', retry);
}

request();
