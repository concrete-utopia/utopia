# back

[![build
status](https://secure.travis-ci.org/jcrugzz/back.png)](http://travis-ci.org/jcrugzz/back)

[![NPM](https://nodei.co/npm/back.png)](https://nodei.co/npm/back/)

A simple module to be used for creating exponentially weighted backoff attempts.
Originally extracted from [Primus][Primus].

__NOTICE__
If you were a pre-1.0.0 `back` user, the API has changed to what is found below.
If you do not like this slightly different abstraction and would prefer the
former, slightly simpler API, it is still available with `require('back/reconnect')`.

The API change thanks to a contribution from
[@Raynos](https://github.com/Raynos) makes things simpler as you don't have to
manage the copying of the options object yourself in order to handle repeated
backoff cases.

## Example

```js
var http = require('http');
var back = require('back');
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
  // The following option is shown with its default value but you will most
  // likely never define it as it creates the exponential curve.
  factor: 2,
};

// Where we will store the backoff instance during a particular backoff attempt
var attempt;

function retry(err) {
  var back = attempt || (attempt = new Back(options));
  return back.backoff(function (fail) {
    if (fail) {
      // Oh noez we never reconnect :(
      console.error('Retry failed with ' + err.message);
      process.exit(1);
    }
    //
    // Remark: .attempt and .timeout are added to this object internally
    //
    console.log('Retry attempt # ' + back.settings.attempt +
                ' being made after ' + back.settings.timeout + 'ms');
  request();
  });
}

function request() {
  http.get('http://localhost:9000', function (res) {
    console.log('Successful Response that will not happen!');
    //
    // If we succeeded, we would set the current to null so the next error
    // generates a new instance.
    //
    attempt = null;
  }).on('error', retry);
}

request();
```

## API

### `var back = new Back(backoffOpts);`

The `Back` constructor function takes your backoff options and saves them as
`settings` in the internal state of the `back` object.

#### `back.backoff(callback)`

The `back` instance has a `backoff` method that takes a  `callback` that is
executed after a `setTimeout`. The timeout is what is based on an [exponential
backoff](http://dthain.blogspot.nl/2009/02/exponential-backoff-in-distributed.html) of course!
It will repeatedly all this callback based on the backoff options you passed to
the back instance until it exhausts its efforts. When it has exhausted its
attempts, it will return an error as the first argument to the callback.

#### `back.close()`

Clear backoff timer in cases where you want to dispose of the instance before the `callback` is executed.

[Primus]: https://github.com/3rd-Eden/primus

