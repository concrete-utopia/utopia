# mana

[![Build Status](https://travis-ci.org/3rd-Eden/mana.svg)](https://travis-ci.org/3rd-Eden/mana)

Mana is an small package that provides basic and a dead simple API structure for
creating high performance API clients. Take your mana potion and start creating
magical API clients which contain the following powers:

- **Rolling tokens** Supports multiple OAuth tokens which will be switched when
  rate limits are hit.
- **Callback queue** Multiple requests calls to the same URL will result in a
  single call.
- **Mirrors** When API endpoints become unresponsive, alternate replica's or
  mirrors API's can be hit instead.
- **Back off** Build in exponential back off when the API endpoint returns an
  error or error code.
- **Conditional requests** Requests with Etags can be cached and result will be
  reused when a 304 is returned. (Supports async and sync cache engines.)

## Installation

The module is released through npm.

```
npm install --save mana
```

## Assumptions

Before you get started with building your first mana based API client there are
some assumptions we make

### Tokens

We assume that the supplied token(s) should be used as `Authorization` header
and that the supplied token should be prefixed with `token `.

### Rate limiting

Again, we have to make some sane assumptions here as well. There tons of ways
that an API server can say that you've reached your limit. We assume that it
sends the following headers with each HTTP response: 

- `x-ratelimit-reset` Time when the limit is reset in UTC EPOCH seconds.
- `x-ratelimit-limit` Maximum of requests the user can make.
- `x-ratelimit-remaining` The amount of requests the user has left.

We will only take these values in to account when multiple tokens are used and a
none `200` status code has been returned from the server.

### Caching

In order to relieve stress on the API server that you're implementing there is
an option to supply a cache instance which will be used to store responses that
have `etag` headers. When we're about to request an API we check if the URL has
been stored in the cache and use the stored `etag` in the `if-none-match` header
so we can trigger a `304` response on the API end point and use our cached data
instead. This reduces the amount of data you need receive over the connection
and some API providers like GitHub don't count `304` requests as part of their
rate limiting.

See [mana.fireforget()](#manafireforget) for more details on the cache API
requirements.

## Usage

In all of the examples we assume that you've loaded the library using:

```js
'use strict';

var mana = require('mana');
```

To create you own custom mana instance you need to extend the returned mana
instance. Extending is done by calling the `mana.extend` method with an object
which will be merged on the prototype:

```js
var MyAPI = mana.extend({
  api: 'https://api.im-implementing.com/'
});
```

In the code snippet you see us adding the `api` property and storing the result
of the extending as the `MyAPI` class. The `api` property is one of the
properties that are required and need to be specified on every single instance.
The following properties are required to be specified on your custom mana:

- `api` The URL of the site we're communicating with.
- `name` Name of your module.
- `version` Version number of your module.

The `version` and `name` can be set automatically if you are using the
`.drink(module)` method to expose the module and lazy load the API endpoints. It
does make the assumption that `package.json` file is in the same directory as
the file that calls the `.drink` method.

There are also a couple of properties which are optional but can be overridden if
needed:

- `maxdelay` The maximum delay for the exponential back off. Defaults to `60
  seconds`.
- `mindelay` The minimum delay for the exponential back off. Defaults to `100 ms`.
- `retries` The maximum of retries of the API call we can do. Defaults to 3.
- `factor` The exponential back off factor. Defaults to 2.
- `timeout` How long a request should run before we automatically assume it
  timed out. Defaults to `20 seconds`.
- `_view` The prefix to access CouchDB view/design doc. Defaults to `/-/_view/`
  which is the same as `npm` is using.
- `strictSSL` Should we request everything with strict SSL. Defaults to `false`.
- `maxSockets` Maximum amount of sockets. Defaults to `444`.
- `prefix` The prefix for the authorization header value, defaults to `token `.

Every property that represents time can be set with either a number in
milliseconds as value or a human readable string like `10 seconds`. This is
parsed to milliseconds automatically but improves the readability of your code
base.

### mana.querystring()

Transform an random object in a query string. If there are no keys or matches in
the object an empty string will be returned instead. The function accepts 2
arguments:

1. `options` The supplied object/options where we extract the parameters from
2. `allowed` An array with keys that are accepted as parameters or an object
   with key->value where key are the accepted parameters and value's the default
   value for when it's missing on the supplied object.

```js
mana.extend({
  initialize: function (options) {
    this.api = this.querystring(options, [
      'foo',
      'bar'
    ]);

    //
    // if the options object had foo and bar the `this.api` would now be:
    //
    //   ?foo=<value>&bar=<value>
    //

    this.api = this.querystring(options, {
      foo: 'foo',
      bar: 'bar'
    });

    //
    // if options was empty, the `this.api` would now be
    //
    //   ?foo=foo&bar=bar
    //
  }
});
```

### mana.json()

This does exactly the same the `mana.querystring` method but instead of
returning a query string it will return a new object.

```js
mana.extend({
  initialize: function (options) {
    options = this.json(options, {
      foo: 'foo',
      bar: 'bar'
    });
  }
});
```

### mana.debug()

Add extra debug information which can be triggered enabling the [debug](#debugging)
flags when you start your application. It's not advised to override this when
extending mana. It accepts multiple arguments but the first argument would be
the log/template string that is actually outputted. This first line is processed
by same function that you would also normally use in `console.log` so all the
`%s` and `%d` formatting still function as intended.

```js
MyAPI = mana.extend({
  initialize: function init(options) {
    this.debug('you and users can only see this when the DEBUG env variable is set');
    this.debug('%d digits and event %j json can be outputted', 1, { foo: 'bar' });
  }
});
```

### mana.args()

Parse arguments or supplied array and return an object with all arguments
classified by type and shorthand. It accepts one argument which would be the
`Arguments` that your function receives or an `Array`.

```js
mana.extend({
  customfunction: function (args) {
    args = this.args(arguments);

    console.log(args.str, args.fn, args.options, args.string);
  }
});
```

As you can see in the example above we also map a view values to a more sensible
name.

- `function` -> `fn`
- `object` -> `options`
- `string` -> `str`
- `number` -> `nr`

### mana.type()

Get accurate type information for the given JavaScript class.

```js
mana.extend({
  foo: function (bar) {
    if (this.type(bar) === 'array') doarraystuff();
    else dosomethingelse();
  }
});
```

### mana.send()

This the method what it's all about. This is the part where you actually start
sending a request to the specified URL. This method accepts multiple arguments.

- `string`: This is the path that you want to request on the URL.
- `array`: If you didn't supply a string, you need to supply an Array with
  paths. This array will be filtered with `Boolean()` and then joined with `/`
  to generate a path. In some cases this much more readable then doing a lot of
  concatenations manually in your code to produce a string.
- `function`: The actual callback function that should receive the results. We
  assume that this callback follows the error first pattern.

In addition to these arguments we also accept an options object/argument which
is used to fully configure the way we send requests. It supports a ton of
options.

- `api`: An alternate API which will be used instead of `mana.api`. It should be
  a string.
- `method`: The HTTP method that should be used for the request. It defaults to
  `GET`>
- `timeout`: Alternate timeout which be used instead of `mana.timeout`.
- `strictSSL`: Should this we request with strictSSL? Defaults to `mana.stictSSL`.
- `headers`: An object of headers that need to be added to the request. The
  object should be formatted as key (header name) -> value (header value).
- `maxSockets` The maximum amount of sockets. Defaults to `mana.maxSockets`.
- `maxdelay` The maximum delay for the exponential back off. Defaults to
  `mana.maxdelay`.
- `mindelay` The minimum delay for the exponential back off. Defaults to
  `mana.mindelay`.
- `retries` The maximum of retries of the API call we can do. Defaults to
  `mana.retries`.
- `assign` A custom or pre-build [assign] instance that should be used to write
  the data against. This can be useful if you need to make multiple requests
  against various of API points but want to process it all at once with one
  callback.
- `factor` The exponential back off factor. Defaults to `mana.factor`.
- `params` Array or object with params that should be send to the server. If
  method is set to `GET` we will use `mana.querystring` and append it after the
  pathname. If the URL already has a query string, it will be merged and
  potentially overridden using these options. For all other HTTP methods we
  assume that this is the data should be send to the server and send transform
  it using `mana.json` to a JSON body.
- `next` A function which will control what happens next after the data has been
  received from the server. If nothing is provided we will automatically end the
  returned [assign] instance so the callback gets triggered. If you did supply
  this function it will be called with the following arguments:
  - `res` The HTTP response object
  - `assign` The assign instance we would have ended
  - `args` The compiled arguments object of the current `send` call.

In the options you've seen that we allow sending of custom headers. There are
some headers we will append by default if they are not specified:

- `User-Agent` We will use the format `<mana.name>/<mana.version> node/<process.version>`
  as user agent format.
- `Authorization` If not authorization header is present we will attempt to use
  the one that is set at `mana.authorization`
- `Accept` This will be set to `application/json`.

If cache has been specified on the `mana` object and a cache entry is also
available we will also add a `if-none-match` header with the value of the
returned `etag` during the time of the caching.

```js
mana.extend({
  afunction: function fn(id, callback) {
    var x = this.send(['path', id, 'action'], this.merge(body, {
      headers: {
        'x-foo-bar': 'foboar'
      }
    }), callback);

    //
    // As the `send` method returns an Assign instance we can do
    // map/reduce/filter operation on the returned dataset. This ensures that
    // the supplied callback gets a data structure that we want and not that the
    // API server returns.
    //
    x.map(function (row) {
      return {
        id: row.id,
        whatever: row.foo.bar
      }
    });
  }
})
```

### mana.view()

The view method allows you do to some basic requests against a CouchDB interface
it makes this a bit more easier to work with. It accepts 3 arguments, the order
of these arguments do not matter.

- `string`, **required**, The name of the CouchDB view you're requesting.
- `function`, **required** The callback function which follows an error first
  callback pattern.
- `object` Optional configuration:
  - `group_level`: The group level of the query, defaults to `3`
  - `key`: Will be set as `startkey` and `endkey` so you can search for results.
  - `descending`: Should the results that are returned be descending. Defaults
    to `false`.
  - `stale`: OK or `update_after` which is the default value.
  - `limit`: Limit the number of documents returned
  - `skip`: Skip the x number of documents

This function will return a [assign] instance which can be used to transform and
manipulate the data if needed.

```js
mana.extend({
  foo: function () {
    this.view('/foo', {
      key: 'known key',
      skip: 10
    }, function (err, data) {
    
    });
  }
});
```

### mana.fireforget()

**This is a private method, do not touch unless you feel adventurous.**

Simple wrapper around a possible cache interfaces. Both async and sync
interfaces are supported. We assume that the cache is stored on the mana
instance as `cache` property. This cache instance should have a minimum of 2
methods:

- `cache.get` Which receives a key of the cache it needs to retrieve. If 2
  arguments are accepted in the function we assume that the second argument is a
  callback and that it's executed asynchronously.
- `cache.set` Which stores the key and value. If 3 or more arguments are
  accepted we assume that the method is async and that the last supplied
  argument should be a callback.

### mana.downgrade()

**This is a private method, do not touch unless you feel adventurous.**

Downgrade the list of given mirrors so we can query against a different server
when our default api endpoint is down.

### mana.roll()

**This is a private method, do not touch unless you feel adventurous.**

This attempts to select a new token when the `remaining` api calls has been set
to 0. It filters all tokens to see which ones are available and most likely to
be reset or still have remaining API calls. The token with the highest remaining
API calls will be set returned first and set as `Authorization` header.

### mana.tokenizer()

**This is a private method, do not touch unless you feel adventurous.**

Transforms the `tokens` property in to an array of `mana.Token` instances. And
it removes all duplicates and potential undefined values.

### mana.all()

**This is a private method, do not touch unless you feel adventurous.**

Returns a function will will call queued functions for the given `urid`.

### mana.fetching()

**This is a private method, do not touch unless you feel adventurous.**

Check if we are already fetching the given request.

## Inherited

There are also a couple of methods that mana has because it inherits from
certain modules. The mana instance that you create is an `EventEmitter` not the
regular emitter from node but a high-performance variant of it. It's an
[EventEmitter3](https://github.com/3rd-Eden/eventemitter3). So all `emit`, `on`
and all other EventEmitter related methods are available for you to use and
abuse.

As the extending is done using the [fusing](https://github.com/bigpipe/fusing)
library it also inherits the following methods:

- [`Mana/this.readable('name', 'value')`](https://github.com/bigpipe/fusing#examplereadable)
- [`Mana/this.writable('name', 'value')`](https://github.com/bigpipe/fusing#examplewritable)
- [`Mana/this.get('name', 'value')`](https://github.com/bigpipe/fusing#exampleget)
- [`Mana/this.set('name', 'value')`](https://github.com/bigpipe/fusing#exampleset)
- [`this.merge(obj, obj2)`](https://github.com/bigpipe/predefine#predefinemerge)
- [`this.mixin(obj, obj2)`](https://github.com/bigpipe/predefine#predefinemixin)
- [`this.emits(event)`](https://github.com/bigpipe/fusing/blob/master/index.js#L154)

## Debugging

The are various of debug outputs available for this module. Debugging is using
the [diagnostics](https://github.com/3rd-Eden/diagnostics) module which will
read out the `DEBUG` and `DIAGNOSTICS` environment variables of your machine to
see if it needs to output debug information to STDOUT. 

This is also one of the reasons why the `name` property is required as this name
is used for the debug flags. If you've named API `example` you can output debug
information by starting your application using:

```
DEBUG=example* node <your-app-index.js>
```

Mana it self also have a few lines of debug output and they can be seen using:

```
DEBUG=mana node <your-app-index.js>
```

## Drinking the potion

The module assumes a simple pattern. The API end points are listed in a folder
called `endpoints`. This folder contains JavaScript files which exports
a function:

```js
function Endpoint(api) {
  this.api = api;
}

module.exports = Endpoint
```

This function receives a reference to your base API class once it's initialised.
These API endpoints will be introduced on the prototype of your base API in
lowercase. So if you name your file `Endpoints` it will create an
`base.endpoints` method for you which access this constructed function. Now the
beauty of this is that these methods support lazy construction. So only when you
access the `.endpoints` property, it will create a new instance (only once
of course). This way you don't construct pointless API points that might never be
used by your users. 

In addition to lowercasing your endpoint and introducing it as constructed
property it also exposes the Full class on the base API. This class is Uppercase
first, just like all Classes should be in JavaScript.

## License

MIT

[assign]: http://github.com/3rd-Eden/assign
