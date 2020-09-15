# npm-registry

To keep myself sane while working with The npm Registry I decided to write my
own library to deal with all the incomplete, inconsistent and horrible data
structures that are available in The npm Registry. NoSQL is nice and all, but
that doesn't mean you should leave your data unmaintained. This library is never
meant as a full replacement of the `npm-registry-client` which the `npm` bin
file is using. Unless those API's and methods are so poorly implemented or
designed that I get a mental breakdown, then yes, this will become a full and
usable replacement of the above said module.

This module is written with high availability in mind. The main reason behind
this is that npm Inc. has added a lot of moving parts on top of the registry
which frequently breaks. In order to combat this I've implemented automatic
downgrading to multiple registries. If all other supplied registries fail to
work an automatic exponential randomized back off algorithm kicks in place and
retries the query once more. This functionality is all provided by the awesome
[mana] package which provides core functionality for writing sane api-clients.

## Installation

```
npm install --save npm-registry
```

And that is all you need to type in your terminal in order to prevent becoming
terminal. The `--save` tells `npm` to automatically add the package and latest
version to your `package.json`.

## Getting started

Now that you've installed the `npm-registry` module you can require and
initialize it using:

```js
'use strict';

var Registry = require('npm-registry');

var npm = new Registry({ options });
```

As seen in the example above, the `Registry` constructor allows an `Object` with
options to customize the npm registry client. The following options are supported:

- `registry` The URL of the npm registry. Defaults to Nodejitsu's mirror.
- `stats` URL of the download stats service. Defaults to npm's API server.
- `mirrors` Array of mirrors to use when a registry is down.
- `maxdelay` Maximum delay for exponential back off.
- `mindelay` Minimum delay for exponential back off.
- `githulk` Reference to a pre-configured [GitHulk] instance.
- `retries` The amount of retries we should do before giving up.
- `factor` Exponential backoff factor.
- `authorization` Optional authorization header for authorized requests.
- `user,password` Optional user/password for authorized requests.

The fully configured npm registry client can then be used to access the various
of API endpoints using:

```js
//
// npm.<endpoint>.<method>(<arg>, <callback>);
//
npm.packages.get('npm-registry', function (err, data) {
  ..
});
```

The following endpoints are available:

### Packages

The `.packages` endpoints allows you to retrieve detailed information about npm
packages. The following methods are implemented:

- [npm.packages.get](#npmpackagesget)
- [npm.packages.details](#npmpackagesdetails)
- [npm.packages.depended](#npmpackagesdepended)
- [npm.packages.starred](#npmpackagesstarred)
- [npm.packages.keyword](#npmpackageskeyword)
- [npm.packages.releases](#npmpackagesreleases)
- [npm.packages.release](#npmpackagesrelease)
- [npm.packages.range](#npmpackagesrange)

#### npm.packages.get

Get information from the npm package. If the name contains an `@` char we assume
that the user wants to get a specific version instead.

Example: **primus@0.1.1 would retrieve primus version 0.1.1**

```js
npm.packages.get('primus', function (err, data) {

});
```

#### npm.packages.details
 
Retrieve additional details for the package information. This a lot slower than
a simple `.get` but much more detailed and accurate as it uses custom parsers
for accurate licensing information. Which could require a fair amount of npm and
github lookups.

```js
npm.packages.details('memcached', function (err, data) {

});
```

#### npm.packages.depended

Get all packages that are depended upon a given package name.

```js
npm.packages.depended('eventemitter3', function (err, depended) {

});
```

#### npm.packages.starred

Find out which users have starred the given package.

```js
npm.packages.starred('npm-registry', function (err, starred) {

});
```

#### npm.packages.keyword
 
Find all packages that matches the giving keywords.

```js
npm.packages.keyword('primus');
```
 
#### npm.packages.releases

Retrieve all release specific information for the given package name. Please
note that this uses the `npm.packages.details` call under the hood to provide
more detailed information but it will therefor also take longer.

```js
npm.packages.releases('bigpipe', function (err, releases) {

});
```

#### npm.packages.release

Get a specific release of a package. Please note that this uses the
`npm.packages.details` call under the hood to provide more detailed information
but it will therefor also take longer.

```js
npm.packages.release('npm-registry', '0.0.2', function (err, release) {

});
```

#### npm.packages.range

Get a release that is satisfying a given semver range. Please note that this
uses the `npm.packages.details` call under the hood to provide more detailed
information but it will therefor also take longer.

```js
npm.packages.release('npm-registry', '^0.1.2', function (err, release) {

});
```

### Users

The `.users` endpoint allows you to retrieve detailed information about a given
npm account. The following methods are implemented:

- [npm.users.add](#npmusersadd)
- [npm.users.create](#npmuserscreate)
- [npm.users.update](#npmusersupdate)
- [npm.users.list](#npmuserslist)
- [npm.users.starred](#npmusersstarred)
- [npm.users.get](#npmusersget)
- [npm.users.sync](#npmuserssync)

#### npm.users.add

Add a user as maintainer of a package.

```js
npm.users.add('foobar', 'npm-registry', function (err) {

});
```

#### npm.users.create

Create a new npm account.

```js
npm.users.create('foobar', 'foo@bar.com', 'secretpassword', function (err) {

});
```

#### npm.users.update

Update the users.

```js
npm.users.update('foobar', {
  twitter: 'foobar',
  email: 'foo@foobar.bar'
}, function (err) {

});
```

#### npm.users.list

List all packages that the user maintains.

```js
npm.users.list('foobar', function (err, modules) {

});
```

#### npm.users.starred

Get all packages that the user has starred.

```js
npm.users.starred('foobar', function (err, modules) {
  
});
```

#### npm.users.get

Get profile information for a given user.

```js
npm.users.get('foobar', function (err) {

});
```

#### npm.users.sync

Sync ownership of npm modules with another account. This is useful if you have
one base owner of modules like a corporate account and you want to on-board a
new user.

```js
npm.users.sync('source-account', 'foobar', function (err) {

});
```

### Downloads

The `.downloads` endpoint allows you to retrieve download stats for a given
package. The following methods are implemented:

- [npm.downloads.totals](#npmdownloadstotals)
- [npm.downloads.range](#npmdownloadsrange)

#### npm.downloads.totals
 
Get the total amount of downloads for a given period. If no package name has
been supplied the total of all packages will be returned. The following date ranges
are accepted by the stats server.

- All packages, last day:
- last day: `last-day`
- specific date: `2014-02-01`
- last week: `last-week
- range of date (30 days max): `2014-02-01:2014-02-08`
- last month: `last-month`A
- specific month: `2014-01-01:2014-01-31`

```js
npm.downloads.totals('last-week', 'npm-registry', function (err, stats) {

});
```

#### npm.downloads.range

Same as above, but it doesn't get the total/summary of the downloads but an array
with the downloads per day. The same date ranges are allowed and if no package name
is supplied, all packages is assumed. 

```js
npm.downloads.range('last-week', 'npm-registry', function (err, stats) {

});
```

## Normalization

As the internal data structure is do damn awkward and unmaintained in npm we
need to normalize the data structures before we can even try to use it. While
this normalization is part automatically done for you internally there might be
use cases where you want to manually normalize a given dataset. The normalize
module can be required directly using:

```js
var normalize = require('npm-registry/normalize');
```

The `normalize` variable now contains two different functions, `users` and
`packages`. As you might have guessed, these functions normalize different data
structures. The function accepts a simple single argument which is the data
object that you receive from the npm registry endpoints.

```js
data = normalize.packages(data);
```

## License

MIT

[mana]: http://github.com/3rd-Eden/mana
[Githulk]: http://github.com/3rd-Eden/githulk
