# Licenses

[![Build Status](https://travis-ci.org/3rd-Eden/licenses.png)](https://travis-ci.org/3rd-Eden/licenses)

Licenses.. This is the most painful part about Open Source. There are so many
different licenses and they all have different restrictions. In order to know
the license footprint of your project you need to know how your modules are
licensed. You might be interested in your license footprint because:

- Some licenses might restrict you from selling your code or using it for
  commercial applications.
- There are unlicensed modules released in to npm on a daily basis. Just
  because they are added in the npm registry it doesn't mean that they are Open
  Source and just free to use.
- The code could be proprietary licensed.
- .. and the list goes on and on.

But the biggest problem is figuring out which license a module is actually
using. There are a lot of ways of saying that your code is licensed under MIT.
There are people who rather say licensed under MIT than just stating MIT. So the
way we write which license we use differ but also the location of our licenses.
It can be in the `package.json` hiding in various of properties or specified in
the `README.md` of the project or even a dedicated `LICENSE` file in the
repository.

Now that you've taken the time to read about some of these issues above, you
know why this module exists. It tries to fulfill one simple task. Get a human
readable license from a given node module.

However, this module isn't flawless as it tries to automate a task that usually
requires the interference and intelligence of a human. If you have module that
is incorrectly detected or not detected at all but does have licensing
information publicly available please create an issue about and we'll see if it
can get resolved.

<!-- many thanks stranger <script>alert('thanks')</script> -->

## Installation

The module is released through npm and can therefor be installed using:

```
npm install --save licenses
```

## CLI

There is CLI version of this module available as `licensing` which can be
installed locally using:

```
npm install -g licensing
```

See https://github.com/3rd-Eden/licensing for more information.

## Getting started with the API

The module exposes one single interface for retrieving the packages, which is a
simple exported function:

```js
'use strict';

var licenses = require('licenses');

licenses('primus', function fetched(err, license) {
  console.log(license.join(',')); // MIT
});
```

As you can see in the example above, the first argument of the function can be a
`string` with the name of the package you want to resolve. In addition to
supplying a string you can also give it the contents of the npm registry's data
directly:

```js
licenses({ name: 'primus', readme: '..', ....}, function fetched(err, license) {

});
```

The function allows a second optional argument which allows you to configure
license function. The following options are supported:

- **githulk** A custom or pre-authorized
  [githulk](https://github.com/3rd-Eden/githulk) instance. The license lookup
  process makes extensive use of GitHub to retrieve license information that
  might not be available in the package.json. But the GitHub API is rate limited
  so if you don't use an authorized GitHulk instance you can only do 60 calls to
  the API.
- **order** The order in which we should attempt to resolve the license. This
  defaults to [[registry](#registry), [github](#github), [content](#content)].
- **registry** The URL of The npm Registry we should use to retrieve package
  data.
- *npmjs* a custom [npm-registry](https://github.com/3rd-Eden/npmjs) instance.

The options are completely optional and can therefore be safely omitted.

```js
licenses('primus', { registry: 'https://registry.npmjs.org/' }, function () {

});
```

As you might have noticed from the options we support three different lookup
algorithms:

### registry

In this algorithm we attempt to search for license information directly in the
supplied or retrieved npm data. This is the fastest lookup as it only needs to
search and parse the `license` and `licenses` fields of the module for license
information.

### github

This reads out your github repository information from the package data to get a
directly listing of your project. Once the directory is listed it fetches files
from the repo where a possible license or license information can be found like
README and LICENSE files. All the data that is found will be scanned with the
[content](#content) algorithm.

### content

It searches the readme or supplied content for matches the license files. If it
fails to do any matching based on the license files it fallback to a really
basic regexp based check.

### License

MIT
