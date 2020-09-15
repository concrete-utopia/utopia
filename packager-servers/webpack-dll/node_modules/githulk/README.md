# Githulk

GitHulk is a developer friendly API client for the Github API. In addition to
providing access to Github's official API's it also implements new methods that
are either convenience functions or a composition of multiple API calls which will
make certain actions easier or more natural.

## Installation

This module is released in to the npm registry as `githulk`:

```
npm install --save githulk
```

## Usage

The GitHulk module is exposed as a single function (constructor) and can
therefor be required as:

```js
'use strict';

var GitHulk = require('githulk');
```

To initialise a new GitHulk instance simply construct it using:

```js
var githulk = new GitHulk(/* optional options */);
```

As you've might have noticed from the function call above, it accepts an
optional options object. The following options can be provided:

- `url`: The API endpoint of the Github API (with trailing slash).
- `maxdelay`: Maximum delay for exponential back off.
- `mindelay`: Minimum delay for exponential back off.
- `retries`: Amount of retries before finally giving up.
- `factor`: Exponential back off factor.
- `cache`: A cache for conditional lookups.
- `username`: A Github username (see Authorization)
- `password`: A Github password (see Authorization)
- `token`: A oauth token (see Authorization)

### Authorization

If no options are supplied your API calls will be severely limited by GitHub and
a really low rate limiting of 60 API calls per hour will be in place. In order
to authorize the GitHulk instance you can provide it with:

- `token`: An oauth token for your application.
- `user`, `password`: The user and password of your account (for basic auth).
- `authorization`: A predefined authorization header for each request.

In addition to supplying your authorization details through the constructor of
the function we also support them through `ENV` variables. Set the
`GITHULK_TOKEN` or `GITHUB_TOKEN` variable with your oauth token and we will use
that automaticaly.

### Arguments

All API endpoints follow the same function signature, the order in which they
are provided does not matter. The only argument that is optional is the
`options` argument.

- `project`: An **string** that has the username/repo combination.
- `options`: An **object** with additional configuration for the API method.
- `fn`: An **function** which is called with an error first callback pattern.

The following API endpoints are implemented, they are currently implemented on
a need to have basis:

### .repository

- `githulk.repository.contents`
- `githulk.repository.readme`
- `githulk.repository.raw`
- `githulk.repository.moved`

## License

MIT
