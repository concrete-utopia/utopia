# CHANGELOG

## v4.0.0 (2021-24-01)
### Breaking changes
  - Remove static method `noConflict()`.

### New features
- Add a new static method `createInstance()` as alternative way to create a new `PubSub` instance.
- Export library in UMD, ESM and CommonJS formats.

### Internal changes
- Refactor source code to ES2015+.
- Use rollup.js to bundle the library.
- Replace Mocha with Jest as testing framework.
- Improve tests and coverage.
- Replace Travis with Github Actions for CI.

## v3.6.2
- Update `devDependencies`
- Update CI configuration
- Delete examples folder

## v3.6.0
- Fix issue #4
- Add `immediateExceptions` option when creating instance


## v3.5.0
- Update `devDependencies`
- Use `mocha` and `chai` for testing instead of `karma` and `jasmine`
- Drop support Bower support
- Exclude `dist` folder from source control

## v3.4.0
- Add static method `PubSub.noConflict()` to roll back the global `PubSub` identifier. Used in a normal browser global namespace environment to avoid conflicts, etc.

## v3.3.0
- If there is no subscriber for a topic, delete topic property when unsubscribing. Used to leave it as an empty array before.
- The result of `subscribers` and `subscribersByTopic` methods is just a copy of the original object or array accordingly.
- Keep devDependencies up to date.

## v3.2.7
Allow passing multiple data arguments to `publish` and `publishSync` methods.
```js
var pubsub = new PubSub();

pubsub.subscribe('event', function (data) {
  console.log(data);
  // => Array [{fname: 'John'}, {lname: 'Doe'}, [1, 2, 3], 'Lorem ipsum dolor sit amet.']

  console.log(data[0]);
  // => Object {lname: 'John'}

  console.log(data[1]);
  // => Object {lname: 'Doe'}

  console.log(data[2]);
  // => Array [1, 2, 3]

  console.log(data[3]);
  // => String "Lorem ipsum dolor sit amet."
});

pubsub.publish('event', {fname: 'John'}, {lname: 'Doe'}, [1, 2, 3], 'Lorem ipsum dolor sit amet.');
```

## v3.2.6
- Ensure that listeners registered on the same topic are invoked in the order they are added.
- Minor updates on documentation.
- Update angular_1.x_example.

## v3.2.5
- Add working example using Angular 1.x.
- Update devDependencies.

## v3.2.4
- Improve tests and coverage

## v3.2.3
- Return a new instance of `PubSub` if it is invoked without the `new` keyword.
- Add code coverage.

## v3.2.2
- Keep devDependencies up to date

## v3.2.1
- Fix License

## v3.2.0
- Add public method `subscribersByTopic()` to get an array of subscribers for a specific topic.

## v3.1.0
- `hasSubscribers` checks if there is at least one subscriber, no matter its name, if no argument is passed.
- Add public method `subscribers()` to get a readonly object of the current subscribers.

## v3.0.0

### Breaking changes

The default API method aliases are deprecated and removed from v3.0.0 onwards. However there is a new method `alias` introduced, that allows to create your own aliases. Therefore, if you already use those aliases in a project you can use the `alias` method to provide your own.

Below is a map of the default aliases that existed prior to version 3.0.0:

| Original method  | Alias method  |
| ---------------  | ------------- |
| `subscribe`      | `on`          |
| `subscribeOnce`  | `once`        |
| `publishSync`    | `triggerSync` |
| `unsubscribe`    | `off`         |
| `hasSubscribers` | `has`         |

To create your own aliases:

```js
var pubsub = new PubSub().alias({
  subscribe: 'on',
  subscribeOnce: 'once',
  publish: 'trigger',
  publishSync: 'triggerSync',
  unsubscribe: 'off',
  hasSubscribers: 'has'
});
```

### Other updates

- Add public method `unsubscribeAll` to clear all subscriptions whatsoever.
- Add public method `alias` to create your own method aliases. (See above)
- Provide source-map for the minified library.

## v2.1.0
- Add support for publishing events synchronously using `publishSync` method.
- Add public method `hasSubscribers` to check if there are subscribers for a specific topic.

## v2.0.3
- Add support for Travis CI.
- Lint source code using ESLint.

## v2.0.2
- Keep devDependencies up to date.

## v2.0.0

### Breaking changes

- Reverse the arguments the `callback` function accepts, in order to allow the usage of `data` argument without the need to also specify the `topic` if not needed.
- Throw exception if `callback` is not a `function` or is not provided at all.

### Other updates
- Return token on `subscribeOnce` method.
- Correct annotations and provide examples.
- Update devDependencies.
- Provide `npm` scripts to run the tasks. No more need for global dependencies installed (Grunt).
