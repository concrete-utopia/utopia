# How to patch React for non-minified error messages in production

We have a patched version of react and react-dom. The reason for patching is that even in the production build of React we want to preserve the full printed error messages.
React by default ships with minified errors in production to save download size for production websites, we on the other hand want to ship production-speed React with easy to read error messages, and it's not a problem for us that the package is a bit larger.

The patches for version [18.1.0](https://github.com/facebook/react/releases/tag/v18.1.0) were generated based on the commit [22edb9f777](https://github.com/facebook/react/tree/22edb9f777d27369fd2c1fad378f74e237b6dfd3) and by commenting out [this line:](https://github.com/facebook/react/blob/22edb9f777d27369fd2c1fad378f74e237b6dfd3/scripts/rollup/build.js#L178)

```javascript
if (!isDevelopment && bundle.minifyWithProdErrorCodes !== false) {
  options.plugins.push(require('../error-codes/transform-error-messages'))
}
```

By the time we want to bump react next time, the build script will probably shift a bit, the point is to find the place which minifies the error messages.

The build script I ran was `yarn build react/index,react/jsx,react-dom/index,scheduler --type=NODE`, based on advice at [How to Contribute:](https://reactjs.org/docs/how-to-contribute.html#development-workflow)

> If your project uses React from npm, you may delete react and react-dom in its dependencies and use yarn link to point them to your local build folder. Note that instead of --type=UMD you’ll want to pass --type=NODE when building.
