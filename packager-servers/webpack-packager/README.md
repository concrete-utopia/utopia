# webpack-packager
[![Build Status](https://travis-ci.org/cerebral/webpack-packager.svg?branch=master)](https://travis-ci.org/cerebral/webpack-packager)

A service that packages DLL bundles and manifests. We use this to bundle `npm` packages to be used as 3rd party components in the utopia app.

# How to run it locally

1. Install `yarn`
2. Install the dependencies: `npm install`
3. Create a package folder: `mkdir packages`
4. Run the server: `npm start`

# API usage examples

Load the latest bundle for react:

`http://localhost:5500/react@*/`

Load the bundle for react version 15.5.3:

`http://localhost:5500/react@15.5.3/`

You can load one bundle generated from  multiple packages and their dependencies by concatenating the packages with `+`, e.g.:

`http://localhost:5500/react@15.5.3+react-dom@15.5.3/`

# Please access this through `webpack-dll`

This server is not reliable by itself, it crashes if you try to load the same bundle multiple times simultaneously. Please use `webpack-dll` to access, that takes care of caching and preventing simultaneous access.