# webpack-dll-service
A service that converts a package into a DLL bundle and manifest. It uses the `webpack-packager` service to retrieve and create  the bundle, and caches the results in mongodb. It also protects `webpack-packager` from multiple simultaneous requests

## How to run it locally?

- Install MongoDB if you don't have it yet (on OS X e.g. `brew install mongodb`)
- Create a directory for the cached data, e.g. `mkdir db`
- Run mongodb using the create the directory, e.g. `mongod --dbpath db`
- Install the dependencies: `npm install`
- Launch the server: `npm start`

## API usage examples

Load the status page:

`http://localhost:5001/`

Load the dll bundle for the latest react:

`http://localhost:5001/v2/react@*/dll.js`

Load the manifest for the latest react:

`http://localhost:5001/v2/react@*/manifest.json`

Load the dll bundle and the manifest for react version 15.5.3:

`http://localhost:5001/v2/react@15.5.3/dll.js`
`http://localhost:5001/v2/react@15.5.3/manifest.json`

You can load one bundle generated from  multiple packages and their dependencies by concatenating the packages with `+`, e.g.:

`http://localhost:5001/v2/react@15.5.3+react-dom@15.5.3/dll.js`
