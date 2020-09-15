# is-windows-bash

Exports a function that returns true if the script is running in a Windows Bash environment (WSL) and false otherwise.

## Usage

```
$ npm install is-windows-bash --save
```

```javascript
const isWindowsBash = require('is-windows-bash');

if (isWindowsBash()) {
  console.log('Welcome to the future');
}
```
