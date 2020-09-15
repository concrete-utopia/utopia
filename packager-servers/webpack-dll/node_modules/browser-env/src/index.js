const Window = require('window');

// Default jsdom config.
// These settings must override any custom settings to make sure we can iterate
// over the window object.
const defaultJsdomConfig = {
  features: {
    FetchExternalResources: false,
    ProcessExternalResources: false
  }
};

// IIFE executed on import to return an array of global Node.js properties that
// conflict with global browser properties.
const protectedproperties = (() => Object
  .getOwnPropertyNames(new Window(defaultJsdomConfig))
  .filter(prop => typeof global[prop] !== 'undefined')
)();

// Sets up global browser environment
module.exports = function browserEnv() {

  // Extract options from args
  const args = Array.from(arguments);
  const properties = args.filter(arg => Array.isArray(arg))[0];
  const userJsdomConfig = args.filter(arg => !Array.isArray(arg))[0];

  // Create window object
  const window = new Window(Object.assign({}, userJsdomConfig, defaultJsdomConfig));

  // Get all global browser properties
  Object.getOwnPropertyNames(window)

    // Remove protected properties
    .filter(prop => protectedproperties.indexOf(prop) === -1)

    // If we're only applying specific required properties remove everything else
    .filter(prop => !(properties && properties.indexOf(prop) === -1))

    // Copy what's left to the Node.js global scope
    .forEach(prop => global[prop] = window[prop]);

  // Return reference to original window object
  return window;
};
