'use strict';

const jsdom = require('jsdom');

// Class to return a window instance.
// Accepts a jsdom config object.
// Config object must be cloned before passing through otherwise jsdom will add
// lots of properties to the original reference.
module.exports = class Window {
	constructor(jsdomConfig) {
		return jsdom.jsdom('', Object.assign({}, jsdomConfig)).defaultView;
	}
};
