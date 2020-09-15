var Reconnect = require('./reconnect');
var extend = require('xtend/immutable');
var clearTimeout = require('timers').clearTimeout;

module.exports = Back;

//
// Takes a set of reconnect options defined in README
//
function Back(options) {
  if (!(this instanceof Back)) {
    return new Back(options);
  }

  this.settings = extend(options);
  this.reconnect = null;
}

Back.prototype.backoff = function backoff(cb) {
  this.reconnect = new Reconnect(cb, this.settings);
};

Back.prototype.close = function close() {
  if (this.reconnect && this.reconnect.timer) {
    return clearTimeout(this.reconnect.timer);
  }
}
