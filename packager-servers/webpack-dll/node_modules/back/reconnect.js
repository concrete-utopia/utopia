
var Reconnect = module.exports = function reconnect(callback, opts) {
  if (!(this instanceof Reconnect)) {
    return new Reconnect(callback, opts);
  }

  opts = opts || {};

  if (opts.backoff) return;

  opts.maxDelay = opts.maxDelay || Infinity;                    // Maximum delay.
  opts.minDelay = opts.minDelay || 500;                         // Minimum delay.
  opts.retries = (opts.retries === 0 ? 0 : opts.retries) || 10; // Amount of allowed retries.
  opts.attempt = (+opts.attempt || 0) + 1;                      // Current attempt.
  opts.factor = opts.factor || 2;                               // Back off factor.

  // Bailout if we are about to make to much attempts. Please note that we use ...
  if (opts.attempt > opts.retries) {
    return callback(new Error('Unable to retry'), opts);
  }

  // Prevent duplicate back off attempts.
  opts.backoff = true;

  //
  // Calculate the timeout, but make it randomly so we don't retry connections
  // at the same interval and defeat the purpose. This exponential back off is
  // based on the work of:
  //
  // http://dthain.blogspot.nl/2009/02/exponential-backoff-in-distributed.html
  //
  opts.timeout = opts.attempt !== 1
    ? Math.min(Math.round(
        (Math.random() + 1) * opts.minDelay * Math.pow(opts.factor, opts.attempt)
      ), opts.maxDelay)
    : opts.minDelay;

  this.timer = setTimeout(function delay() {
    opts.backoff = false;
    clearTimeout(this.timer);

    callback(undefined, opts);
  }.bind(this), opts.timeout);

};
