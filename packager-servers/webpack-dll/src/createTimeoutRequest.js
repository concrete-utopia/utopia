var config = require(`../configs/${process.env.WEBPACK_DLL_ENV}.json`);
var errors = require('./errors');

function createTimeoutRequest (res, queue) {
  const started = Date.now()
  const timeout = setTimeout(function () {
    queue.splice(queue.indexOf(res), 1)
    res.status(504).send(errors.STILL_PENDING)
  }, config.connectTimeout);

  return {
    send(status, data, headers) {
      clearTimeout(timeout)
      headers = headers || {}
      Object.keys(headers).forEach(function (header) {
        res.setHeader(header, headers[header]);
      });
      res.status(status).send(data);
    }
  }
}

module.exports = createTimeoutRequest;
