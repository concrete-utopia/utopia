var config = require(`../configs/${process.env.WEBPACK_DLL_ENV}.json`);
var hash = require('string-hash');
var path = require('path');
var mime = require('mime');

module.exports = {
  isProduction: function () {
    return process.env.NODE_ENV === 'production';
  },
  getVendorsBundleName: function (packages) {
    return String(hash(packages));
  },
  logError: function (err) {
    console.log(err.message);
    console.log(err.stack);
  },
  sendFile: function (fileName, content) {
    var contentType = mime.lookup(fileName);
    var contentLength = content.length;


    return function (request) {
      try {
        request.send(200, content, {
          'Cache-Control': 'public, max-age=' + config.cacheMaxAge,
          'Content-Type': contentType,
          'Content-Length': contentLength
        });
      } catch (e) {}
    }
  },
  getPackagerName: function (packager) {
    return packager.url.replace('http://', '').replace(/\./g, '')
  }
};
