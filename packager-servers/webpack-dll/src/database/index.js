var config = require(`../../configs/${process.env.WEBPACK_DLL_ENV}.json`);
var db = require('./mongodb.js');
var mime = require('mime');
var path = require('path');
var Readable = require('stream').Readable;

module.exports = {
  connect: db.connect,
  saveFile: function (bundleName, fileName, content) {
    var stream = new Readable();
    stream.push(content);
    stream.push(null);

    return db.writeFile(bundleName + '_' + fileName, stream);
  },
  getFile: function (bundleName, fileName, res) {
    res.set({
      'Content-Type': mime.lookup(fileName),
      'Cache-Control': 'public, max-age=' + config.cacheMaxAge
    })

    return db.readFile(bundleName + '_' + fileName, res);
  },
  fileExists: function (bundleName, fileName) {
    return db.fileExists(bundleName + '_' + fileName);
  },
  getStats: function () {
    return db.find('stats');
  },
  updateStats: function (stats) {
    return db.update('stats', {
      name: 'total'
    }, {
      name: 'total',
      stats: stats
    });
  }
}
