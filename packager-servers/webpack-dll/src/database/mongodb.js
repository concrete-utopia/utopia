var mongo = require('mongodb');
var MongoClient = mongo.MongoClient;
var Grid = require('gridfs-stream');
var db = null;
var gfs = null;

module.exports = {
  connect: function (url) {
    return new Promise(function (resolve, reject) {
      MongoClient.connect(url, function(err, connectedDb) {
        if (err) {
          return reject(err);
        }
        gfs = new Grid(connectedDb, mongo);
        db = connectedDb;
        resolve(db);
      });
    })
  },
  find: function (collection, query) {
    return new Promise(function (resolve, reject) {
      db.collection(collection).find(query || {}).toArray(function (err, docs) {
        if (err) {
          reject(err);
          return;
        }
        resolve(docs);
      })
    })
  },
  update: function (collection, query, doc) {
    return new Promise(function (resolve, reject) {
      db.collection(collection).update(query, doc, {
        upsert: true
      }, function (err) {
        if (err) {
          reject(err);
          return;
        }
        resolve();
      })
    })
  },
  writeFile: function (fileName, readStream) {
    return new Promise(function (resolve, reject) {
      var writeStream = gfs.createWriteStream({
        filename: fileName
      });
      writeStream.on('finish', function (err) {
        if (err) {
          return reject(err);
        }
        resolve();
      })
      readStream.pipe(writeStream);
    });
  },
  readFile: function (fileName, writeStream) {
    return new Promise(function (resolve, reject) {
      var readStream = gfs.createReadStream({
        filename: fileName
      });
      readStream.on('close', function (err) {
        if (err) {
          return reject(err);
        }
        resolve();
      })
      readStream.pipe(writeStream);
    });
  },
  fileExists: function (fileName) {
    return new Promise(function (resolve, reject) {
      gfs.exist({filename: fileName}, function (err, found) {
        if (err) {
          reject(err);
        }
        resolve(found);
      });
    })
  }
};
