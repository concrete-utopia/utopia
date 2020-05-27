var path = require('path');
var extract = require('../extract');
var resolveEntries = require('../resolveEntries');
var bundle = require('../bundle');
var utils = require('../utils');
var exec = require('child_process').exec;
var fs = require('fs');
var verifyAvailability = require('./verifyAvailability');

function extractAndBundle (req, res) {
  var packages = req.params.packages.split('+');
  var packagePath = `packages/${utils.getHash(packages)}`;
  var currentTime = Date.now()

  verifyAvailability.isAvailable = false;
  console.log('Started - ' + packages.join(', ') + ' - ' + new Date(currentTime))
  return extract(packages, packagePath)
    .then(resolveEntries(packages, packagePath))
    .then(bundle(packagePath))
    .then(function respond () {
      return Promise.all([
        utils.readFile(path.resolve(packagePath, 'manifest.json')),
        utils.readFile(path.resolve(packagePath, 'dll.js'))
      ]);
    })
    .then(function (files) {
      console.log('Success - ' + utils.getDuration(currentTime)  + 's')
      currentTime = Date.now()

      res.send({
        manifest: files[0],
        dll: files[1]
      });
      verifyAvailability.isAvailable = true;

      exec(`rm -rf ${packagePath}`, function (err, stdout, stderr) {
        if (err) {
          console.log(err);
        }
        console.log('Cleaned - ' + utils.getDuration(currentTime)  + 's')
      })
    })
    .catch(function (error) {
      console.log('Error - ' + error.message + ' - ' + utils.getDuration(currentTime) + 's')
      console.log(error.stack);
      currentTime = Date.now()

      verifyAvailability.isAvailable = true;
      res.status(500).send({ error: error.message });

      var stats = fs.lstatSync(packagePath);
      if (stats.isDirectory()) {
        exec(`rm -rf ${packagePath}`, function (err, stdout, stderr) {
          if (err) {
            console.log(err);
          }
          console.log('Cleaned - ' + utils.getDuration(currentTime)  + 's')
        })
      }
    });
}

module.exports = extractAndBundle;
