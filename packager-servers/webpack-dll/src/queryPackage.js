const request = require('request');
const registryUrl = require('registry-url');

module.exports = function queryPackage(req, res) {
  var nameSplit = req.params.packageName.split('@');

  // If leading @
  if (!nameSplit[0]) {
    nameSplit.shift();
    nameSplit[0] = '@' + encodeURIComponent(nameSplit[0]);
  }

  var name = nameSplit[0];
  var version = nameSplit[1];

  new Promise(function (resolve, reject) {
    request(registryUrl() + name, function (err, response, body) {
      if (err || response.statusCode < 200 || response.statusCode >= 300) {
        return res.sendStatus(404);
      }

      try {
        var package = JSON.parse(body);
      } catch (e) {
        return reject();
      }

      resolve(package);
    });
  })
    .then(function (package) {
      var packageVersion

      if (version) {
        if (package['dist-tags'][version]) {
          packageVersion = package['dist-tags'][version]
        } else if (package.versions[version]) {
          packageVersion = version
        } else {
          throw new Error('Version not valid')
        }
      } else {
        packageVersion = package['dist-tags'].latest
      }

      res.send({
        name: package.name,
        version: packageVersion
      });
    })
    .catch(function (err) {
      res.sendStatus(404);
    });
}
