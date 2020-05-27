var path = require('path');
var utils = require('./utils');

module.exports = function extractPackageJsonPaths(entries, packagePath) {
  var entryKeys = Object.keys(entries).map(function (entryKey) {
    return path.resolve(packagePath, 'node_modules', entryKey)
  });

  return function (vendors) {
    var results = vendors.reduce(function (result, vendor) {
      if (path.basename(vendor) === 'package.json' && entryKeys.indexOf(path.dirname(vendor)) === -1) {
        result.packageJsons.push(vendor)
      }
      result.vendors.push(vendor)

      return result
    }, {
      vendors: [],
      packageJsons: []
    })

    return Promise.all(results.packageJsons.map(function (packageJson) {
      return utils.readFile(packageJson)
        .then(function (packageJsonContent) {
          var parsedPackageJson = JSON.parse(packageJsonContent)

          if (!parsedPackageJson.module || !parsedPackageJson.main) {
            return null
          }

          return {
            path: path.dirname(packageJson.replace(path.resolve(packagePath, 'node_modules') + '/', '')),
            main: path.resolve(path.dirname(packageJson), parsedPackageJson.module || parsedPackageJson.main).replace(path.resolve(packagePath, 'node_modules') + '/', '')
          }
        })
    }))
      .then(function (packageJsonResults) {
        return {
          vendors: results.vendors,
          packageJsons: packageJsonResults.filter(function (result) {
            return Boolean(result)
          })
        }
      })
  }
}
