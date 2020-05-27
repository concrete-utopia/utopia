var path = require('path');
var utils = require('./utils');

module.exports = function resolveEntries (packages, packagePath) {
  return function () {
    return Promise.all(packages.map(function (pkg) {
      var packageName = utils.getPackageName(pkg);

      return utils.readFile(path.resolve(packagePath, 'node_modules', packageName, 'package.json'))
        .then((result) => JSON.parse(result));
    }))
      .then(function (results) {
        return results.reduce(function (entriesPromise, packageJson) {
          var main = utils.evaluateEntry(packageJson.main);
          var browser = utils.evaluateEntry(packageJson.browser);
          var module = utils.evaluateEntry(packageJson.module);
          var unpkg = utils.evaluateEntry(packageJson.unpkg);
          var bin = utils.evaluateEntry(packageJson.bin);
          var entryList = [
            utils.evaluateEntry(packageJson.unpkg),
            utils.evaluateEntry(packageJson.browser),
            utils.evaluateEntry(packageJson.module),
            utils.evaluateEntry(packageJson.main),
            utils.evaluateEntry(packageJson.bin)
          ].filter(function (entry) {
            return Boolean(entry);
          }).sort(function (entryA, entryB) {
            if (utils.isPrebundledFile(entryA) && !utils.isPrebundledFile(entryB)) {
              return 1;
            } else if (!utils.isPrebundledFile(entryA) && utils.isPrebundledFile(entryB)) {
              return -1;
            }

            return 0;
          })
          var emptyModulesAliases = [];

          var mainEntry = entryList.shift()
          var map = typeof packageJson.browser === 'object' && Object.keys(packageJson.browser).reduce((currentMap, key) => {
            var entry = path.resolve(packagePath, 'node_modules', packageJson.name, key);
            
            // If one of the mappings matches main entry, override it, as we do not
            // want to bundle whatever is actually on main (angoliasearch). We create
            // a reverse mapping with a DOT in front to make it easier to work with in manifest
            if (packageJson.browser[key] === false) {
              emptyModulesAliases.push(key);
            } else {
              if (mainEntry && entry === path.resolve(packagePath, 'node_modules', packageJson.name, mainEntry)) {
                mainEntry = packageJson.browser[key];
              } else {
                var mapping = path.resolve(packagePath, 'node_modules', packageJson.name, packageJson.browser[key]);
                currentMap['.' + mapping] = entry;
              }
            }

            return currentMap;
          }, {});

          if (mainEntry && path.extname(mainEntry)) {
            return entriesPromise.then(function (entries) {
              return Object.assign(entries, {
                [packageJson.name]: {
                  main: mainEntry,
                  other: entryList,
                  map: map,
                  emptyModulesAliases: emptyModulesAliases
                }
              });
            });
          } else if (mainEntry) {
            return utils.stat(path.resolve(packagePath, 'node_modules', packageJson.name, mainEntry + '.js'))
              .then(function () {
                return entriesPromise.then(function (entries) {
                  return Object.assign(entries, {
                    [packageJson.name]: {
                      main: mainEntry + '.js',
                      other: entryList,
                      map: map,
                      emptyModulesAliases: emptyModulesAliases
                    }
                  });
                });
              })
              .catch(function () {
                return utils.stat(path.resolve(packagePath, 'node_modules', packageJson.name, mainEntry, 'index.js'))
                  .then(() => {
                    return entriesPromise.then(function (entries) {
                      return Object.assign(entries, {
                        [packageJson.name]: {
                          main: path.join(mainEntry, 'index.js'),
                          other: entryList,
                          map: map,
                          emptyModulesAliases: emptyModulesAliases
                        }
                      });
                    });
                  })
                  .catch(() => {
                    return entriesPromise.then(function (entries) {
                      return Object.assign(entries, {
                        [packageJson.name]: {
                          main: mainEntry,
                          other: entryList,
                          map: map,
                          emptyModulesAliases: emptyModulesAliases
                        }
                      });
                    });
                  })
              })
          } else {
            return utils.stat(path.resolve(packagePath, 'node_modules', packageJson.name, 'index.js'))
              .then(() => {
                return entriesPromise.then(function (entries) {
                  return Object.assign(entries, {
                    [packageJson.name]: {
                      main: './index.js',
                      other: entryList,
                      map: map,
                      emptyModulesAliases: emptyModulesAliases
                    }
                  });
                });
              })
              .catch(() => {
                return entriesPromise.then(function (entries) {
                  return Object.assign(entries, {
                    [packageJson.name]: {
                      main: null,
                      other: entryList,
                      map: map,
                      emptyModulesAliases: emptyModulesAliases
                    }
                  });
                });
              })
          }
        }, Promise.resolve({}))
      });
  }
}
