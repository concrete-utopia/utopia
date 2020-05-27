var fs = require('fs');
var hash = require('string-hash');
var path = require('path');

module.exports = {
  getHash: function (packages) {
    if (!packages || Object.keys(packages).length === 0) {
      return null;
    }
    var packagesList = Object.keys(packages).map(function (key) {
      return key + ':' + packages[key];
    }).sort(function (a, b) {
      if (a < b) return -1;
      if (a > b) return 1;
      return 0;
    });
    return String(hash(JSON.stringify(packagesList)));
  },
  readFile: function (path) {
    return new Promise(function (resolve, reject) {
      fs.readFile(path, 'utf-8', function (error, content) {
          if (error) {
            reject(error);
          } else {
            resolve(content);
          }
      });
    });
  },
  writeFile: function (path, content) {
    return new Promise(function (resolve, reject) {
      fs.writeFile(path, content, function (error) {
          if (error) {
            reject(error);
          } else {
            resolve();
          }
      });
    });
  },
  readDir: function (path) {
    return new Promise(function (resolve, reject) {
      fs.readdir(path, function (error, dir) {
          if (error) {
            reject(error);
          } else {
            resolve(dir);
          }
      });
    });
  },
  stat: function (path) {
    return new Promise(function (resolve, reject) {
      fs.stat(path, function (error, stat) {
          if (error) {
            reject(error);
          } else {
            resolve(stat);
          }
      });
    });
  },
  isPrebundledFile: function (file) {
    if (typeof file !== 'string') {
      return false
    }

    return (
      file.match(/.min.js$/) ||
      file.match(/\-min.js$/) ||
      file.match(/.umd.js$/) ||
      file.match(/.common.js$/) ||
      file.match(/.es.js$/) ||
      file.match(/.es6.js$/) ||
      file.match(/.bundle.js$/)
    )
  },
  cleanManifestContent: function (manifest, entries, packagePath) {
    var entryKeys = Object.keys(entries);
    var entryPaths = entryKeys.reduce(function (currentEntryPaths, entryKey) {
      return currentEntryPaths.concat(path.join(entryKey, entries[entryKey].fallbackDir == null ? entries[entryKey].main : entries[entryKey].fallbackDir));
    }, []);
    var projectPath = path.resolve();

    // Browser mappings
    var mappings = entryKeys.reduce(function (currentMappings, entryKey) {
      return Object.assign(currentMappings, entries[entryKey].map || {});
    }, {});

    return Object.keys(manifest.content).reduce(function (currentManifest, key) {
      var entryMatchIndex = entryPaths.reduce(function (matchIndex, entryPath, index) {
        if (key === '.' + path.join(projectPath, packagePath, 'node_modules', entryPath)) {
          return index;
        }

        return matchIndex;
      }, -1);

      var pathKey = key.replace(projectPath, '');

      if (entryMatchIndex >= 0) {
        pathKey = './' + path.join(packagePath, 'node_modules', entryKeys[entryMatchIndex]);
      }

      currentManifest[pathKey.replace(packagePath + '/', '')] = manifest.content[key].id;

      if (mappings[key]) {
        currentManifest[mappings[key].replace(projectPath, '').replace(packagePath + '/', '')] = manifest.content[key].id;
      }

      return currentManifest;
    }, {});
  },
  createExternals: function (manifest, packageJsons, entries) {
    var externalsResult = Object.keys(manifest.content).reduce(function (externals, manifestKey, index) {
      var directPath = manifestKey.substr(2).split('/').slice(1).join('/');
      var entry = directPath.split('/')[0];
      var fileName = path.basename(directPath)
      var extName = path.extname(directPath)
      var baseName = path.basename(fileName, extName);

      externals[directPath] = 'dll_bundle(' + manifest.content[manifestKey] + ')';
      externals[path.dirname(directPath) + '/' + baseName] = 'dll_bundle(' + manifest.content[manifestKey] + ')';

      if (!directPath.match(/node_modules/) && fileName === 'index.js') {
        externals[path.dirname(directPath)] = 'dll_bundle(' + manifest.content[manifestKey] + ')';
      }

      return externals;
    }, {});

    packageJsons.forEach(function (packageJson) {
      externalsResult[packageJson.path] = externalsResult[packageJson.main];
    })

    return externalsResult;
  },
  evaluateEntry: function (entry) {
    if (typeof entry === 'string') {
      return entry;
    }

    return null;
  },
  getPackageName: function (pkg) {
    const nameSplit = pkg.split('@');

    // Leading @
    if (!nameSplit[0]) {
      nameSplit.shift();

      return '@' + nameSplit[0];
    }

    return nameSplit[0];
  },
  isValidPackages: function (packages) {
    return packages.split('+').reduce(function (isValid, pkg) {
      if (pkg.indexOf('@') === -1) {
        return false
      }

      return isValid
    }, true)
  },
  getDuration: function (time) {
    return (Date.now() - time) / 1000
  }
}
