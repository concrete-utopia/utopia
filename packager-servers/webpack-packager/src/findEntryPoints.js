var path = require('path');
var utils = require('./utils');

function isValidFile (file, filePath, packageName) {
  return (
    (path.extname(file) === '.js' || path.extname(file) === '.css' || path.basename(file) === 'package.json' || path.basename(file).endsWith('.d.ts')) &&
    filePath.indexOf(packageName + '/index.js') === -1 &&
    file[0] !== '_' &&
    file !== 'gulpfile.js' &&
    !file.match(/webpack/) &&
    !(file.substr(0, packageName.length) === packageName && path.extname(file) === '.js') &&
    !utils.isPrebundledFile(file) &&
    file.indexOf('.test.js') === -1 &&
    file.indexOf('-test.js') === -1 &&
    file.indexOf('.spec.js') === -1 &&
    file.indexOf('-spec.js') === -1
  );
}

var invalidDirs = [
  'demo',
  'docs',
  'benchmark',
  'es6',
  'es',
  'src',
  'bundles',
  'examples',
  'scripts',
  'tests',
  'test',
  'testing',
  'min',
  'node_modules',
  'flow-typed'
];

function isValidDir (dir, dirOverride) {
  return (
    path.join('/', dir) === path.join('/', dirOverride) ||
    invalidDirs.indexOf(dir) === -1
  )
}

module.exports = function readPackage (packageName, filePath, dirOverride, blackListedEntries) {
  return utils.readDir(filePath)
    .then(function (dir) {
      return Promise.all(dir.map(function (fileOrDir) {
        var currentPath = path.join(filePath, fileOrDir);

        if (blackListedEntries.indexOf(currentPath) >= 0) {
          return;
        } else {
          return utils.stat(currentPath)
            .then(function (fileStat) {
              if (fileStat.isDirectory() && isValidDir(fileOrDir, dirOverride)) {
                return readPackage(packageName, currentPath, dirOverride, blackListedEntries);
              } else if (!fileStat.isDirectory() && isValidFile(fileOrDir, currentPath, packageName)) {
                return currentPath;
              }
            });
        }
      }))
        .then(function (hits) {
          return hits.reduce(function (entryPoints, hit) {
            if (hit) {
              return entryPoints.concat(hit);
            }

            return entryPoints;
          }, []);
        })
    })
}
