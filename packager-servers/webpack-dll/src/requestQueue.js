var config = require(`../configs/${process.env.WEBPACK_DLL_ENV}.json`);
var path = require('path');
var queue = {};
var utils = require('./utils');
var request = require('request');
var errors = require('./errors');
var createTimeoutRequest = require('./createTimeoutRequest');

var packagerUpdateListeners = [];
var packagers = config.packagerServiceUrls.map(function (packageServiceUrl) {
  return {
    url: packageServiceUrl,
    isAvailable: true,
    lastUsed: Date.now(),
    resolvedCount: 0,
    errorCount: 0,
    isBusyCount: 0,
    timeoutCount: 0
  }
});

function emitPackagersUpdate () {
  packagerUpdateListeners.forEach(function (cb) {
    cb();
  });
}

module.exports = {
  listenToPackageUpdates: function (cb) {
    packagerUpdateListeners.push(cb);
  },
  getPackagers: function () {
    return packagers;
  },
  updatePackagersWithStats: function (stats) {
    packagers.forEach(function (packager) {
      var packagerName = utils.getPackagerName(packager)

      if (!stats[packagerName]) {
        return;
      }

      packager.lastUsed = stats[packagerName].lastUsed;
      packager.resolvedCount = stats[packagerName].resolvedCount || 0;
      packager.errorCount = stats[packagerName].errorCount || 0;
      packager.isBusyCount = stats[packagerName].isBusyCount || 0;
      packager.timeoutCount = stats[packagerName].timeoutCount || 0;
    });
  },
  add: function (id, packages, file, res) {
    if (queue[id])  {
      const timeoutRequest = createTimeoutRequest(res, queue[id][file])

      queue[id][file].push(timeoutRequest);
    } else {

      queue[id] = {packages: packages, 'dll.js': [], 'manifest.json': [], bundle: null};

      const timeoutRequest = createTimeoutRequest(res, queue[id][file])
      queue[id][file].push(timeoutRequest);

      var requeustQueue = this;

      return this.getBundle(packages)
    }
  },
  getBundle (packages) {
    var requestQueue = this;

    return new Promise(function (resolve, reject) {
      var availablePackager = packagers.sort(function (packagerA, packagerB) {
        if (packagerA.lastUsed > packagerB.lastUsed) {
          return 1;
        } else if (packagerB.lastUsed < packagerB.lastUsed) {
          return -1;
        }

        return 0;
      }).reduce(function (currentPackager, packager) {
        if (currentPackager) {
          return currentPackager;
        }

        if (packager.isAvailable) {
          return packager;
        }

        return currentPackager;
      }, null);

      if (!availablePackager) {
        throw new Error(errors.PACKAGER_NOT_AVAILABLE);
      }

      availablePackager.lastUsed = Date.now();
      availablePackager.isAvailable = false;
      emitPackagersUpdate();

      request({
        url: availablePackager.url + '/' + packages,
        timeout: config.packageServiceTimeout
      }, function (err, response, body) {
        if (response && response.statusCode === 503) {
          console.log('PACKAGER 503 ERROR - ' + (err ? err.message : body));
          availablePackager.isAvailable = false;
          availablePackager.timeoutCount++;
          reject(new Error(body));
          setTimeout(function () {
            availablePackager.isAvailable = true;
          }, 60000);
        } else if (err || (response && response.statusCode !== 200)) {
          console.log('PACKAGER ERROR - ' + (err ? err.message : body));
          if (body === 'INVALID_VERSION') {
            availablePackager.isAvailable = true;
            reject(new Error(body));
          } else {
            availablePackager.isAvailable = false;
            setTimeout(function () {
              availablePackager.isAvailable = true;
            }, 10000);
            resolve(requestQueue.getBundle(packages));
          }
          availablePackager.errorCount++;
          emitPackagersUpdate();
        } else {
          availablePackager.isAvailable = true;

          try {
            resolve(JSON.parse(body));
            availablePackager.resolvedCount++;
            emitPackagersUpdate();
          } catch (e) {
            availablePackager.errorCount++;
            emitPackagersUpdate();
            reject(e);
          }
        }
      })
    })
  },
  has: function (id) {
    return Boolean(queue[id]);
  },
  remove: function (id) {
    delete queue[id];
  },
  resolveFiles (id, bundle) {
    console.log('# RESOLVING FILES - ', queue[id].packages, bundle.manifest.length, bundle.dll.length);
    this.resolveManifest(id, bundle.manifest);
    this.resolveDll(id, bundle.dll);
  },
  resolveDll: function (id, content) {
    var requests = queue[id]['dll.js'];

    requests.forEach(utils.sendFile('dll.js', content))

    queue[id]['dll.js'] = [];
  },
  resolveManifest: function (id, content) {
    var requests = queue[id]['manifest.json'];

    requests.forEach(utils.sendFile('manifest.json', content));

    queue[id]['manifest.json'] = [];
  },
  reject: function (id, err) {
    var requests = queue[id]['dll.js'].concat(queue[id]['manifest.json']);

    console.error(err);
    requests.forEach(function (request) {
      try {
        request.send(500, err.message);
      } catch (e) {}
    })

    delete queue[id];
  }
}
