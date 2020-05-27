const path = require('path');
const fs = require('fs');
const extractAndBundle = require('./src/middleware/extractAndBundle');

function getManifest (filePath) {
  return new Promise((resolve, reject) => {
    fs.lstat(filePath, (err, stat) => {
      if (err) {
        resolve(null)
      } else if (stat.isFile()) {
        fs.readFile(filePath, (err, file) => {
          if (err) {
            reject(err)
          } else {
            resolve(JSON.parse(file.toString()))
          }
        })
      } else {
        resolve(null)
      }
    })
  })
}

function createTest (package, version) {
  const filePath = path.resolve('manifests', encodeURIComponent(`${package}_${version}.json`));

  return function (test) {
    test.expect(1);

    getManifest(filePath)
      .then((manifest) => {
        const req = {
          params: {
            packages: `${package}@${version}`
          }
        }
        const res = {
          send(response) {
            if (manifest) {
              test.deepEqual(JSON.parse(response.manifest), manifest);
              console.log('TEST - Comparing existing manifest');
            } else {
              fs.writeFileSync(filePath, response.manifest, 'utf-8');
              test.ok(true);
              console.log('TEST - Wrote new manifest');
            }
          },
          status() {
            throw Error()
          }
        }

        return extractAndBundle(req, res)
      })
      .then(test.done).catch(test.done)
  }
}

function createTests (packages) {
  Object.keys(packages).forEach((package) => {
    module.exports[`should work with ${package}`] = createTest(package, packages[package]);
  })
}

createTests({
  'react': '15.5.4',
  'react-dom': '15.5.4',
  'react-portal': '3.1.0',
  'redux': '3.6.0',
  'styled-components': '2.0.0-10',
  '@angular/common': '2.4.9',
  '@cycle/run': '3.0.0',
  'xstream': '10.3.0',
  'mobx': '3.1.7',
  'vue': '2.2.4',
  'glamorous': '3.10.0',
  'apollo-client': '1.1.1',
  'axios': '0.16.1',
  'slate': '0.20.2',
  'react-icons': '2.2.5',
  'react-router-dom': '4.1.1',
  'react-draggable': '2.2.6',
  'todomvc-app-css': '2.1.0',
  'algoliasearch': '3.23.0',
  'history': '4.6.2',
  'uuid': '3.1.0',
  'es6-promise': '4.1.1'
})
