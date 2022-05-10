const fs = require('fs')

function flatten(arr) {
  return Array.prototype.concat.apply([], arr)
}

// https://github.com/tc39/proposal-object-from-entries/blob/master/polyfill.js
function ObjectFromEntries(iter) {
  const obj = {}

  for (const pair of iter) {
    if (Object(pair) !== pair) {
      throw new TypeError('iterable for fromEntries should yield objects')
    }

    // Consistency with Map: contract is that entry has "0" and "1" keys, not
    // that it is an array or iterable.

    const { 0: key, 1: val } = pair

    Object.defineProperty(obj, key, {
      configurable: true,
      enumerable: true,
      writable: true,
      value: val,
    })
  }

  return obj
}

function directory() {
  return {
    type: 'DIRECTORY',
  }
}

function codeFile(fileContents) {
  return {
    type: 'CODE_FILE',
    fileContents: fileContents,
    lastSavedContents: null,
  }
}

/**
 * @param {string} filePath
 * @return {Promise<string>}
 */
function readFileAsync(filePath) {
  return new Promise((resolve, reject) => {
    fs.readFile(filePath, (err, data) => {
      if (err != null) {
        reject(err)
      } else {
        resolve(data.toString('base64'))
      }
    })
  })
}

/**
 *
 * @param {string} filePath
 * @return {Array<fs.Dirent>}
 */
function readDirAsync(filePath) {
  return new Promise((resolve, reject) => {
    fs.readdir(filePath, { withFileTypes: true }, (err, files) => {
      if (err != null) {
        reject(err)
      } else {
        resolve(files)
      }
    })
  })
}

function filterFileByName(name) {
  return ['.DS_Store', 'loader.js', '.spec.ts', '.snapshot.'].every(
    (word) => name.indexOf(word) < 0,
  )
}

/**
 *
 * @param {string} realPath
 * @param {string} utopiaPath
 * @returns {Promise<[string, any]>}
 */
async function turnDirectoryIntoProjectContents(realPath, utopiaPath) {
  const directoryEntries = await readDirAsync(realPath)
  const keyValueResults = [
    [utopiaPath, directory()],
    ...flatten(
      await Promise.all(
        directoryEntries.map(async (dirEnt) => {
          const filePath = realPath + '/' + dirEnt.name
          const utopiaFilePath = utopiaPath + '/' + dirEnt.name
          if (dirEnt.isFile() && filterFileByName(dirEnt.name)) {
            const fileString = await readFileAsync(filePath)
            return [[utopiaFilePath, codeFile(fileString)]]
          } else if (dirEnt.isDirectory()) {
            return turnDirectoryIntoProjectContents(filePath, utopiaFilePath)
          } else {
            return null
          }
        }),
      ),
    ).filter((e) => e != null),
  ]

  return keyValueResults
}

module.exports = async (options, loaderContext) => {
  const result = await turnDirectoryIntoProjectContents(loaderContext.context, '/uuiui')
  const resultObject = ObjectFromEntries(result)
  const printedSanitizedResult = JSON.stringify(resultObject, null, 2).replace(/\\/g, '\\\\')

  return new Promise((resolve, reject) => {
    setTimeout(function () {
      resolve({ code: `module.exports = JSON.parse(\`${printedSanitizedResult}\`)` })
    }, 1000)
  })
}
