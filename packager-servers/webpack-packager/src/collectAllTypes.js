const path = require('path')
const utils = require('./utils')
const findEntryPoints = require('./findEntryPoints')

function flatten(arr) {
  return Array.prototype.concat.apply([], arr)
}

function walkAllNodeModules(packagePath) {
  const nodeModulesPath = path.resolve(packagePath, 'node_modules')
  return utils.readDir(nodeModulesPath).then((dir) => {
    return Promise.all(
      dir.map((fileOrDir) => {
        const currentPath = path.join(nodeModulesPath, fileOrDir)
        return utils.stat(currentPath).then((fileStat) => {
          if (fileStat.isDirectory()) {
            return findEntryPoints(fileOrDir, currentPath, '.', [])
          } else {
            return null // Make sure to filter these nulls!
          }
        })
      }),
    )
  })
}

/**
 * @param packagePath {string}

    @returns {(mainResults: {
        vendors: string[];
        packageJsons: string[];
    }) => Promise<{
        vendors: string[];
        packageJsons: string[];
        extraDeps: string[]
    }>}
 */
module.exports = (packagePath) => (mainResults) => {
  return walkAllNodeModules(packagePath).then((allFilesResult) => {
    const allFiles = flatten(allFilesResult)
    const filteredFiles = allFiles.filter(
      (file) => file != null && (file.endsWith('.d.ts') || file.endsWith('package.json')),
    )
    return {
      ...mainResults,
      extraDeps: filteredFiles,
    }
  })
}
