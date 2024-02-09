const fs = require('fs')
const { join } = require('path')

/**
 * Scan dependencies in the package.json files in these folders.
 */
const folders = [
  '.',
  './editor',
  './utopia-remix',
  './puppeteer-tests',
  './utopia-api',
  './utopia-vscode-common',
  './utopia-vscode-extension',
]

const unpinnedDeps = folders.flatMap((projectPath) => {
  const path = join(projectPath, 'package.json')
  const packageJson = JSON.parse(fs.readFileSync(path), 'utf8')
  const allDeps = {
    ...packageJson.dependencies,
    // ...packageJson.devDependencies,
  }
  return Object.entries(allDeps)
    .filter(([, version]) => version.startsWith('^') || version.startsWith('~'))
    .map(([name, version]) => ({ path, name, version }))
})

if (unpinnedDeps.length > 0) {
  console.log(`🚨 Found ${unpinnedDeps.length} unpinned dependencies.`)
  console.log(unpinnedDeps)
  process.exit(1)
}
