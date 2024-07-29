const process = require('process')
const child_process = require('child_process')
const fs = require('fs')
const fse = require('fs-extra')
const glob = require('glob')
const rmdir = require('rimraf')

const vscodeVersion = '1.91.1'

if (fs.existsSync('vscode')) {
  process.chdir('vscode')
  child_process.execSync('git restore .', {
    stdio: 'inherit',
  })
  child_process.execSync('git clean -f -d', {
    stdio: 'inherit',
  })
  child_process.execSync('git fetch', {
    stdio: 'inherit',
  })
} else {
  child_process.execSync('git clone https://github.com/microsoft/vscode.git', {
    stdio: 'inherit',
  })
  process.chdir('vscode')
}

child_process.execSync(`git checkout -q ${vscodeVersion}`, {
  stdio: 'inherit',
})
child_process.execSync(`git apply ../vscode.patch`, {
  stdio: 'inherit',
})

child_process.execSync('yarn', { stdio: 'inherit' })

// Compile
// child_process.execSync('yarn compile-build', { stdio: 'inherit' }) // yarn compile-build ?
// child_process.execSync('yarn minify-vscode', { stdio: 'inherit' }) // yarn minify-vscode ?
child_process.execSync('yarn gulp vscode-web-min', { stdio: 'inherit' })
child_process.execSync('yarn compile-web', { stdio: 'inherit' }) // Maybe don't need this?

// // Remove maps
// const mapFiles = glob.sync('out-vscode-min/**/*.js.map', {})
// mapFiles.forEach((mapFile) => {
//   fs.unlinkSync(mapFile)
// })

// Extract compiled files
if (fs.existsSync('../dist')) {
  fs.rmdirSync('../dist', { recursive: true })
}

fse.moveSync('../vscode-web', '../dist')
fs.mkdirSync('../dist/lib')
fse.copySync('resources', '../dist/vscode/resources')
fse.copySync('product.json', '../dist/product.json')
fse.copySync('../node_modules/semver-umd', '../dist/lib/semver-umd')
fse.copySync('../node_modules/vscode-oniguruma', '../dist/lib/vscode-oniguruma')
fse.copySync('../node_modules/vscode-textmate', '../dist/lib/vscode-textmate')
// fse.copySync('../node_modules/utopia-vscode-common', '../dist/lib/utopia-vscode-common')

// const extensionNM = glob.sync('extensions/**/node_modules', {})
// extensionNM.forEach((modules) => {
//   rmdir.sync(modules, { recursive: true })
// })
// fse.copySync('extensions', '../dist/extensions')

// // Pull in the utopia extension and update the extensions metadata
// process.chdir('../')
// // child_process.execSync('yarn pull-utopia-extension', { stdio: 'inherit' })
