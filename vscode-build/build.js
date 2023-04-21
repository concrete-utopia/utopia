const process = require('process')
const child_process = require('child_process')
const fs = require('fs')
const fse = require('fs-extra')
const glob = require('glob')
const rmdir = require('rimraf')

const vscodeVersion = '1.61.2'

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
child_process.execSync('yarn gulp compile-build', { stdio: 'inherit' })
child_process.execSync('yarn gulp minify-vscode', { stdio: 'inherit' })
child_process.execSync('yarn compile-web', { stdio: 'inherit' })

// Remove maps
const mapFiles = glob.sync('out-vscode-min/**/*.js.map', {})
mapFiles.forEach((mapFile) => {
  fs.unlinkSync(mapFile)
})

// Extract compiled files
if (fs.existsSync('../dist')) {
  fs.rmdirSync('../dist', { recursive: true })
}

fs.mkdirSync('../dist')
fs.mkdirSync('../dist/lib')
fse.copySync('out-vscode-min', '../dist/vscode')
fse.copySync('product.json', '../dist/product.json')
fse.copySync('../node_modules/semver-umd', '../dist/lib/semver-umd')
fse.copySync('../node_modules/vscode-oniguruma', '../dist/lib/vscode-oniguruma')
fse.copySync('../node_modules/vscode-textmate', '../dist/lib/vscode-textmate')
fse.copySync('../node_modules/utopia-vscode-common', '../dist/lib/utopia-vscode-common')

const extensionNM = glob.sync('extensions/**/node_modules', {})
extensionNM.forEach((modules) => {
  rmdir.sync(modules, { recursive: true })
})
fse.copySync('extensions', '../dist/extensions')

// Pull in the utopia extension and update the extensions metadata
process.chdir('../')
child_process.execSync('yarn pull-utopia-extension', { stdio: 'inherit' })
