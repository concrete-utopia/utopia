var webpack = require('webpack')
var path = require('path')
var utils = require('./utils')
var getVendors = require('./getVendors')
var extractPackageJsonPaths = require('./extractPackageJsonPaths')
const collectAllTypes = require('./collectAllTypes')

module.exports = function(packagePath) {
  return function(entries) {
    return getVendors(entries, packagePath)
      .then(extractPackageJsonPaths(entries, packagePath))
      .then(collectAllTypes(packagePath))
      .then(function(results) {
        // Grab all extra entries related to browser mappings
        var mapEntries = Object.keys(entries).reduce(function(currentMapEntries, entryKey) {
          return currentMapEntries.concat(Object.keys(entries[entryKey].map || {}))
        }, [])
        var emptyAliases = Object.keys(entries).reduce(function(currentAliases, entryKey) {
          return currentAliases.concat(entries[entryKey].emptyModulesAliases)
        }, [])

        var alias = emptyAliases.reduce(
          function(result, emptyModule) {
            return Object.assign(result, {
              [emptyModule]: 'empty-module',
            })
          },
          {
            'custom-css-loader': require.resolve('./customCssLoader'),
            'custom-dts-loader': require.resolve('./customDtsLoader'),
          },
        )

        const vendorsWithTypes = Array.from(new Set(results.vendors.concat(mapEntries).concat(results.extraDeps))) 
        
        var webpackConfig = {
          context: '/',
          entry: { vendors: vendorsWithTypes },
          output: {
            path: path.resolve(packagePath),
            filename: 'dll.js',
            library: 'dll_bundle',
          },
          plugins: [
            new webpack.DefinePlugin({
              'process.env.NODE_ENV': JSON.stringify('development'),
            }),
            new webpack.DllPlugin({
              path: path.resolve(packagePath, 'manifest.json'),
              name: 'dll_bundle',
            }),
            // new webpack.optimize.UglifyJsPlugin({minimize: true, mangle: false})
          ],
          resolve: {
            // The code here used to be:
            // `modules: [path.resolve(packagePath, 'node_modules'), path.resolve('node_modules')]`
            // Balazs: I removed `path.resolve('node_modules')` from here, because that is most probably a mistake
            // which we already fixed by making a `yarn init` in the temp directory
            modules: [path.resolve(packagePath, 'node_modules')],
          },
          resolveLoader: {
            alias: alias,
          },
          node: { fs: 'empty' },
          module: {
            loaders: [
              {
                test: /\.css$/,
                use: 'custom-css-loader',
              },
              {
                test: /\.d.ts$/,
                use: 'custom-dts-loader',
              },
            ],
          },
          /**
           * externals stores the common peer dependencies required by modules,
           * we can't include these in the bundle,
           * but we want __webpack_require__ to be able to find them.
           * These MUST BE mirrored in the editor's eval namespace in custom-code-component.ts
           */
          externals: {
            react: 'React',
            'react-dom': 'ReactDOM',
            'utopia-api': 'utopia-api',
          },
        }

        var vendorsCompiler = webpack(webpackConfig)

        return new Promise(function(resolve, reject) {
          vendorsCompiler.run(function(err) {
            if (err) {
              return reject(err)
            }

            resolve()
          })
        })
          .then(function() {
            return utils.readFile(path.resolve(packagePath, 'manifest.json'))
          })
          .then(function(manifestJson) {
            var manifest = JSON.parse(manifestJson)

            manifest.content = utils.cleanManifestContent(manifest, entries, packagePath)
            manifest.externals = utils.createExternals(manifest, results.packageJsons, entries)

            return utils.writeFile(
              path.resolve(packagePath, 'manifest.json'),
              JSON.stringify(manifest, null, 2),
            )
          })
      })
  }
}
