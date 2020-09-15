/*
Copyright (c) 2015, Yahoo! Inc. All rights reserved.
Copyrights licensed under the New BSD License.
See the accompanying LICENSE file for terms.
*/
module.exports = function(grunt) {

  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),
    jshint: {
      files: ['src/*.js'],
      options: {
        scripturl: true,
        camelcase: true,
        unused: true,
        curly: true,
        node: true
      }
    },
    jsdoc : {
      dist : {
        src: ['README.md', 'src/<%= pkg.name %>.js'], 
        options: {
          destination: 'dist/docs',
          template : 'node_modules/ink-docstrap/template'
        }
      }
    },
    browserify: {
      standalone: {
        src: [ 'src/<%= pkg.name %>.js' ],
        dest: 'dist/<%= pkg.name %>.js',
        options: {
          browserifyOptions: {
            standalone: 'xssFilters'
          }
        }
      }
    },
    uglify: {
      options: {
        banner: '/**\n'
              + ' * <%= pkg.name %> - v<%= pkg.version %>\n'
              + ' * Yahoo! Inc. Copyrights licensed under the New BSD License. See the accompanying LICENSE file for terms.\n'
              + ' */\n',
        compress: {
          join_vars: true
        },
        screwIE8: false // must turn it off, otherwise, \x0B will be converted as \v, which is interpreted as v in IE8 or below
      },
      buildBrowserified: {
        src: 'dist/<%= pkg.name %>.js',
        dest: 'dist/<%= pkg.name %>.min-browserified.js'
      },
      buildMin: {
        options: {
          wrap: 'xssFilters'
        },
        src: 'src/<%= pkg.name %>.js',
        dest: 'dist/<%= pkg.name %>.min.js'
      },
      buildMinWithVersion: {
        options: {
          wrap: 'xssFilters'
        },
        src: 'src/<%= pkg.name %>.js',
        dest: 'dist/<%= pkg.name %>.<%= pkg.version %>.min.js'
      }
    },
    mocha_istanbul: {
      coverage: {
        src: 'tests/node-unit-tests.js',
        options: {
          coverageFolder: 'artifacts/test/coverage',
          check: {
            lines: 80,
            statements: 80
          },
          timeout: 10000
        }
      }
    },
    karma: {
      options: {
        configFile: 'karma.conf.js'
      },
      ci: {
        
      },
      dev: {
        reporters: 'dots',
        browsers: ['Chrome']
      }
    },
    clean: {
      all: ['artifacts', 'node_modules', 'bower_components']
    }
  });

  grunt.loadNpmTasks('grunt-mocha-istanbul');
  grunt.loadNpmTasks('grunt-browserify');
  grunt.loadNpmTasks('grunt-contrib-uglify');
  grunt.loadNpmTasks('grunt-contrib-jshint');
  grunt.loadNpmTasks('grunt-contrib-clean');
  grunt.loadNpmTasks('grunt-jsdoc');
  grunt.loadNpmTasks('grunt-karma');


  var testSet = ['jshint', 'mocha_istanbul'];

  if (process.env.TRAVIS && process.env.TRAVIS_NODE_VERSION === '0.12')
    testSet.push('dist', 'karma:ci');

  grunt.registerTask('test', testSet);
  grunt.registerTask('dist', ['browserify', 'uglify'])
  grunt.registerTask('docs', ['jsdoc']);
  grunt.registerTask('default', ['test', 'dist']);

};
