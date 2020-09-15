# Change Log

All notable changes to this project will be documented in this file. See [standard-version](https://github.com/conventional-changelog/standard-version) for commit guidelines.

<a name="2.6.2"></a>
## [2.6.2](https://github.com/tleunen/babel-plugin-module-resolver/compare/v2.6.1...v2.6.2) (2017-03-30)


### Bug Fixes

* Fix the double plugin bug ([#146](https://github.com/tleunen/babel-plugin-module-resolver/issues/146)) ([4e19188](https://github.com/tleunen/babel-plugin-module-resolver/commit/4e19188))



<a name="2.6.1"></a>
## [2.6.1](https://github.com/tleunen/babel-plugin-module-resolver/compare/v2.6.0...v2.6.1) (2017-03-29)


### Bug Fixes

* Fix `regExps.find` undefined error ([2171200](https://github.com/tleunen/babel-plugin-module-resolver/commit/2171200))



<a name="2.6.0"></a>
# [2.6.0](https://github.com/tleunen/babel-plugin-module-resolver/compare/v2.5.0...v2.6.0) (2017-03-29)


### Bug Fixes

* Fix plugin running twice in some cases ([#136](https://github.com/tleunen/babel-plugin-module-resolver/issues/136)) ([097bcc8](https://github.com/tleunen/babel-plugin-module-resolver/commit/097bcc8)), closes [#96](https://github.com/tleunen/babel-plugin-module-resolver/issues/96)


### Features

* Add regular expression support ([#132](https://github.com/tleunen/babel-plugin-module-resolver/issues/132)) ([6d87b25](https://github.com/tleunen/babel-plugin-module-resolver/commit/6d87b25)), closes [#88](https://github.com/tleunen/babel-plugin-module-resolver/issues/88) [#127](https://github.com/tleunen/babel-plugin-module-resolver/issues/127)
* Handle the "export from" statement ([#129](https://github.com/tleunen/babel-plugin-module-resolver/issues/129)) ([627b897](https://github.com/tleunen/babel-plugin-module-resolver/commit/627b897)), closes [#128](https://github.com/tleunen/babel-plugin-module-resolver/issues/128)



<a name="2.5.0"></a>
# [2.5.0](https://github.com/tleunen/babel-plugin-module-resolver/compare/v2.4.0...v2.5.0) (2017-02-05)


### Bug Fixes

* Add only directories in a glob root config ([#120](https://github.com/tleunen/babel-plugin-module-resolver/issues/120)) ([ac87924](https://github.com/tleunen/babel-plugin-module-resolver/commit/ac87924))
* Remove unnecessary trailing /index in the local path resolution ([#117](https://github.com/tleunen/babel-plugin-module-resolver/issues/117)) ([ac0c671](https://github.com/tleunen/babel-plugin-module-resolver/commit/ac0c671))


### Features

* Add support for jest doMock and dontMock functions ([#114](https://github.com/tleunen/babel-plugin-module-resolver/issues/114)) ([0bc01ec](https://github.com/tleunen/babel-plugin-module-resolver/commit/0bc01ec))
* Export manipulateOptions ([85270e1](https://github.com/tleunen/babel-plugin-module-resolver/commit/85270e1))
* Export the default extensions used by the plugin ([1c4fbcc](https://github.com/tleunen/babel-plugin-module-resolver/commit/1c4fbcc))



<a name="2.4.0"></a>
# [2.4.0](https://github.com/tleunen/babel-plugin-module-resolver/compare/v2.3.0...v2.4.0) (2016-11-30)


### Features

* Add support for Jest specific functions ([#100](https://github.com/tleunen/babel-plugin-module-resolver/issues/100)) ([ffa2c39](https://github.com/tleunen/babel-plugin-module-resolver/commit/ffa2c39)), closes [#97](https://github.com/tleunen/babel-plugin-module-resolver/issues/97)
* Add support for System.import ([#101](https://github.com/tleunen/babel-plugin-module-resolver/issues/101)) ([06ba4dd](https://github.com/tleunen/babel-plugin-module-resolver/commit/06ba4dd)), closes [#99](https://github.com/tleunen/babel-plugin-module-resolver/issues/99)



<a name="2.3.0"></a>
# [2.3.0](https://github.com/tleunen/babel-plugin-module-resolver/compare/v3.0.0-beta.1...v2.3.0) (2016-11-06)


### Bug Fixes

* Fix wrong location of modified path ([e7083ab](https://github.com/tleunen/babel-plugin-module-resolver/commit/e7083ab)), closes [#94](https://github.com/tleunen/babel-plugin-module-resolver/issues/94)


### Features

* Move the babelrc lookup behind a custom cwd value option ([2a8aca5](https://github.com/tleunen/babel-plugin-module-resolver/commit/2a8aca5))



<a name="2.2.0"></a>
# [2.2.0](https://github.com/tleunen/babel-plugin-module-resolver/compare/v2.1.1...v2.2.0) (2016-08-27)


### Bug Fixes

* Fix plugin on windows ([d6b0a4b](https://github.com/tleunen/babel-plugin-module-resolver/commit/d6b0a4b))
* Fix the root resolver when both a file and directory have the same name ([a4cff68](https://github.com/tleunen/babel-plugin-module-resolver/commit/a4cff68))


### Features

* Add glob support in the root config ([#78](https://github.com/tleunen/babel-plugin-module-resolver/issues/78)) ([1f6245b](https://github.com/tleunen/babel-plugin-module-resolver/commit/1f6245b))



<a name="2.1.1"></a>
## [2.1.1](https://github.com/tleunen/babel-plugin-module-resolver/compare/v2.1.0...v2.1.1) (2016-08-22)


### Bug Fixes

* Fix resolver with filename containing a dot ([#75](https://github.com/tleunen/babel-plugin-module-resolver/issues/75)) ([bb6c903](https://github.com/tleunen/babel-plugin-module-resolver/commit/bb6c903)), closes [#74](https://github.com/tleunen/babel-plugin-module-resolver/issues/74)



<a name="2.1.0"></a>
# [2.1.0](https://github.com/tleunen/babel-plugin-module-resolver/compare/v2.0.0...v2.1.0) (2016-08-19)


### Bug Fixes

* Fix root mapping with custom extensions ([#72](https://github.com/tleunen/babel-plugin-module-resolver/issues/72)) ([3d4756b](https://github.com/tleunen/babel-plugin-module-resolver/commit/3d4756b))


### Features

* Aliasing a npm module without 'npm:' ([#73](https://github.com/tleunen/babel-plugin-module-resolver/issues/73)) ([8e95988](https://github.com/tleunen/babel-plugin-module-resolver/commit/8e95988))



<a name="2.0.0"></a>
# [2.0.0](https://github.com/tleunen/babel-plugin-module-resolver/compare/v1.6.0...v2.0.0) (2016-08-14)


### Features

* Add support for custom root directories ([#69](https://github.com/tleunen/babel-plugin-module-resolver/issues/69)) ([3dd29a0](https://github.com/tleunen/babel-plugin-module-resolver/commit/3dd29a0)), closes [#46](https://github.com/tleunen/babel-plugin-module-resolver/issues/46)


### BREAKING CHANGES

* There's a new way to specify alias in the plugin options. See README for more info
