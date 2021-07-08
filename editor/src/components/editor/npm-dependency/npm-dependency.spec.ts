import { requestedNpmDependency } from '../../../core/shared/npm-dependency-types'
import { codeFile, importAlias, importDetails } from '../../../core/shared/project-file-types'
import {
  allDependenciesFromPackageJsonContents,
  dependenciesFromPackageJson,
  findMatchingVersion,
  getVersionType,
  importResultFromModule,
  npmVersionLookupSuccess,
  packageNotFound,
} from './npm-dependency'

require('jest-fetch-mock').enableMocks()

describe('Attempting to match a version for a valid package', () => {
  it('Matches a standard version if that version is the only one returned', async () => {
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        return Promise.resolve({ status: 200, body: JSON.stringify('1.0.0') })
      },
    )
    const version = '1.0.0'
    const result = await findMatchingVersion('foo', version)
    expect(result).toEqual(npmVersionLookupSuccess(version))
  })
  it('Fails to match a standard version if multiple versions are returned', async () => {
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        return Promise.resolve({ status: 200, body: JSON.stringify(['1.0.0', '1.0.1']) })
      },
    )
    const version = '1.0.0'
    const result = await findMatchingVersion('foo', version)
    expect(result).toEqual(npmVersionLookupSuccess(version))
  })
  it('Matches a ranged version if that range can be satisfied by the list', async () => {
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        return Promise.resolve({ status: 200, body: JSON.stringify(['1.0.0', '1.1.0']) })
      },
    )
    const version = '^1.0.0'
    const result = await findMatchingVersion('foo', version)
    expect(result).toEqual(npmVersionLookupSuccess('1.1.0'))
  })
  it('Fails a version if that version is not in the list', async () => {
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        return Promise.resolve({ status: 200, body: JSON.stringify(['1.0.0', '1.1.0']) })
      },
    )
    const version = '2.0.0'
    const result = await findMatchingVersion('foo', version)
    expect(result).toEqual(packageNotFound)
  })
  it('Matches a version tag if only one result is returned', async () => {
    const matchingResult = '1.0.0'
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        return Promise.resolve({ status: 200, body: JSON.stringify(matchingResult) })
      },
    )
    const version = 'latest'
    const result = await findMatchingVersion('foo', version)
    expect(result).toEqual(npmVersionLookupSuccess(matchingResult))
  })
})

describe('Attempting to match a version for an invalid package', () => {
  it('Fails for any version', async () => {
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        return Promise.resolve({ status: 404 })
      },
    )
    const version = '1.0.0'
    const result = await findMatchingVersion('foo', version)
    expect(result).toEqual(packageNotFound)
  })
})

describe('Checking the version type of a dependency', () => {
  // The below examples are (mostly) taken from https://docs.npmjs.com/files/package.json#dependencies

  it('Returns GITHUB for github repos', () => {
    expect(getVersionType('expressjs', 'expressjs/express')).toEqual('GITHUB')
    expect(getVersionType('mochajs', 'mochajs/mocha#4727d357ea')).toEqual('GITHUB')
    expect(getVersionType('user', 'user/repo#feature/branch')).toEqual('GITHUB')
    expect(getVersionType('npm', 'git+ssh://git@github.com:npm/cli.git#v1.0.27')).toEqual('GITHUB')
    expect(getVersionType('npm', 'git+ssh://git@github.com:npm/cli#semver:^5.0')).toEqual('GITHUB')
    expect(getVersionType('npm', 'git+https://isaacs@github.com/npm/cli.git')).toEqual('GITHUB')
    expect(getVersionType('npm', 'git://github.com/npm/cli.git#v1.0.27')).toEqual('GITHUB')
  })

  it('Returns LOCAL for local files', () => {
    expect(getVersionType('bar', '../foo/bar')).toEqual('LOCAL')
    expect(getVersionType('bar', '~/foo/bar')).toEqual('LOCAL')
    expect(getVersionType('bar', './foo/bar')).toEqual('LOCAL')
    expect(getVersionType('bar', '/foo/bar')).toEqual('LOCAL')
    expect(getVersionType('bar', 'file:../foo/bar')).toEqual('LOCAL')
  })

  it('Returns SEMVER for semver versions', () => {
    expect(getVersionType('foo', '1.0.0 - 2.9999.9999')).toEqual('SEMVER')
    expect(getVersionType('foo', '>=1.0.2 <2.1.2')).toEqual('SEMVER')
    expect(getVersionType('foo', '>1.0.2 <=2.3.4')).toEqual('SEMVER')
    expect(getVersionType('foo', '2.0.1')).toEqual('SEMVER')
    expect(getVersionType('foo', '<1.0.0 || >=2.3.1 <2.4.5 || >=2.5.2 <3.0.0')).toEqual('SEMVER')
    expect(getVersionType('foo', '~1.2')).toEqual('SEMVER')
    expect(getVersionType('foo', '~1.2.3')).toEqual('SEMVER')
    expect(getVersionType('foo', '2.x')).toEqual('SEMVER')
    expect(getVersionType('foo', '3.3.x')).toEqual('SEMVER')
  })

  it('Returns TAG for tagged versions', () => {
    expect(getVersionType('foo', 'beta')).toEqual('TAG')
    expect(getVersionType('foo', 'experimental')).toEqual('TAG')
    expect(getVersionType('foo', 'latest')).toEqual('TAG')
    expect(getVersionType('foo', 'next')).toEqual('TAG')
    expect(getVersionType('foo', 'unstable')).toEqual('TAG')
  })

  it('Returns URL for URLs', () => {
    expect(getVersionType('asdf', 'http://asdf.com/asdf.tar.gz')).toEqual('URL')
    expect(
      getVersionType('forever', 'https://github.com/indexzero/forever/tarball/v0.5.6'),
    ).toEqual('URL')
  })
})

describe('Importing from a resolved module', () => {
  it('Handles all forms of imports for an es module', () => {
    // import cake, { icing as doIWantIcing } from './cake'
    // import * as cakeStuff from './cake'
    const imports = importDetails('cake', [importAlias('icing', 'doIWantIcing')], 'cakeStuff')

    const defaultExport = 'A fully fledged cake'
    const icingExport = 'ooooh yes please, but only the decent stuff'

    // export const icing = 'ooooh yes please, but only the decent stuff'
    // export default 'A fully fledged cake'
    const cakesModule = {
      default: defaultExport,
      icing: icingExport,
      __esModule: true,
    }

    const importResult = importResultFromModule(imports, cakesModule)
    expect(importResult).toEqual({
      cake: defaultExport,
      doIWantIcing: icingExport,
      cakeStuff: cakesModule,
    })
  })

  it('Handles all forms of imports for a common js module with a default export', () => {
    // import cake, { icing as doIWantIcing } from './cake'
    // import * as cakeStuff from './cake'
    const imports = importDetails('cake', [importAlias('icing', 'doIWantIcing')], 'cakeStuff')

    const defaultExport = 'A fully fledged cake'

    // module.exports = 'A fully fledged cake'
    const cakesModule = defaultExport

    const importResult = importResultFromModule(imports, cakesModule)
    expect(importResult).toEqual({
      cake: defaultExport,
      doIWantIcing: undefined,
      cakeStuff: {
        default: defaultExport,
      },
    })
  })

  it('Handles all forms of imports for a common js module with named exports', () => {
    // import cake, { icing as doIWantIcing } from './cake'
    // import * as cakeStuff from './cake'
    const imports = importDetails('cake', [importAlias('icing', 'doIWantIcing')], 'cakeStuff')

    const icingExport = 'ooooh yes please, but only the decent stuff'

    // exports.icing = 'ooooh yes please, but only the decent stuff'
    const cakesModule = {
      icing: icingExport,
    }

    const importResult = importResultFromModule(imports, cakesModule)
    expect(importResult).toEqual({
      cake: cakesModule,
      doIWantIcing: icingExport,
      cakeStuff: {
        default: cakesModule,
        icing: icingExport,
      },
    })
  })
})

describe('Parsing the package.json', () => {
  const packageJsonFileContents = `
  {
    "name": "myapp",
    "version": "1.0.0",
    "license": "MIT",
    "dependencies": {
      'dep1': '1.0.0',
      'dep2': '2.0.0',
      'dep3': '3.0.0'
    },
    "devDependencies": {
      'devDep1': '1.0.0',
      'devDep2': '2.0.0',
      'devDep3': '3.0.0'
    }
  }
  `
  const packageJsonFile = codeFile(packageJsonFileContents, null)

  const expectedDependencies = [
    requestedNpmDependency('dep1', '1.0.0'),
    requestedNpmDependency('dep2', '2.0.0'),
    requestedNpmDependency('dep3', '3.0.0'),
  ]

  const expectedDevDependencies = [
    requestedNpmDependency('devDep1', '1.0.0'),
    requestedNpmDependency('devDep2', '2.0.0'),
    requestedNpmDependency('devDep3', '3.0.0'),
  ]

  const expectedCombined = [...expectedDependencies, ...expectedDevDependencies]

  it('allDependenciesFromPackageJsonContents parses all dependencies and dev dependencies from the package.json', () => {
    const parsedDependencies = allDependenciesFromPackageJsonContents(packageJsonFileContents)
    expect(parsedDependencies.dependencies).toEqual(expectedDependencies)
    expect(parsedDependencies.devDependencies).toEqual(expectedDevDependencies)
    expect(parsedDependencies.combined).toEqual(expectedCombined)
  })

  it('dependenciesFromPackageJson returns the combined dependencies if includeDevDependencies is true', () => {
    const combinedDependencies = dependenciesFromPackageJson(packageJsonFile, 'combined')
    expect(combinedDependencies).toEqual(expectedCombined)
  })

  it('dependenciesFromPackageJson returns the regular dependencies only if includeDevDependencies is false', () => {
    const regularDependencies = dependenciesFromPackageJson(packageJsonFile, 'regular-only')
    expect(regularDependencies).toEqual(expectedDependencies)
  })

  it('dependenciesFromPackageJson returns a cached result if called again with the same unchanged file contents', () => {
    const firstCombined = dependenciesFromPackageJson(packageJsonFile, 'combined')
    const secondCombined = dependenciesFromPackageJson(packageJsonFile, 'combined')

    const firstRegular = dependenciesFromPackageJson(packageJsonFile, 'regular-only')
    const secondRegular = dependenciesFromPackageJson(packageJsonFile, 'regular-only')

    expect(firstCombined === secondCombined).toBeTruthy()
    expect(firstRegular === secondRegular).toBeTruthy()
  })
})
