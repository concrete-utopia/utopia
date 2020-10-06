import {
  findMatchingVersion,
  getVersionType,
  packageNotFound,
  versionLookupSuccess,
} from './npm-dependency'

require('jest-fetch-mock').enableMocks()

describe('Attempting to match a version for a valid package', () => {
  it('Matches a standard version if that version is in the list', async () => {
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        return Promise.resolve({ status: 200, body: JSON.stringify(['1.0.0', '1.1.0']) })
      },
    )
    const version = '1.0.0'
    const result = await findMatchingVersion('', version)
    expect(result).toEqual(versionLookupSuccess(version))
  })
  it('Matches a ranged version if that range can be satisfied by the list', async () => {
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        return Promise.resolve({ status: 200, body: JSON.stringify(['1.0.0', '1.1.0']) })
      },
    )
    const version = '^1.0.0'
    const result = await findMatchingVersion('', version)
    expect(result).toEqual(versionLookupSuccess('1.1.0'))
  })
  it('Fails a version if that version is not in the list', async () => {
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        return Promise.resolve({ status: 200, body: JSON.stringify(['1.0.0', '1.1.0']) })
      },
    )
    const version = '2.0.0'
    const result = await findMatchingVersion('', version)
    expect(result).toEqual(packageNotFound)
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
    const result = await findMatchingVersion('', version)
    expect(result).toEqual(packageNotFound)
  })
})

describe('Checking the version type of a dependency', () => {
  // The below examples are (mostly) taken from https://docs.npmjs.com/files/package.json#dependencies

  it('Returns GITHUB for github repos', () => {
    expect(getVersionType('expressjs/express')).toEqual('GITHUB')
    expect(getVersionType('mochajs/mocha#4727d357ea')).toEqual('GITHUB')
    expect(getVersionType('user/repo#feature/branch')).toEqual('GITHUB')
  })

  it('Returns LOCAL for local files', () => {
    expect(getVersionType('../foo/bar')).toEqual('LOCAL')
    expect(getVersionType('~/foo/bar')).toEqual('LOCAL')
    expect(getVersionType('./foo/bar')).toEqual('LOCAL')
    expect(getVersionType('/foo/bar')).toEqual('LOCAL')
    expect(getVersionType('file:../foo/bar')).toEqual('LOCAL')
  })

  it('Returns SEMVER for semver versions', () => {
    expect(getVersionType('1.0.0 - 2.9999.9999')).toEqual('SEMVER')
    expect(getVersionType('>=1.0.2 <2.1.2')).toEqual('SEMVER')
    expect(getVersionType('>1.0.2 <=2.3.4')).toEqual('SEMVER')
    expect(getVersionType('2.0.1')).toEqual('SEMVER')
    expect(getVersionType('<1.0.0 || >=2.3.1 <2.4.5 || >=2.5.2 <3.0.0')).toEqual('SEMVER')
    expect(getVersionType('~1.2')).toEqual('SEMVER')
    expect(getVersionType('~1.2.3')).toEqual('SEMVER')
    expect(getVersionType('2.x')).toEqual('SEMVER')
    expect(getVersionType('3.3.x')).toEqual('SEMVER')
  })

  it('Returns TAG for tagged versions', () => {
    expect(getVersionType('beta')).toEqual('TAG')
    expect(getVersionType('experimental')).toEqual('TAG')
    expect(getVersionType('latest')).toEqual('TAG')
    expect(getVersionType('next')).toEqual('TAG')
    expect(getVersionType('unstable')).toEqual('TAG')
  })

  it('Returns URL for URLs', () => {
    expect(getVersionType('http://asdf.com/asdf.tar.gz')).toEqual('URL')
    expect(getVersionType('git+ssh://git@github.com:npm/cli.git#v1.0.27')).toEqual('URL')
    expect(getVersionType('git+ssh://git@github.com:npm/cli#semver:^5.0')).toEqual('URL')
    expect(getVersionType('git+https://isaacs@github.com/npm/cli.git')).toEqual('URL')
    expect(getVersionType('git://github.com/npm/cli.git#v1.0.27')).toEqual('URL')
  })
})
