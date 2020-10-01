import { findMatchingVersion, packageNotFound, versionLookupSuccess } from './npm-dependency'

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
