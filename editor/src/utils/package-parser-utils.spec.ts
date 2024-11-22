import * as FastCheck from 'fast-check'
import {
  parseVersionPackageJsonFile,
  parseUtopiaConfigFromPackageJsonFile,
} from './package-parser-utils'
import { foldEither, right } from '../core/shared/either'

describe('parseVersionPackageJsonFile', () => {
  // eslint-disable-next-line jest/expect-expect
  it('parses the version out of a given file', () => {
    const arbitraryVersion = FastCheck.string()
    const prop = FastCheck.property(arbitraryVersion, (version: string) => {
      const packageJson = {
        name: 'test project',
        version: version,
      }
      const asString = JSON.stringify(packageJson)
      const result = parseVersionPackageJsonFile(asString)
      return foldEither(
        (_) => false,
        (parsedVersion) => parsedVersion === version,
        result,
      )
    })
    FastCheck.assert(prop, { verbose: true })
  })

  describe('parseUtopiaConfigFromPackageJsonFile', () => {
    it('parses the utopia config out of package.json string, with the utopia config present', () => {
      const result = parseUtopiaConfigFromPackageJsonFile(
        JSON.stringify({ version: '42', utopia: { tailwind: {} } }),
      )
      expect(result).toEqual(right({ tailwind: {} }))
    })
    it('parses the utopia config out of package.json string, with empty utopia object', () => {
      const result = parseUtopiaConfigFromPackageJsonFile(
        JSON.stringify({ version: '42', utopia: {} }),
      )
      expect(result).toEqual(right({}))
    })
    it('parses the utopia config out of package.json string, without the utopia config', () => {
      const result = parseUtopiaConfigFromPackageJsonFile(JSON.stringify({ version: '42' }))
      expect(result).toEqual(right(null))
    })
  })
})
