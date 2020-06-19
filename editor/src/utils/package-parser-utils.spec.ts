import * as FastCheck from 'fast-check'
import { parseVersionPackageJsonFile } from './package-parser-utils'
import { foldEither } from '../core/shared/either'

describe('parseVersionPackageJsonFile', () => {
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
})
