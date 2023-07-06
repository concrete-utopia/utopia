/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "FastCheck.assert"] }] */
import * as FastCheck from 'fast-check'
import fastDeepEquals from 'fast-deep-equal'
import type { Either } from '../either'
import { left, mapEither, right } from '../either'
import { eitherRight, fromField, logOptic, traverseArray } from './optic-creators'
import { modify, toArrayOf } from './optic-utilities'
import type { Optic } from './optics'

describe('optics examples', () => {
  it('flattening arrays', () => {
    const property = FastCheck.property(
      FastCheck.array(FastCheck.array(FastCheck.integer())),
      (array: Array<Array<number>>) => {
        const actualResult: Array<number> = toArrayOf(
          traverseArray<Array<number>>().compose(traverseArray<number>()),
          array,
        )
        const expectedResult: Array<number> = array.flat()
        return fastDeepEquals(actualResult, expectedResult)
      },
    )
    FastCheck.assert(property, { verbose: true })
  })
  describe('manipulates complicated structures', () => {
    interface JSXElement {
      name: string
    }
    const jsxElementArbitrary: FastCheck.Arbitrary<JSXElement> = FastCheck.string().map((name) => {
      return {
        name: name,
      }
    })
    interface ParseSuccess {
      elements: Array<JSXElement>
    }
    const parseSuccessArbitrary: FastCheck.Arbitrary<ParseSuccess> = FastCheck.array(
      jsxElementArbitrary,
    ).map((elements) => {
      return {
        elements: elements,
      }
    })
    interface ProjectFile {
      filename: string
      code: string
      parseResult: Either<string, ParseSuccess>
    }
    const parseResultArbitrary: FastCheck.Arbitrary<Either<string, ParseSuccess>> = FastCheck.tuple(
      FastCheck.string(),
      parseSuccessArbitrary,
      FastCheck.boolean(),
    ).map(([leftValue, rightValue, isRight]) => {
      return isRight ? right(rightValue) : left(leftValue)
    })
    const filenameArbitrary: FastCheck.Arbitrary<string> = FastCheck.oneof(
      FastCheck.constant('/somefile.js'),
      FastCheck.constant('/folder/file.js'),
      FastCheck.constant('/anotherfile.js'),
    )
    const projectFileArbitrary: FastCheck.Arbitrary<ProjectFile> = FastCheck.tuple(
      filenameArbitrary,
      FastCheck.string(),
      parseResultArbitrary,
    ).map(([filename, code, parseResult]) => {
      return {
        filename: filename,
        code: code,
        parseResult: parseResult,
      }
    })
    // We can reuse this.
    const filesToElementNameLens: Optic<Array<ProjectFile>, string> = traverseArray<ProjectFile>()
      .compose(fromField('parseResult'))
      .compose(eitherRight())
      .compose(fromField('elements'))
      .compose(traverseArray())
      .compose(fromField('name'))
    it('pulls values from deep within a structure', () => {
      const property = FastCheck.property(
        FastCheck.array(projectFileArbitrary).noShrink(),
        (files) => {
          const lensResult = toArrayOf(filesToElementNameLens, files)
          let manualResult: Array<string> = []
          for (const file of files) {
            if (file.parseResult.type === 'RIGHT') {
              const parseSuccess = file.parseResult.value
              for (const element of parseSuccess.elements) {
                manualResult.push(element.name)
              }
            }
          }
          return fastDeepEquals(lensResult, manualResult)
        },
      )
      FastCheck.assert(property, { verbose: true })
    })
    it('updates values from deep within a structure', () => {
      const property = FastCheck.property(
        FastCheck.array(projectFileArbitrary).noShrink(),
        (files) => {
          const lensResult = modify(filesToElementNameLens, (name) => `NamePrefix.${name}`, files)
          const manualResult: Array<ProjectFile> = files.map((file) => {
            return {
              ...file,
              parseResult: mapEither((success) => {
                return {
                  ...success,
                  elements: success.elements.map((element) => {
                    return {
                      ...element,
                      name: `NamePrefix.${element.name}`,
                    }
                  }),
                }
              }, file.parseResult),
            }
          })
          return fastDeepEquals(lensResult, manualResult)
        },
      )
      FastCheck.assert(property, { verbose: true })
    })
  })
})
