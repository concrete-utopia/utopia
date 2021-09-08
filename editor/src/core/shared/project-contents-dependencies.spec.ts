import { SampleNodeModules } from '../../components/custom-code/code-file.test-utils'
import {
  complexDefaultProjectPreParsed,
  simpleDefaultProjectPreParsed,
} from '../../sample-projects/sample-project-utils.test-utils'
import { getDirectReverseDependencies } from './project-contents-dependencies'

describe('getDirectReverseDependencies', () => {
  it('should return some expected value for a multi-file project', () => {
    const actualResult = getDirectReverseDependencies(
      complexDefaultProjectPreParsed().projectContents,
      SampleNodeModules,
    )
    expect(actualResult).toMatchInlineSnapshot(`
      Object {
        "/src/app.js": Array [
          "/src/index.js",
          "/utopia/storyboard.js",
        ],
        "/src/card.js": Array [
          "/src/app.js",
        ],
      }
    `)
  })
  it('should return some expected value for a simple project', () => {
    const actualResult = getDirectReverseDependencies(
      simpleDefaultProjectPreParsed().projectContents,
      SampleNodeModules,
    )
    expect(actualResult).toMatchInlineSnapshot(`
      Object {
        "/src/app.js": Array [
          "/src/index.js",
          "/utopia/storyboard.js",
        ],
      }
    `)
  })
})
