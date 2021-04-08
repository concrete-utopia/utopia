import { SampleNodeModules } from '../../components/custom-code/code-file.test-utils'
import {
  complexDefaultProject,
  simpleDefaultProject,
} from '../../sample-projects/sample-project-utils'
import { getDirectReverseDependencies } from './project-contents-dependencies'

describe('getDirectReverseDependencies', () => {
  it('should return some expected value for a multi-file project', () => {
    const actualResult = getDirectReverseDependencies(
      complexDefaultProject().projectContents,
      SampleNodeModules,
    )
    expect(actualResult).toMatchInlineSnapshot(`
      Object {
        "/src/app.js": Array [
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
      simpleDefaultProject().projectContents,
      SampleNodeModules,
    )
    expect(actualResult).toMatchInlineSnapshot(`Object {}`)
  })
})
