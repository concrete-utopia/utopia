import { isRight } from '../core/shared/either'
import { notNullIsTextFile, ProjectContents, ProjectFile } from '../core/shared/project-file-types'
import { createComplexDefaultProjectContents } from '../sample-projects/sample-project-utils'
import { compose4Lenses, fromField, fromTypeGuard, fromObjectField, toFirst } from './lens'

describe('lenses connected up', () => {
  it('should be able to retrieve data from a model', () => {
    const projectContents = createComplexDefaultProjectContents()
    const lens = compose4Lenses(
      fromObjectField<ProjectFile, ProjectContents>('/package.json'),
      fromTypeGuard(notNullIsTextFile),
      fromField('fileContents'),
      fromField('code'),
    )
    const actualResult = toFirst(lens, projectContents)
    if (isRight(actualResult)) {
      expect(actualResult.value).toMatchInlineSnapshot(`
        "{
          \\"name\\": \\"Utopia Project\\",
          \\"version\\": \\"0.1.0\\",
          \\"utopia\\": {
            \\"main-ui\\": \\"utopia/storyboard.js\\",
            \\"html\\": \\"public/index.html\\",
            \\"js\\": \\"src/index.js\\"
          },
          \\"dependencies\\": {
            \\"react\\": \\"16.13.1\\",
            \\"react-dom\\": \\"16.13.1\\",
            \\"utopia-api\\": \\"0.4.1\\"
          }
        }"
      `)
    } else {
      throw new Error(`Didn't get a value from the lens.`)
    }
  })
})
