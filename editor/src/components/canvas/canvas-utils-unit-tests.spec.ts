import * as TP from '../../core/shared/template-path'
import {
  getTestParseSuccess,
  makeTestProjectCodeWithSnippet,
  TestScenePath,
  testPrintCode,
} from './ui-jsx-test-utils'
import { singleResizeChange, EdgePosition } from './canvas-types'
import { CanvasVector, canvasRectangle } from '../../core/shared/math-utils'
import { updateFramesOfScenesAndComponents } from './canvas-utils'
import { ParseSuccess } from '../../core/shared/project-file-types'
import { getComponentsFromTopLevelElements } from '../../core/model/project-file-utils'
import { createFakeMetadataForParseSuccess } from '../../utils/test-utils'

describe('updateFramesOfScenesAndComponents - singleResizeChange -', () => {
  it('a simple TLWH pin change works', async () => {
    const testProject = getTestParseSuccess(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
      <View
        style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 250, height: 300 }}
        layout={{ layoutSystem: 'pinSystem' }}
        data-uid={'bbb'}
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      { x: -20, y: -10 } as CanvasVector,
      { x: 40, y: 30 } as CanvasVector,
      { x: 1, y: 1 } as EdgePosition,
    )

    const transformedComponents = updateFramesOfScenesAndComponents(
      getComponentsFromTopLevelElements(testProject.topLevelElements),
      createFakeMetadataForParseSuccess(testProject),
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    const updatedProject: ParseSuccess = {
      ...testProject,
      topLevelElements: transformedComponents,
    }

    expect(testPrintCode(updatedProject)).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', height: 340, top: 40, width: 310, left: 30 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })
})
