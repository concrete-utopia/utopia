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
      { x: 1, y: 1 } as EdgePosition,
      { x: 60, y: 40 } as CanvasVector,
    )

    const transformedComponents = updateFramesOfScenesAndComponents(
      getComponentsFromTopLevelElements(testProject.topLevelElements),
      createFakeMetadataForParseSuccess(testProject),
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      false,
    )

    const updatedProject: ParseSuccess = {
      ...testProject,
      topLevelElements: transformedComponents.components,
    }

    expect(testPrintCode(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, height: 340, width: 310 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })
  it('TLW, missing H resizing from bottom right edge adds height', async () => {
    const testProject = getTestParseSuccess(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
      <View
        style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256 }}
        layout={{ layoutSystem: 'pinSystem' }}
        data-uid={'bbb'}
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 40, y: 30 } as CanvasVector,
    )

    const transformedComponents = updateFramesOfScenesAndComponents(
      getComponentsFromTopLevelElements(testProject.topLevelElements),
      createFakeMetadataForParseSuccess(testProject),
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      false,
    )

    const updatedProject: ParseSuccess = {
      ...testProject,
      topLevelElements: transformedComponents.components,
    }

    expect(testPrintCode(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, height: 30, width: 296 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })
  it('TLWHBR, too many frame points work', async () => {
    const testProject = getTestParseSuccess(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
      <View
        style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256, height: 202, bottom: 137, right: 93 }}
        layout={{ layoutSystem: 'pinSystem' }}
        data-uid={'bbb'}
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 50, y: 50 } as CanvasVector,
    )

    const transformedComponents = updateFramesOfScenesAndComponents(
      getComponentsFromTopLevelElements(testProject.topLevelElements),
      createFakeMetadataForParseSuccess(testProject),
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      false,
    )

    const updatedProject: ParseSuccess = {
      ...testProject,
      topLevelElements: transformedComponents.components,
    }

    expect(testPrintCode(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{
            backgroundColor: '#0091FFAA',
            left: 52,
            top: 61,
            height: 252,
            bottom: 87,
            width: 306,
            right: 43,
          }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })

  it('TLRB pin change works, dragged from topleft point', async () => {
    const testProject = getTestParseSuccess(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
      <View
        style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, right: 50, bottom: 20 }}
        layout={{ layoutSystem: 'pinSystem' }}
        data-uid={'bbb'}
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      { x: 0, y: 0 } as EdgePosition,
      { x: 50, y: 20 } as CanvasVector,
    )

    const transformedComponents = updateFramesOfScenesAndComponents(
      getComponentsFromTopLevelElements(testProject.topLevelElements),
      createFakeMetadataForParseSuccess(testProject),
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      false,
    )

    const updatedProject: ParseSuccess = {
      ...testProject,
      topLevelElements: transformedComponents.components,
    }

    expect(testPrintCode(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', right: 50, bottom: 20, top: 41, left: 2 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })
  it('TLRB pin change works, dragged from bottom right point', async () => {
    const testProject = getTestParseSuccess(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
      <View
        style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, right: 50, bottom: 20 }}
        layout={{ layoutSystem: 'pinSystem' }}
        data-uid={'bbb'}
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 80, y: -10 } as CanvasVector,
    )

    const transformedComponents = updateFramesOfScenesAndComponents(
      getComponentsFromTopLevelElements(testProject.topLevelElements),
      createFakeMetadataForParseSuccess(testProject),
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      false,
    )

    const updatedProject: ParseSuccess = {
      ...testProject,
      topLevelElements: transformedComponents.components,
    }

    expect(testPrintCode(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, bottom: 30, right: -30 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })

  it('TLCxCy pin change works, dragged from topleft point', async () => {
    const testProject = getTestParseSuccess(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
      <View
        style={{ backgroundColor: '#0091FFAA', left: 52, top: 61 }}
        layout={{ layoutSystem: 'pinSystem', centerX: 100, centerY: 100 }}
        data-uid={'bbb'}
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      { x: 0, y: 0 } as EdgePosition,
      { x: 40, y: 30 } as CanvasVector,
    )

    const transformedComponents = updateFramesOfScenesAndComponents(
      getComponentsFromTopLevelElements(testProject.topLevelElements),
      createFakeMetadataForParseSuccess(testProject),
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      false,
    )

    const updatedProject: ParseSuccess = {
      ...testProject,
      topLevelElements: transformedComponents.components,
    }

    expect(testPrintCode(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', top: 31, left: 12 }}
          layout={{ layoutSystem: 'pinSystem', centerY: 85, centerX: 80 }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })
  it('TLCxCy pin change works, dragged from bottomright point', async () => {
    const testProject = getTestParseSuccess(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
      <View
        style={{ backgroundColor: '#0091FFAA', left: 52, top: 61 }}
        layout={{ layoutSystem: 'pinSystem', centerX: 100, centerY: 100 }}
        data-uid={'bbb'}
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 40, y: 30 } as CanvasVector,
    )

    const { components: transformedComponents } = updateFramesOfScenesAndComponents(
      getComponentsFromTopLevelElements(testProject.topLevelElements),
      createFakeMetadataForParseSuccess(testProject),
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      false,
    )

    const updatedProject: ParseSuccess = {
      ...testProject,
      topLevelElements: transformedComponents,
    }

    expect(testPrintCode(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61 }}
          layout={{ layoutSystem: 'pinSystem', centerY: 115, centerX: 120 }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })
})
