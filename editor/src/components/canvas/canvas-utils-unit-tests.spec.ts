import * as TP from '../../core/shared/template-path'
import {
  makeTestProjectCodeWithSnippet,
  TestScenePath,
  testPrintCodeFromEditorState,
  getEditorState,
} from './ui-jsx.test-utils'
import { singleResizeChange, EdgePosition, pinMoveChange } from './canvas-types'
import { CanvasVector, canvasRectangle } from '../../core/shared/math-utils'
import { updateFramesOfScenesAndComponents } from './canvas-utils'
import { complexDefaultProject } from '../../sample-projects/sample-project-utils'
import { NO_OP } from '../../core/shared/utils'
import { editorModelFromPersistentModel } from '../editor/store/editor-state'

describe('updateFramesOfScenesAndComponents - multi-file', () => {
  it('a simple TLWH pin change works', async () => {
    const testProject = editorModelFromPersistentModel(complexDefaultProject(), NO_OP)
    const targetScenePath = TP.scenePath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
    ])
    const targetPath = TP.instancePath(targetScenePath, ['card-outer-div', 'card-inner-rectangle'])

    const pinChange = singleResizeChange(
      targetPath,
      { x: 1, y: 1 } as EdgePosition,
      { x: 60, y: 40 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(testProject, [pinChange], null)

    expect(testPrintCodeFromEditorState(updatedProject, '/src/card.js')).toMatchInlineSnapshot(`
      "/** @jsx jsx */
      import * as React from 'react'
      import { jsx, Rectangle } from 'utopia-api'
      export var Card = (props) => {
        return (
          <div data-uid='card-outer-div' style={{ ...props.style }}>
            <div
              data-uid='card-inner-div'
              style={{
                position: 'absolute',
                left: 0,
                top: 0,
                width: 50,
                height: 50,
                backgroundColor: 'red',
              }}
            />
            <Rectangle
              data-uid='card-inner-rectangle'
              style={{
                position: 'absolute',
                left: 100,
                top: 200,
                width: 110,
                height: 90,
                backgroundColor: 'blue',
              }}
            />
          </div>
        )
      }
      "
    `)
  })

  it('an element move works', async () => {
    const testProject = editorModelFromPersistentModel(complexDefaultProject(), NO_OP)
    const targetScenePath = TP.scenePath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
    ])
    const targetPath = TP.instancePath(targetScenePath, ['card-outer-div', 'card-inner-rectangle'])

    const pinChange = pinMoveChange(targetPath, { x: 60, y: 40 } as CanvasVector)

    const updatedProject = updateFramesOfScenesAndComponents(testProject, [pinChange], null)

    expect(testPrintCodeFromEditorState(updatedProject, '/src/card.js')).toMatchInlineSnapshot(`
      "/** @jsx jsx */
      import * as React from 'react'
      import { jsx, Rectangle } from 'utopia-api'
      export var Card = (props) => {
        return (
          <div data-uid='card-outer-div' style={{ ...props.style }}>
            <div
              data-uid='card-inner-div'
              style={{
                position: 'absolute',
                left: 0,
                top: 0,
                width: 50,
                height: 50,
                backgroundColor: 'red',
              }}
            />
            <Rectangle
              data-uid='card-inner-rectangle'
              style={{
                position: 'absolute',
                left: 160,
                top: 240,
                width: 50,
                height: 50,
                backgroundColor: 'blue',
              }}
            />
          </div>
        )
      }
      "
    `)
  })
})

describe('updateFramesOfScenesAndComponents - singleResizeChange -', () => {
  it('a simple TLWH pin change works', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 250, height: 300 }}
        layout={{ layoutSystem: 'pinSystem' }}
        data-uid='bbb'
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 60, y: 40 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 310, height: 340 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('TLW, missing H resizing from bottom right edge adds height', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256 }}
        layout={{ layoutSystem: 'pinSystem' }}
        data-uid='bbb'
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 40, y: 30 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 296, height: 30 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('TLWHBR, too many frame points work', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256, height: 202, bottom: 137, right: 93 }}
        layout={{ layoutSystem: 'pinSystem' }}
        data-uid='bbb'
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 50, y: 50 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid='aaa'>
        <View
          style={{
            backgroundColor: '#0091FFAA',
            left: 52,
            top: 61,
            width: 306,
            height: 252,
            bottom: 87,
            right: 43,
          }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  it('TLRB pin change works, dragged from topleft point', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, right: 50, bottom: 20 }}
        layout={{ layoutSystem: 'pinSystem' }}
        data-uid='bbb'
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      { x: 0, y: 0 } as EdgePosition,
      { x: 50, y: 20 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 2, top: 41, right: 50, bottom: 20 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('TLRB pin change works, dragged from bottom right point', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, right: 50, bottom: 20 }}
        layout={{ layoutSystem: 'pinSystem' }}
        data-uid='bbb'
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 80, y: -10 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, right: -30, bottom: 30 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  it('TLCxCy pin change works, dragged from topleft point', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', left: 52, top: 61 }}
        layout={{ layoutSystem: 'pinSystem', centerX: 100, centerY: 100 }}
        data-uid='bbb'
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      { x: 0, y: 0 } as EdgePosition,
      { x: 40, y: 30 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 12, top: 31 }}
          layout={{ layoutSystem: 'pinSystem', centerX: 80, centerY: 85 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('TLCxCy pin change works, dragged from bottomright point', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', left: 52, top: 61 }}
        layout={{ layoutSystem: 'pinSystem', centerX: 100, centerY: 100 }}
        data-uid='bbb'
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 40, y: 30 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61 }}
          layout={{ layoutSystem: 'pinSystem', centerX: 120, centerY: 115 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('a simple TLWH pin change with values in string pixels', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '50px', top: '50px', width: '250px', height: '350px' }}
        data-uid='bbb'
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 60, y: 40 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '50px', top: '50px', width: 310, height: 390 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('a simple TLWH pin change with expression, the expression is not changed', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50, top: 50, width: 200 + 50, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 60, y: 40 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50, top: 50, width: 200 + 50, height: 340 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('a simple TLWH pin change with values in exotic units', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '50pt', top: '5em', width: '50vw', height: '10cm' }}
        data-uid='bbb'
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 60, y: 40 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '50pt', top: '5em', width: '50vw', height: '10cm' }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
})
