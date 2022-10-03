import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../ui-jsx.test-utils'
import { CanvasControlsContainerID } from '../controls/new-canvas-controls'
import { cmdModifier, emptyModifiers, Modifiers } from '../../../utils/modifiers'
import { mouseClickAtPoint, mouseDragFromPointWithDelta } from '../event-helpers.test-utils'
import { rectangleDifference, windowPoint, WindowPoint } from '../../../core/shared/math-utils'
import * as EP from '../../../core/shared/element-path'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { assert } from 'chai'
import { wait } from '../../../utils/utils.test-utils'

const TestProjectBlockElements = `
<div style={{ width: '100%', height: '100%', position: 'absolute' }} data-uid='container'>
  <div
    style={{
      width: 50,
      height: 50,
      backgroundColor: '#CA1E4C80',
    }}
    data-uid='aaa'
    data-testid='aaa'
  />
  <div
    style={{
      width: 50,
      height: 50,
      backgroundColor: '#297374',
    }}
    data-uid='bbb'
    data-testid='bbb'
  />
  <div
    style={{
      width: 50,
      height: 50,
      backgroundColor: '#292E74',
    }}
    data-uid='ccc'
    data-testid='ccc'
  />
</div>
`

const TestProjectCCCDraggedToSecond = `
<div style={{ width: '100%', height: '100%', position: 'absolute' }} data-uid='container'>
  <div
    style={{
      width: 50,
      height: 50,
      backgroundColor: '#CA1E4C80',
    }}
    data-uid='aaa'
    data-testid='aaa'
  />
  <div
    style={{
      width: 50,
      height: 50,
      backgroundColor: '#292E74',
    }}
    data-uid='ccc'
    data-testid='ccc'
  />
  <div
    style={{
      width: 50,
      height: 50,
      backgroundColor: '#297374',
    }}
    data-uid='bbb'
    data-testid='bbb'
  />
</div>
`

const TestProjectMixedProperties = `
<div style={{ width: '100%', height: '100%' }} data-uid='container'>
  <div
    style={{
      width: 50,
      height: 50,
      backgroundColor: '#CA1E4C80',
    }}
    data-uid='aaa'
    data-testid='aaa'
  />
  <div
    style={{
      width: 50,
      height: 50,
      backgroundColor: '#297374',
    }}
    data-uid='bbb'
    data-testid='bbb'
  />
  <div
    style={{
      width: 50,
      height: 50,
      backgroundColor: '#292E74',
      display: 'inline-block',
    }}
    data-uid='ccc'
    data-testid='ccc'
  />
  <div
    style={{
      height: 80,
      width: 80,
      backgroundColor: '#D9DDAA80',
      display: 'inline-block',
    }}
    data-uid='ddd'
    data-testid='ddd'
  />
  <div
    style={{
      height: 80,
      width: 80,
      backgroundColor: '#DDAAB880',
      display: 'inline-block',
    }}
    data-uid='eee'
    data-testid='eee'
  />
</div>
`

function dragElement(
  renderResult: EditorRenderResult,
  targetTestId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
  expectedNavigatorTargetsDuringMove: Array<string>,
) {
  const targetElement = renderResult.renderedDOM.getByTestId(targetTestId)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
  mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
  mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, {
    modifiers: modifiers,
    midDragCallback: () => {
      expect(
        renderResult.getEditorState().derived.visibleNavigatorTargets.map(EP.toString),
      ).toEqual(expectedNavigatorTargetsDuringMove)
    },
  })
}

describe('Flow Reorder Strategy (Mixed Display Type)', () => {
  it('simple dragging the element in a block reorders it', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectBlockElements),
      'await-first-dom-report',
    )

    // drag element 'CCC' up a little to replace it with it's direct sibling
    const dragDelta = windowPoint({ x: 0, y: -45 })
    dragElement(renderResult, 'ccc', dragDelta, emptyModifiers, [
      'utopia-storyboard-uid/scene-aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity',
      'utopia-storyboard-uid/scene-aaa/app-entity:container',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
    ])

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(TestProjectCCCDraggedToSecond),
    )
  })
  it('dragging a flow element with mixed inline and block siblings is not allowed', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectMixedProperties),
      'await-first-dom-report',
    )

    // drag element 'CCC' up a little
    const dragDelta = windowPoint({ x: 0, y: -45 })
    dragElement(renderResult, 'ccc', dragDelta, emptyModifiers, [
      'utopia-storyboard-uid/scene-aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity',
      'utopia-storyboard-uid/scene-aaa/app-entity:container',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/eee',
    ])

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(TestProjectMixedProperties),
    )
  })

  it('dragging an element over a sibling with float:right will skip reorder', async () => {
    const TestCodeWithFloat = `
      <div style={{ width: 100, height: 50, position: 'absolute' }} data-uid='container'>
        <div
          style={{
            width: 50,
            height: 50,
            backgroundColor: '#CA1E4C80',
            float: 'right'
          }}
          data-uid='aaa'
          data-testid='aaa'
        />
        <div
          style={{
            width: 50,
            height: 50,
            backgroundColor: '#FF00FF',
          }}
          data-uid='bbb'
          data-testid='bbb'
        />
      </div>
    `
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestCodeWithFloat),
      'await-first-dom-report',
    )

    const elementAFrame = MetadataUtils.getFrameInCanvasCoords(
      EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/aaa'),
      renderResult.getEditorState().editor.jsxMetadata,
    )
    const elementBFrame = MetadataUtils.getFrameInCanvasCoords(
      EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/bbb'),
      renderResult.getEditorState().editor.jsxMetadata,
    )

    if (elementAFrame == null || elementBFrame == null) {
      assert.fail()
    } else {
      // drag element 'B' over 'A' will skip reorder
      const dragDelta = windowPoint(rectangleDifference(elementBFrame, elementAFrame))
      dragElement(renderResult, 'bbb', dragDelta, emptyModifiers, [
        'utopia-storyboard-uid/scene-aaa',
        'utopia-storyboard-uid/scene-aaa/app-entity',
        'utopia-storyboard-uid/scene-aaa/app-entity:container',
        'utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
        'utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      ])

      await renderResult.getDispatchFollowUpActionsFinished()
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(TestCodeWithFloat),
      )
    }
  })
})
