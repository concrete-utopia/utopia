import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'
import { offsetPoint, windowPoint, WindowPoint } from '../../../../core/shared/math-utils'
import { emptyModifiers, Modifiers } from '../../../../utils/modifiers'
import * as EP from '../../../../core/shared/element-path'
import { selectComponents } from '../../../editor/actions/action-creators'
import { IconSize } from '../../controls/flow-slider-control'
import { ReorderChangeThreshold } from './flow-reorder-helpers'
import { mouseDragFromPointWithDelta } from '../../event-helpers.test-utils'

const TestProject = `
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

const TestProjectCCCInlineBlock = `
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
      backgroundColor: '#DDAAB880',
      display: 'inline-block',
    }}
    data-uid='eee'
    data-testid='eee'
  />
</div>
`

function dragControl(
  renderResult: EditorRenderResult,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
  expectedNavigatorTargetsDuringMove: Array<string>,
) {
  const targetControl = renderResult.renderedDOM.getByTestId('flow-reorder-slider-control')
  const targetControlBounds = targetControl.getBoundingClientRect()

  const startPoint = {
    x: targetControlBounds.x + IconSize / 2,
    y: targetControlBounds.y + IconSize / 2,
  }

  mouseDragFromPointWithDelta(targetControl, startPoint, dragDelta, {
    modifiers: modifiers,
    midDragCallback: () => {
      expect(
        renderResult.getEditorState().derived.visibleNavigatorTargets.map(EP.toString),
      ).toEqual(expectedNavigatorTargetsDuringMove)
    },
  })
}

describe('Flow Reorder Slider Strategy', () => {
  it('dragging the control in a block reorders it', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProject),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [
        selectComponents(
          [EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/ccc')],
          false,
        ),
      ],
      true,
    )

    // drag control for 'CCC' left to replace it with it's direct sibling
    const dragDelta = windowPoint({ x: -ReorderChangeThreshold, y: 0 })
    dragControl(renderResult, dragDelta, emptyModifiers, [
      'utopia-storyboard-uid/scene-aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity',
      'utopia-storyboard-uid/scene-aaa/app-entity:container',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/eee',
    ])

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(TestProjectCCCDraggedToSecond),
    )
  })
  it('dragging a control for a block element, reorder is using conversion to inline row', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProject),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [
        selectComponents(
          [EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/ccc')],
          false,
        ),
      ],
      true,
    )

    // drag control for 'CCC' right, the element is inserted into a row with conversion to inline-block
    const dragDelta = windowPoint({ x: ReorderChangeThreshold, y: 0 })
    dragControl(renderResult, dragDelta, emptyModifiers, [
      'utopia-storyboard-uid/scene-aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity',
      'utopia-storyboard-uid/scene-aaa/app-entity:container',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/eee',
    ])

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(TestProjectCCCInlineBlock),
    )
  })
  it('dragging the control too far to the right restarts the reorder after reaching the end', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProject),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [
        selectComponents(
          [EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/ccc')],
          false,
        ),
      ],
      true,
    )

    // drag control for 'CCC' to the right
    const dragDelta = windowPoint({ x: ReorderChangeThreshold * 14, y: 0 })
    dragControl(renderResult, dragDelta, emptyModifiers, [
      'utopia-storyboard-uid/scene-aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity',
      'utopia-storyboard-uid/scene-aaa/app-entity:container',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/eee',
    ])

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(TestProjectCCCDraggedToSecond),
    )
  })
})
