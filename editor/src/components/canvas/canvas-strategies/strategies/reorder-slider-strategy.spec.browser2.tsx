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
import { IconSize } from '../../controls/reorder-slider-control'
import { ReorderChangeThreshold } from './flow-reorder-helpers'
import { mouseDragFromPointWithDelta } from '../../event-helpers.test-utils'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'

const TestProjectComplex = (additionalStyle: string = '') => `
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
      ${additionalStyle}
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

const TestProjectDraggedCCCInline = `
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

const TestProjectSimpleBlocks = `
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
      backgroundColor: '#297374',
    }}
    data-uid='ccc'
    data-testid='ccc'
  />
</div>
`

const TestProjectFlex = (flexWrap: 'wrap' | 'nowrap') => `
<div style={{ width: 200, height: '100%', position: 'absolute', display: 'flex', flexWrap: '${flexWrap}' }} data-uid='container'>
  <div
    style={{
      width: 100,
      height: 50,
      backgroundColor: '#CA1E4C80',
    }}
    data-uid='aaa'
    data-testid='aaa'
  />
  <div
    style={{
      width: 60,
      height: 50,
      backgroundColor: '#297374',
    }}
    data-uid='bbb'
    data-testid='bbb'
  />
  <div
    style={{
      width: 40,
      height: 50,
      backgroundColor: '#2D2974',
    }}
    data-uid='ccc'
    data-testid='ccc'
  />
  <div
    style={{
      width: 30,
      height: 50,
      backgroundColor: '#F8731B',
    }}
    data-uid='ddd'
    data-testid='ddd'
  />
</div>
`

function dragControl(
  renderResult: EditorRenderResult,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
  expectedNavigatorTargetsDuringMove: Array<string>,
) {
  const targetControl = renderResult.renderedDOM.getByTestId('reorder-slider-control')
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

describe('Reorder Slider Strategy', () => {
  it('dragging the control in a block reorders it', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectComplex()),
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
  it('dragging a control for a block element, after reorder to conversion result is inline-block', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectComplex()),
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
      makeTestProjectCodeWithSnippet(TestProjectDraggedCCCInline),
    )
  })
  it('dragging a control for an inline-block element, after reorder to conversion result is block', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectComplex(`display: 'inline-block'`)),
      'await-first-dom-report',
    )
    const target = EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/ccc')

    await renderResult.dispatch([selectComponents([target], false)], true)

    // drag control for 'CCC' to the left, the element is inserted into a column with conversion to block
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
    const draggedElementMetadata = MetadataUtils.findElementByElementPath(
      renderResult.getEditorState().editor.jsxMetadata,
      target,
    )
    const newDisplayValue = draggedElementMetadata?.specialSizeMeasurements.display
    expect(newDisplayValue).toEqual('block')
  })
  it('dragging a control for a flex element, after reorder to conversion result is inline flex', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectComplex(`display: 'flex'`)),
      'await-first-dom-report',
    )
    const target = EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/ccc')

    await renderResult.dispatch([selectComponents([target], false)], true)

    // drag control for 'CCC' right, the element is inserted into a row with conversion to inline-flex
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

    const draggedElementMetadata = MetadataUtils.findElementByElementPath(
      renderResult.getEditorState().editor.jsxMetadata,
      target,
    )
    const newDisplayValue = draggedElementMetadata?.specialSizeMeasurements.display
    expect(newDisplayValue).toEqual('inline-flex')
  })
  it('dragging a control for a inline-flex element, after reorder to conversion result is flex', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectComplex(`display: 'inline-flex'`)),
      'await-first-dom-report',
    )
    const target = EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/ccc')

    await renderResult.dispatch([selectComponents([target], false)], true)

    // drag control for 'CCC' left, the element is inserted into a column with conversion to flex
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

    const draggedElementMetadata = MetadataUtils.findElementByElementPath(
      renderResult.getEditorState().editor.jsxMetadata,
      target,
    )
    const newDisplayValue = draggedElementMetadata?.specialSizeMeasurements.display
    expect(newDisplayValue).toEqual('flex')
  })
  it('dragging the control too far to the right restarts the reorder after reaching the end', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectComplex()),
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

describe('Reorder Slider Strategy Control', () => {
  it('the reorder control is visible on wrapping flex layouts', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectFlex('wrap')),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [
        selectComponents(
          [EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/bbb')],
          false,
        ),
      ],
      true,
    )
    await renderResult.getDispatchFollowUpActionsFinished()

    const targetControl = renderResult.renderedDOM.getByTestId('reorder-slider-control')
    expect(targetControl).toBeDefined()
  })
  it('the reorder control is visible on wrapping flow layouts', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectComplex()),
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
    await renderResult.getDispatchFollowUpActionsFinished()

    const targetControl = renderResult.renderedDOM.getByTestId('reorder-slider-control')
    expect(targetControl).toBeDefined()
  })
  it('the reorder control is not visible on non-wrapping flex layouts', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectFlex('nowrap')),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [
        selectComponents(
          [EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/bbb')],
          false,
        ),
      ],
      true,
    )
    await renderResult.getDispatchFollowUpActionsFinished()

    const targetControl = renderResult.renderedDOM.queryByTestId('reorder-slider-control')
    expect(targetControl).toBeNull()
  })
  it('the reorder control is not visible on simple, 1d flow layouts', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectSimpleBlocks),
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
    await renderResult.getDispatchFollowUpActionsFinished()

    const targetControl = renderResult.renderedDOM.queryByTestId('reorder-slider-control')
    expect(targetControl).toBeNull()
  })
})
