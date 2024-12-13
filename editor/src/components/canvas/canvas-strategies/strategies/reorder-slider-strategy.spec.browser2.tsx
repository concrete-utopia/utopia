import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'
import type { WindowPoint } from '../../../../core/shared/math-utils'
import { windowPoint } from '../../../../core/shared/math-utils'
import type { Modifiers } from '../../../../utils/modifiers'
import { emptyModifiers } from '../../../../utils/modifiers'
import * as EP from '../../../../core/shared/element-path'
import {
  duplicateSelected,
  selectComponents,
  setHoveredView,
} from '../../../editor/actions/action-creators'
import { IconSize } from '../../controls/reorder-slider-control'
import { ReorderChangeThreshold } from './flow-reorder-helpers'
import {
  mouseDownAtPoint,
  mouseDragFromPointWithDelta,
  mouseMoveToPoint,
} from '../../event-helpers.test-utils'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { navigatorEntryToKey } from '../../../../components/editor/store/editor-state'
import { getNavigatorTargetsFromEditorState } from '../../../navigator/navigator-utils'

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

async function dragControl(
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

  await mouseDragFromPointWithDelta(targetControl, startPoint, dragDelta, {
    modifiers: modifiers,
    midDragCallback: async () => {
      expect(
        getNavigatorTargetsFromEditorState(
          renderResult.getEditorState().editor,
        ).visibleNavigatorTargets.map(navigatorEntryToKey),
      ).toEqual(expectedNavigatorTargetsDuringMove)
    },
  })
}

async function selectAndHover(pathString: string, renderResult: EditorRenderResult): Promise<void> {
  const path = EP.fromString(pathString)
  return renderResult.dispatch([selectComponents([path], false), setHoveredView(path)], true)
}

describe('Reorder Slider Strategy', () => {
  it('dragging the control in a block reorders it', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectComplex()),
      'await-first-dom-report',
    )

    await selectAndHover('utopia-storyboard-uid/scene-aaa/app-entity:container/ccc', renderResult)

    // drag control for 'CCC' left to replace it with it's direct sibling
    const dragDelta = windowPoint({ x: -ReorderChangeThreshold, y: 0 })
    await dragControl(renderResult, dragDelta, emptyModifiers, [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/eee',
    ])

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(TestProjectCCCDraggedToSecond),
    )
  })
  it('dragging the control too far to the right restarts the reorder after reaching the end', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectComplex()),
      'await-first-dom-report',
    )

    await selectAndHover('utopia-storyboard-uid/scene-aaa/app-entity:container/ccc', renderResult)

    // drag control for 'CCC' to the right
    const dragDelta = windowPoint({ x: ReorderChangeThreshold * 14, y: 0 })
    await dragControl(renderResult, dragDelta, emptyModifiers, [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/eee',
    ])

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(TestProjectCCCDraggedToSecond),
    )
  })
})

describe('Reorder Slider Strategy Control', () => {
  it('is not visible when not hovering over the selected element', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectFlex('wrap')),
      'await-first-dom-report',
    )

    // Directly trigger selection
    await renderResult.dispatch(
      [
        selectComponents(
          [EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/bbb')],
          false,
        ),
      ],
      true,
    )

    const targetControl = renderResult.renderedDOM.queryByTestId('reorder-slider-control')
    expect(targetControl).toBeNull()
  })

  it('is visible when hovering over the selected element', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectFlex('wrap')),
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementRect = targetElement.getBoundingClientRect()
    const targetElementCenter = {
      x: targetElementRect.x + targetElementRect.width / 2,
      y: targetElementRect.y + targetElementRect.height / 2,
    }

    // Directly trigger selection
    await renderResult.dispatch(
      [
        selectComponents(
          [EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/bbb')],
          false,
        ),
      ],
      true,
    )

    // Now hover over the element
    await mouseMoveToPoint(canvasControlsLayer, targetElementCenter)

    const targetControl = renderResult.renderedDOM.queryByTestId('reorder-slider-control')
    expect(targetControl).not.toBeNull()
  })

  it('is visible when dragging the control even when not hovering the selected element', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectFlex('wrap')),
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementRect = targetElement.getBoundingClientRect()
    const targetElementCenter = {
      x: targetElementRect.x + targetElementRect.width / 2,
      y: targetElementRect.y + targetElementRect.height / 2,
    }

    // Directly trigger selection
    await renderResult.dispatch(
      [
        selectComponents(
          [EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/bbb')],
          false,
        ),
      ],
      true,
    )

    // Now hover over the element to display the control
    await mouseMoveToPoint(canvasControlsLayer, targetElementCenter)

    // Find the control
    const targetControlAtStart = renderResult.renderedDOM.getByTestId('reorder-slider-control')
    const targetControlBounds = targetControlAtStart.getBoundingClientRect()

    const startPoint = {
      x: targetControlBounds.x + IconSize / 2,
      y: targetControlBounds.y + IconSize / 2,
    }

    const dragDelta = {
      x: targetElementRect.width * 10,
      y: targetElementRect.height * 10,
    }

    const endPoint = {
      x: startPoint.x + dragDelta.x,
      y: startPoint.y + dragDelta.y,
    }

    // Mouse down and start dragging the control way beyond the bounds of the target element
    await mouseDownAtPoint(targetControlAtStart, startPoint)
    await mouseMoveToPoint(targetControlAtStart, endPoint, {
      eventOptions: {
        movementX: dragDelta.x,
        movementY: dragDelta.y,
        buttons: 1,
      },
    })

    // Ensure the control is still showing
    const targetControlAtEnd = renderResult.renderedDOM.queryByTestId('reorder-slider-control')
    expect(targetControlAtEnd).not.toBeNull()
  })

  it('the reorder control is visible on wrapping flex layouts', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectFlex('wrap')),
      'await-first-dom-report',
    )

    await selectAndHover('utopia-storyboard-uid/scene-aaa/app-entity:container/bbb', renderResult)

    const targetControl = renderResult.renderedDOM.getByTestId('reorder-slider-control')
    expect(targetControl).toBeDefined()
  })
  it('the reorder control is visible on wrapping flow layouts', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectComplex()),
      'await-first-dom-report',
    )

    await selectAndHover('utopia-storyboard-uid/scene-aaa/app-entity:container/ccc', renderResult)

    const targetControl = renderResult.renderedDOM.getByTestId('reorder-slider-control')
    expect(targetControl).toBeDefined()
  })
  it('the reorder control is not visible on non-wrapping flex layouts', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectFlex('nowrap')),
      'await-first-dom-report',
    )

    await selectAndHover('utopia-storyboard-uid/scene-aaa/app-entity:container/bbb', renderResult)

    const targetControl = renderResult.renderedDOM.queryByTestId('reorder-slider-control')
    expect(targetControl).toBeNull()
  })
  it('the reorder control is not visible on simple, 1d flow layouts', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectSimpleBlocks),
      'await-first-dom-report',
    )

    await selectAndHover('utopia-storyboard-uid/scene-aaa/app-entity:container/ccc', renderResult)

    const targetControl = renderResult.renderedDOM.queryByTestId('reorder-slider-control')
    expect(targetControl).toBeNull()
  })
  it('the reorder control is not visible on simple, 1d flow layouts for metadata where the key insertion order does not match the code order', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectSimpleBlocks),
      'await-first-dom-report',
    )

    // Select and duplicate the first element, as that will modify the metadata in a way that
    // the Object.keys order won't match the children order

    const firstElementPath = 'utopia-storyboard-uid/scene-aaa/app-entity:container/aaa'
    const path = EP.fromString(firstElementPath)
    await renderResult.dispatch([selectComponents([path], false)], true)
    await renderResult.dispatch([duplicateSelected()], true)
    await renderResult.getDispatchFollowUpActionsFinished()

    await selectAndHover(firstElementPath, renderResult)

    const targetControl = renderResult.renderedDOM.queryByTestId('reorder-slider-control')
    expect(targetControl).toBeNull()
  })
})
