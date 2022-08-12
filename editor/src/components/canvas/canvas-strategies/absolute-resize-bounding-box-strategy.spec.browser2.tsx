import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../ui-jsx.test-utils'
import { act, fireEvent } from '@testing-library/react'
import * as EP from '../../../core/shared/element-path'
import { selectComponents } from '../../editor/actions/action-creators'
import CanvasActions from '../canvas-actions'
import { createInteractionViaMouse, updateInteractionViaMouse } from './interaction-state'
import {
  canvasPoint,
  CanvasVector,
  offsetPoint,
  windowPoint,
  WindowPoint,
  zeroCanvasPoint,
} from '../../../core/shared/math-utils'
import { emptyModifiers, Modifiers } from '../../../utils/modifiers'
import { ElementPath } from '../../../core/shared/project-file-types'
import {
  EdgePositionBottomRight,
  EdgePosition,
  EdgePositionLeft,
  EdgePositionTopLeft,
} from '../canvas-types'
import { setupTearDownClock, wait } from '../../../utils/utils.test-utils'
import { ControlDelay } from './canvas-strategy-types'

function selectAndResizeElement(
  renderResult: EditorRenderResult,
  dragDelta: WindowPoint,
  edgePosition: EdgePosition,
  modifiers: Modifiers,
): void {
  const canvasControl = renderResult.renderedDOM.queryByTestId(
    `absolute-resize-${edgePosition.x}-${edgePosition.y}`,
  )
  if (canvasControl == null) {
    return
  }

  const resizeCornerBounds = canvasControl.getBoundingClientRect()
  const startPoint = windowPoint({
    x: resizeCornerBounds.x + 2,
    y: resizeCornerBounds.y + 2,
  })
  const endPoint = offsetPoint(startPoint, dragDelta)

  fireEvent(
    canvasControl,
    new MouseEvent('mousedown', {
      bubbles: true,
      cancelable: true,
      metaKey: true,
      altKey: modifiers.alt,
      shiftKey: modifiers.shift,
      clientX: startPoint.x,
      clientY: startPoint.y,
      buttons: 1,
    }),
  )

  fireEvent(
    canvasControl,
    new MouseEvent('mousemove', {
      bubbles: true,
      cancelable: true,
      metaKey: modifiers.cmd,
      altKey: modifiers.alt,
      shiftKey: modifiers.shift,
      clientX: endPoint.x,
      clientY: endPoint.y,
      buttons: 1,
    }),
  )

  fireEvent(
    window,
    new MouseEvent('mouseup', {
      bubbles: true,
      cancelable: true,
      metaKey: modifiers.cmd,
      altKey: modifiers.alt,
      shiftKey: modifiers.shift,
      clientX: endPoint.x,
      clientY: endPoint.y,
    }),
  )
}

// no mouseup here! it starts the interaction and resizes with drag delta
async function startDragUsingActions(
  renderResult: EditorRenderResult,
  target: ElementPath,
  edgePosition: EdgePosition,
  dragDelta: CanvasVector,
) {
  await renderResult.dispatch([selectComponents([target], false)], true)
  const startInteractionSession = createInteractionViaMouse(zeroCanvasPoint, emptyModifiers, {
    type: 'RESIZE_HANDLE',
    edgePosition: edgePosition,
  })
  await renderResult.dispatch(
    [CanvasActions.createInteractionSession(startInteractionSession)],
    false,
  )
  await renderResult.getDispatchFollowUpActionsFinished()
  await renderResult.dispatch(
    [
      CanvasActions.updateInteractionSession(
        updateInteractionViaMouse(startInteractionSession, dragDelta, emptyModifiers, {
          type: 'RESIZE_HANDLE',
          edgePosition: edgePosition,
        }),
      ),
    ],
    false,
  )
  await renderResult.getDispatchFollowUpActionsFinished()
}

const doesOrNotSupportStyleCode = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export const SupportsStyle = (props) => {
  return (
    <div
      style={{
        backgroundColor: 'teal',
        position: 'absolute',
        left: 0,
        top: 200,
        width: 100,
        height: 100,
        ...props.style,
      }}
      data-uid='supports-style-component'
      data-testid='supports-style-component'
    >
      DOES
    </div>
  )
}

export const DoesNotSupportStyle = (props) => {
  return (
    <div
      style={{
        backgroundColor: 'lightgreen',
        position: 'absolute',
        left: 100,
        top: 200,
        width: 100,
        height: 100,
      }}
      data-uid='does-not-support-style-component'
      data-testid='does-not-support-style-component'
    >
      DOES NOT
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      style={{
        backgroundColor: 'white',
        position: 'absolute',
        left: 0,
        top: 0,
        width: 400,
        height: 700,
      }}
      data-uid='scene'
    >
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 0,
          width: 400,
          height: 700,
        }}
        data-uid='containing-div'
      >
        <SupportsStyle
          data-uid='supports-style'
          data-testid='supports-style'
        />
        <DoesNotSupportStyle
          data-uid='does-not-support-style'
          data-testid='does-not-support-style'
        />
      </div>
    </Scene>
  </Storyboard>
)`

describe('Absolute Resize Strategy', () => {
  it('resizes absolute positioned element from bottom right edge', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    const dragDelta = windowPoint({ x: 40, y: -25 })

    await renderResult.dispatch([selectComponents([target], false)], true)
    act(() =>
      selectAndResizeElement(renderResult, dragDelta, EdgePositionBottomRight, emptyModifiers),
    )

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 50, width: 240, height: 95 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })
  it('resizes absolute element with snapping, `bbb` should snap to `ccc`', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 70, height: 30 }}
            data-uid='ccc'
          />
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    const dragDelta = windowPoint({ x: 29, y: -23 })

    await renderResult.dispatch([selectComponents([target], false)], true)
    act(() => selectAndResizeElement(renderResult, dragDelta, EdgePositionTopLeft, emptyModifiers))
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 70, height: 30 }}
            data-uid='ccc'
          />
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 70, top: 30, width: 170, height: 140 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })
  it('handles checking if a component supports the style property (for those that do)', async () => {
    const renderResult = await renderTestEditorWithCode(
      doesOrNotSupportStyleCode,
      'await-first-dom-report',
    )

    const target = EP.fromString('storyboard/scene/containing-div/supports-style')
    const dragDelta = windowPoint({ x: 25, y: 25 })

    await renderResult.dispatch([selectComponents([target], false)], true)
    act(() =>
      selectAndResizeElement(renderResult, dragDelta, EdgePositionBottomRight, emptyModifiers),
    )
    await renderResult.getDispatchFollowUpActionsFinished()

    const supportsStyleDiv = renderResult.renderedDOM.getByTestId('supports-style-component')
    const supportsStyleRect = supportsStyleDiv.getBoundingClientRect()
    expect(supportsStyleRect.width).toEqual(125)
    expect(supportsStyleRect.height).toEqual(125)
  })
  it('handles checking if a component supports the style property (for those that do not)', async () => {
    const renderResult = await renderTestEditorWithCode(
      doesOrNotSupportStyleCode,
      'await-first-dom-report',
    )

    const target = EP.fromString('storyboard/scene/containing-div/does-not-support-style')
    const dragDelta = windowPoint({ x: 25, y: 25 })

    await renderResult.dispatch([selectComponents([target], false)], true)
    act(() =>
      selectAndResizeElement(renderResult, dragDelta, EdgePositionBottomRight, emptyModifiers),
    )
    await renderResult.getDispatchFollowUpActionsFinished()

    const supportsStyleDiv = renderResult.renderedDOM.getByTestId(
      'does-not-support-style-component',
    )
    const supportsStyleRect = supportsStyleDiv.getBoundingClientRect()
    expect(supportsStyleRect.width).toEqual(100)
    expect(supportsStyleRect.height).toEqual(100)
  })
})

describe('Absolute Resize Strategy Canvas Controls', () => {
  const { clock } = setupTearDownClock()
  it('when an absolute positioned element is resized the parent outlines become visible', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 50, right: 160, bottom: 230 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const parentOutlineControlBeforeDrag =
      renderResult.renderedDOM.queryByTestId('parent-outlines-control')
    expect(parentOutlineControlBeforeDrag).toBeNull()
    const parentBoundsControlBeforeDrag =
      renderResult.renderedDOM.queryByTestId('parent-bounds-control')
    expect(parentBoundsControlBeforeDrag).toBeNull()

    const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    await startDragUsingActions(renderResult, target, EdgePositionLeft, zeroCanvasPoint)

    clock.current.tick(ControlDelay + 10)
    const parentOutlineControl = renderResult.renderedDOM.getByTestId('parent-outlines-control')
    expect(parentOutlineControl).toBeDefined()
    const parentBoundsControl = renderResult.renderedDOM.getByTestId('parent-bounds-control')
    expect(parentBoundsControl).toBeDefined()
  })
  it('snap guidelines are visible when an absolute positioned element(bbb) is resized and snaps to its sibling (ccc)', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 70, height: 30 }}
            data-uid='ccc'
          />
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    const dragDelta = canvasPoint({ x: 29, y: -23 }) // 'bbb' will snap to bottom right corner of 'ccc'

    await startDragUsingActions(renderResult, target, EdgePositionTopLeft, dragDelta)

    expect(renderResult.renderedDOM.getByTestId('guideline-0').style.display).toEqual('block')
    expect(renderResult.renderedDOM.getByTestId('guideline-1').style.display).toEqual('block')
  })
})
