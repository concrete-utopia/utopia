import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../ui-jsx.test-utils'
import { act, fireEvent } from '@testing-library/react'
import * as EP from '../../../core/shared/element-path'
import { selectComponents } from '../../editor/actions/action-creators'
import { CanvasControlsContainerID } from '../controls/new-canvas-controls'
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
import { emptyModifiers } from '../../../utils/modifiers'
import { ElementPath } from '../../../core/shared/project-file-types'
import { wait } from '../../../utils/utils.test-utils'

function dragElement(
  canvasControl: HTMLElement,
  startPoint: WindowPoint,
  dragDelta: WindowPoint,
  cmdPressed: boolean,
  altPressed: boolean,
  shiftPressed: boolean,
) {
  const endPoint = offsetPoint(startPoint, dragDelta)
  fireEvent(
    canvasControl,
    new MouseEvent('mousedown', {
      bubbles: true,
      cancelable: true,
      metaKey: true,
      altKey: altPressed,
      shiftKey: shiftPressed,
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
      metaKey: cmdPressed,
      altKey: altPressed,
      shiftKey: shiftPressed,
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
      metaKey: cmdPressed,
      altKey: altPressed,
      shiftKey: shiftPressed,
      clientX: endPoint.x,
      clientY: endPoint.y,
    }),
  )
}

// no mouseup here! it starts the interaction and moves it with drag delta
async function startDragUsingActions(
  renderResult: any,
  target: ElementPath,
  dragDelta: CanvasVector,
) {
  await renderResult.dispatch([selectComponents([target], false)], true)
  const startInteractionSession = createInteractionViaMouse(zeroCanvasPoint, emptyModifiers, {
    type: 'BOUNDING_AREA',
    target: target,
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
          type: 'BOUNDING_AREA',
          target: target,
        }),
      ),
    ],
    false,
  )
  await renderResult.getDispatchFollowUpActionsFinished()
}

describe('Absolute Move Strategy', () => {
  it('moves absolute positioned element', async () => {
    const startX = 40
    const startY = 50
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: ${startX}, top: ${startY}, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 40, y: -25 })

    act(() => dragElement(canvasControlsLayer, startPoint, dragDelta, false, false, false))

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: ${
              startX + dragDelta.x
            }, top: ${startY + dragDelta.y}, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })
  it('moves absolute element with snapping, `bbb` should snap to `ccc`', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 100, height: 30 }}
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

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 9, y: -23 }) // 'bbb' will snap to bottom edge and middle of 'ccc'

    act(() => dragElement(canvasControlsLayer, startPoint, dragDelta, false, false, false))

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 100, height: 30 }}
            data-uid='ccc'
          />
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50, top: 30, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })
  it('moves absolute element without snapping while pressing cmd `bbb` should not snap to `ccc`', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 100, height: 30 }}
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

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 9, y: -23 })

    act(() => dragElement(canvasControlsLayer, startPoint, dragDelta, true, false, false))

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 100, height: 30 }}
            data-uid='ccc'
          />
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 49, top: 27, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })
})

describe('Absolute Move Strategy Canvas Controls', () => {
  it('when an absolute positioned element is started to be moved parent outlines become visible', async () => {
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
    await startDragUsingActions(renderResult, target, zeroCanvasPoint)

    const parentOutlineControl = renderResult.renderedDOM.getByTestId('parent-outlines-control')
    expect(parentOutlineControl).toBeDefined()
    const parentBoundsControl = renderResult.renderedDOM.getByTestId('parent-bounds-control')
    expect(parentBoundsControl).toBeDefined()
  })
  it('when an absolute positioned element is selected the pin lines are visible', async () => {
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

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      true,
    )

    const pinLineLeft = renderResult.renderedDOM.getByTestId('pin-line-left')
    expect(pinLineLeft).toBeDefined()
    const pinLineTop = renderResult.renderedDOM.getByTestId('pin-line-top')
    expect(pinLineTop).toBeDefined()
    const pinLineRight = renderResult.renderedDOM.getByTestId('pin-line-right')
    expect(pinLineRight).toBeDefined()
    const pinLineBottom = renderResult.renderedDOM.getByTestId('pin-line-bottom')
    expect(pinLineBottom).toBeDefined()
  })
  it('the snap guidelines are visible when an absolute positioned element(bbb) is dragged and snaps to its sibling (ccc)', async () => {
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

    await startDragUsingActions(renderResult, target, dragDelta)

    expect(renderResult.renderedDOM.getByTestId('guideline-0').style.display).toEqual('block')
    expect(renderResult.renderedDOM.getByTestId('guideline-1').style.display).toEqual('block')
  })
})
