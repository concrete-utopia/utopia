import {
  formatTestProjectCode,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
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
import { BakedInStoryboardUID } from '../../../core/model/scene-utils'
import { SceneLabelTestID } from '../controls/select-mode/scene-label'

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
  before(() => {
    viewport.set(2200, 1000)
  })
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
  it('moves absolute positioned scene', async () => {
    const startX = 40
    const startY = 50
    const renderResult = await renderTestEditorWithCode(
      `
        import * as React from 'react'
        import {
          Scene,
          Storyboard,
        } from 'utopia-api'
        export var App = (props) => {
          return (
            <div data-uid='aaa' />
          )
        }
        export var storyboard = (props) => {
          return (
            <Storyboard data-uid='${BakedInStoryboardUID}'>
              <Scene
                style={{ position: 'absolute', left: ${startX}, top: ${startY}, width: 375, height: 812 }}
                data-uid='${TestSceneUID}'
              >
                <App
                  data-uid='${TestAppUID}'
                  style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
                />
              </Scene>
            </Storyboard>
          )
        }
      `,
      'await-first-dom-report',
    )

    const sceneLabel = renderResult.renderedDOM.getByTestId(SceneLabelTestID)
    const sceneLabelBounds = sceneLabel.getBoundingClientRect()

    const startPoint = windowPoint({ x: sceneLabelBounds.x + 5, y: sceneLabelBounds.y + 5 })
    const dragDelta = windowPoint({ x: 40, y: -25 })

    act(() => dragElement(sceneLabel, startPoint, dragDelta, false, false, false))

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      formatTestProjectCode(`
        import * as React from 'react'
        import {
          Scene,
          Storyboard,
        } from 'utopia-api'
        export var App = (props) => {
          return (
            <div data-uid='aaa' />
          )
        }
        export var storyboard = (props) => {
          return (
            <Storyboard data-uid='${BakedInStoryboardUID}'>
              <Scene
                style={{ position: 'absolute', left: ${startX + dragDelta.x}, top: ${
        startY + dragDelta.y
      }, width: 375, height: 812 }}
                data-uid='${TestSceneUID}'
              >
                <App
                  data-uid='${TestAppUID}'
                  style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
                />
              </Scene>
            </Storyboard>
          )
        }
    `),
    )
  })
  it('moves selected element even when it is covered with a non-selected one', async () => {
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
          <div
            style={{ backgroundColor: 'red', position: 'absolute', left: ${startX}, top: ${startY}, width: 200, height: 120 }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      true,
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
          <div
            style={{ backgroundColor: 'red', position: 'absolute', left: ${startX}, top: ${startY}, width: 200, height: 120 }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </div>
      `),
    )
  })
  it('moves multiselection when dragging one of the selected elements', async () => {
    const startX1 = 40
    const startY1 = 50
    const startX2 = 80
    const startY2 = 100
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: ${startX1}, top: ${startY1}, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{ backgroundColor: 'red', position: 'absolute', left: ${startX2}, top: ${startY2}, width: 200, height: 120 }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [
        selectComponents(
          [
            EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
            EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc']),
          ],
          false,
        ),
      ],
      true,
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
              startX1 + dragDelta.x
            }, top: ${startY1 + dragDelta.y}, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{ backgroundColor: 'red', position: 'absolute', left: ${
              startX2 + dragDelta.x
            }, top: ${startY2 + dragDelta.y}, width: 200, height: 120 }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </div>
      `),
    )
  })
  it('moves multiselection when dragging a non-selected element inside the selection area', async () => {
    const startX1 = 40
    const startY1 = 50
    const startX2 = 122
    const startY2 = 50
    const startX3 = 122
    const startY3 = 130
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: ${startX1}, top: ${startY1}, width: 60, height: 60 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{ backgroundColor: 'red', position: 'absolute', left: ${startX2}, top: ${startY2}, width: 60, height: 60 }}
            data-uid='ccc'
            data-testid='ccc'
          />
          <div
            style={{ backgroundColor: 'green', position: 'absolute', left: ${startX3}, top: ${startY3}, width: 60, height: 60 }}
            data-uid='ddd'
            data-testid='ddd'
          />
          
        </div>
      `),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [
        selectComponents(
          [
            EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
            EP.appendNewElementPath(TestScenePath, ['aaa', 'ddd']),
          ],
          false,
        ),
      ],
      true,
    )

    const targetElement = renderResult.renderedDOM.getByTestId('ccc')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 41, y: -26 })

    act(() => dragElement(canvasControlsLayer, startPoint, dragDelta, false, false, false))

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: ${
              startX1 + dragDelta.x
            }, top: ${startY1 + dragDelta.y}, width: 60, height: 60 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{ backgroundColor: 'red', position: 'absolute', left: ${startX2}, top: ${startY2}, width: 60, height: 60 }}
            data-uid='ccc'
            data-testid='ccc'
          />
          <div
            style={{ backgroundColor: 'green', position: 'absolute', left: ${
              startX3 + dragDelta.x
            }, top: ${startY3 + dragDelta.y}, width: 60, height: 60 }}
            data-uid='ddd'
            data-testid='ddd'
          />
        </div>
      `),
    )
  })
  it('ignores selection when dragging element outside of the selection area', async () => {
    const startX1 = 0
    const startY1 = 0
    const startX2 = 210
    const startY2 = 220
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: ${startX1}, top: ${startY1}, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{ backgroundColor: 'red', position: 'absolute', left: ${startX2}, top: ${startY2}, width: 200, height: 120 }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc'])], false)],
      true,
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
              startX1 + dragDelta.x
            }, top: ${startY1 + dragDelta.y}, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{ backgroundColor: 'red', position: 'absolute', left: ${startX2}, top: ${startY2}, width: 200, height: 120 }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </div>
      `),
    )
  })
  it('fills in missing props of absolute positioned element', async () => {
    const parentMargin = 100
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          position: 'absolute',
          width: '100%',
          height: '100%',
          backgroundColor: '#FFFFFF',
        }}
      >
        <div
          style={{ left: 50, top: 50, margin: ${parentMargin}}}
          data-uid='ccc'
        >
          <div
            data-uid='bbb'
            data-testid='bbb'
            style={{
              position: 'absolute',
              width: 200,
              height: 300,
              backgroundColor: '#d3d3d3',
            }}
          />
        </div>
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
      <div
        data-uid='aaa'
        style={{
          position: 'absolute',
          width: '100%',
          height: '100%',
          backgroundColor: '#FFFFFF',
        }}
      >
        <div
          style={{ left: 50, top: 50, margin: ${parentMargin} }}
          data-uid='ccc'
        >
          <div
            data-uid='bbb'
            data-testid='bbb'
            style={{
              position: 'absolute',
              width: 200,
              height: 300,
              backgroundColor: '#d3d3d3',
              left: ${parentMargin + dragDelta.x},
              top: ${parentMargin + dragDelta.y},
            }}
          />
        </div>
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
