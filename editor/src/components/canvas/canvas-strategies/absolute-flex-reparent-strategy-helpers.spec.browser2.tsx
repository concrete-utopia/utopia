/* eslint-disable jest/expect-expect */
import { act, fireEvent } from '@testing-library/react'
import { PrettierConfig } from 'utopia-vscode-common'
import {
  BakedInStoryboardVariableName,
  BakedInStoryboardUID,
} from '../../../core/model/scene-utils'
import * as EP from '../../../core/shared/element-path'
import { WindowPoint, windowPoint, offsetPoint } from '../../../core/shared/math-utils'
import { cmdModifier, emptyModifiers, Modifiers } from '../../../utils/modifiers'
import { wait } from '../../../utils/utils.test-utils'
import { selectComponents } from '../../editor/actions/action-creators'
import { CSSCursor } from '../canvas-types'
import { CanvasControlsContainerID } from '../controls/new-canvas-controls'
import { getCursorForOverlay } from '../controls/select-mode/cursor-overlay'
import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../ui-jsx.test-utils'

function dragElement(
  renderResult: EditorRenderResult,
  targetTestId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
  includeMouseUp: boolean,
): void {
  const targetElement = renderResult.renderedDOM.getByTestId(targetTestId)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControl = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = windowPoint({ x: targetElementBounds.x + 20, y: targetElementBounds.y + 20 })
  const endPoint = offsetPoint(startPoint, dragDelta)
  fireEvent(
    canvasControl,
    new MouseEvent('mousedown', {
      bubbles: true,
      cancelable: true,
      metaKey: modifiers.cmd,
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

  if (includeMouseUp) {
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
}

describe('Unified Reparent Fitness Function Tests', () => {
  beforeEach(() => {
    viewport.set(2200, 1000)
  })

  it('if an element is larger than its parent, we still allow reparent to its grandparent, if the reparenting starts from the area of the original parent', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: 'white',
            position: 'absolute',
            width: '100%',
            height: '100%',
          }}
          data-uid='aaa'
        >
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 40,
              top: 40,
              width: 100,
              height: 100,
            }}
            data-uid='bbb'
          >
            <div
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 200,
                height: 200,
              }}
              data-uid='ccc'
              data-testid='ccc'
            />
          </div>
        </div>
      `),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: 120, y: 0 })

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ccc'])
    await renderResult.dispatch([selectComponents([targetPath], false)], false)
    act(() => dragElement(renderResult, 'ccc', dragDelta, emptyModifiers, true))

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: 'white',
            position: 'absolute',
            width: '100%',
            height: '100%',
          }}
          data-uid='aaa'
        >
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 40,
              top: 40,
              width: 100,
              height: 100,
            }}
            data-uid='bbb'
          />
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 160,
              top: 40,
              width: 200,
              height: 200,
            }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </div>
      `),
    )
  })

  it('if an element is larger than its parent, we still allow reparent to its grandparent, if the reparenting starts from the area of the original parent, even if the original parent is not a viable parent', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: 'white',
            position: 'absolute',
            width: '100%',
            height: '100%',
          }}
          data-uid='aaa'
        >
          <div
            style={{
              backgroundColor: '#0091FFAA',
              left: 40,
              top: 40,
              width: 100,
              height: 100,
            }}
            data-uid='bbb'
          >
            <div
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 50,
                height: 50,
              }}
              data-uid='ccc'
              data-testid='ccc'
            />
          </div>
        </div>
      `),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: 120, y: 0 })

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ccc'])
    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    act(() => dragElement(renderResult, 'ccc', dragDelta, emptyModifiers, true))

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: 'white',
            position: 'absolute',
            width: '100%',
            height: '100%',
          }}
          data-uid='aaa'
        >
          <div
            style={{
              backgroundColor: '#0091FFAA',
              left: 40,
              top: 40,
              width: 100,
              height: 100,
            }}
            data-uid='bbb'
          />
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 120,
              top: 0,
              width: 50,
              height: 50,
            }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </div>
      `),
    )
  })
})

function getVariedProjectCodeWithAFlexContainer(flexDirection: string): string {
  return `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var Button = () => {
  return (
    <div
      data-uid='buttondiv'
      data-testid='buttondiv'
      data-label='buttondiv'
      style={{
        width: 100,
        height: 30,
        backgroundColor: 'pink',
      }}
    >
      BUTTON
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
          height: 500,
        }}
        data-uid='sceneroot'
      >
        <div
          style={{
            backgroundColor: 'purple',
            position: 'absolute',
            left: 21,
            top: 215.5,
            width: 123,
            height: 100,
          }}
          data-uid='seconddiv'
          data-testid='seconddiv'
          data-label='seconddiv'
        />
        <div
          style={{
            backgroundColor: 'green',
            height: 65,
            width: 66,
            position: 'absolute',
            left: 241,
            top: 142,
          }}
          data-uid='notdrag'
          data-testid='notdrag'
          data-label='notdrag'
        >
          not drag
        </div>
        <div
          style={{
            backgroundColor: '#0091FFAA',
            height: 111,
            width: 140,
            position: 'absolute',
            left: 210,
            top: 346,
          }}
          data-uid='dragme'
          data-testid='dragme'
          data-label='dragme'
        >
          <Button
            data-uid='button'
            data-testid='button'
            data-label='button'
          />
        </div>
      </div>
      <div
        style={{
          backgroundColor: 'grey',
          position: 'absolute',
          display: 'flex',
          flexDirection: '${flexDirection}',
          gap: '50px',
          left: 0,
          top: 500,
          width: 400,
          height: 200,
        }}
        data-uid='parentsibling'
        data-testid='parentsibling'
        data-label='parentsibling'
      >
        <div
          style={{
            backgroundColor: 'teal',
            position: 'relative',
            width: 109,
            height: 123,
          }}
          data-uid='firstdiv'
          data-testid='firstdiv'
          data-label='firstdiv'
        />
        <div
          style={{
            backgroundColor: 'green',
            position: 'relative',
            width: 118,
            height: 123,
          }}
          data-uid='thirddiv'
          data-testid='thirddiv'
          data-label='thirddiv'
        />
      </div>
    </Scene>
  </Storyboard>
)
`
}

async function checkReparentIndicator(
  renderResult: EditorRenderResult,
  expectedLeft: number,
  expectedTop: number,
  expectedWidth: number,
  expectedHeight: number,
): Promise<void> {
  const element = await renderResult.renderedDOM.findByTestId('flex-reparent-indicator-0')
  const bounds = element.getBoundingClientRect()
  expect(bounds.left).toEqual(expectedLeft)
  expect(bounds.top).toEqual(expectedTop)
  expect(bounds.width).toEqual(expectedWidth)
  expect(bounds.height).toEqual(expectedHeight)
}

describe('Reparent indicators', () => {
  beforeEach(() => {
    viewport.set(2200, 1000)
  })

  it(`shows the reparent indicator before all the elements in a 'row' container`, async () => {
    const renderResult = await renderTestEditorWithCode(
      getVariedProjectCodeWithAFlexContainer('row'),
      'await-first-dom-report',
    )

    // Select the target first.
    const targetPath = EP.fromString('storyboard/scene/sceneroot/seconddiv')
    await act(() => renderResult.dispatch([selectComponents([targetPath], false)], false))
    await renderResult.getDispatchFollowUpActionsFinished()

    // Start dragging the target.
    const targetElement = renderResult.renderedDOM.getByTestId('seconddiv')
    const targetElementBounds = targetElement.getBoundingClientRect()

    const flexContainer = renderResult.renderedDOM.getByTestId('parentsibling')
    const flexContainerBounds = flexContainer.getBoundingClientRect()

    const startPoint = { x: targetElementBounds.x + 20, y: targetElementBounds.y + 20 }
    const endPoint = {
      x: flexContainerBounds.x,
      y: flexContainerBounds.y + flexContainerBounds.height / 2,
    }
    const dragDelta = windowPoint({
      x: endPoint.x - startPoint.x,
      y: endPoint.y - startPoint.y,
    })

    act(() => dragElement(renderResult, 'seconddiv', dragDelta, emptyModifiers, false))

    await renderResult.getDispatchFollowUpActionsFinished()

    // Check the indicator presence and position.
    await checkReparentIndicator(renderResult, 431, 636, 2, 161.5)
  })

  it(`shows the reparent indicator before all the elements in a 'row-reverse' container`, async () => {
    const renderResult = await renderTestEditorWithCode(
      getVariedProjectCodeWithAFlexContainer('row-reverse'),
      'await-first-dom-report',
    )

    // Select the target first.
    const targetPath = EP.fromString('storyboard/scene/sceneroot/seconddiv')
    await act(() => renderResult.dispatch([selectComponents([targetPath], false)], false))
    await renderResult.getDispatchFollowUpActionsFinished()

    // Start dragging the target.
    const targetElement = renderResult.renderedDOM.getByTestId('seconddiv')
    const targetElementBounds = targetElement.getBoundingClientRect()

    const flexContainer = renderResult.renderedDOM.getByTestId('parentsibling')
    const flexContainerBounds = flexContainer.getBoundingClientRect()

    const startPoint = { x: targetElementBounds.x + 20, y: targetElementBounds.y + 20 }
    const endPoint = {
      x: flexContainerBounds.x + flexContainerBounds.width - 2,
      y: flexContainerBounds.y + flexContainerBounds.height / 2,
    }
    const dragDelta = windowPoint({
      x: endPoint.x - startPoint.x,
      y: endPoint.y - startPoint.y,
    })

    act(() => dragElement(renderResult, 'seconddiv', dragDelta, emptyModifiers, false))

    await renderResult.getDispatchFollowUpActionsFinished()

    // Check the indicator presence and position.
    await checkReparentIndicator(renderResult, 831, 636, 2, 161.5)
  })

  it(`shows the reparent indicator between two elements in a 'row' container`, async () => {
    const renderResult = await renderTestEditorWithCode(
      getVariedProjectCodeWithAFlexContainer('row'),
      'await-first-dom-report',
    )

    // Select the target first.
    const targetPath = EP.fromString('storyboard/scene/sceneroot/seconddiv')
    await act(() => renderResult.dispatch([selectComponents([targetPath], false)], false))
    await renderResult.getDispatchFollowUpActionsFinished()

    // Start dragging the target.
    const targetElement = renderResult.renderedDOM.getByTestId('seconddiv')
    const targetElementBounds = targetElement.getBoundingClientRect()

    const flexContainer = renderResult.renderedDOM.getByTestId('parentsibling')
    const flexContainerBounds = flexContainer.getBoundingClientRect()

    const startPoint = { x: targetElementBounds.x + 20, y: targetElementBounds.y + 20 }
    const endPoint = {
      x: flexContainerBounds.x + 130,
      y: flexContainerBounds.y + flexContainerBounds.height / 2,
    }
    const dragDelta = windowPoint({
      x: endPoint.x - startPoint.x,
      y: endPoint.y - startPoint.y,
    })

    act(() => dragElement(renderResult, 'seconddiv', dragDelta, emptyModifiers, false))

    await renderResult.getDispatchFollowUpActionsFinished()

    // Check the indicator presence and position.
    await checkReparentIndicator(renderResult, 565, 636, 2, 123)
  })

  it(`shows the reparent indicator between two elements in a 'row-reverse' container`, async () => {
    const renderResult = await renderTestEditorWithCode(
      getVariedProjectCodeWithAFlexContainer('row-reverse'),
      'await-first-dom-report',
    )

    // Select the target first.
    const targetPath = EP.fromString('storyboard/scene/sceneroot/seconddiv')
    await act(() => renderResult.dispatch([selectComponents([targetPath], false)], false))
    await renderResult.getDispatchFollowUpActionsFinished()

    // Start dragging the target.
    const targetElement = renderResult.renderedDOM.getByTestId('seconddiv')
    const targetElementBounds = targetElement.getBoundingClientRect()

    const flexContainer = renderResult.renderedDOM.getByTestId('parentsibling')
    const flexContainerBounds = flexContainer.getBoundingClientRect()

    const startPoint = { x: targetElementBounds.x + 20, y: targetElementBounds.y + 20 }
    const endPoint = {
      x: flexContainerBounds.x + flexContainerBounds.width - 130,
      y: flexContainerBounds.y + flexContainerBounds.height / 2,
    }
    const dragDelta = windowPoint({
      x: endPoint.x - startPoint.x,
      y: endPoint.y - startPoint.y,
    })

    act(() => dragElement(renderResult, 'seconddiv', dragDelta, emptyModifiers, false))

    await renderResult.getDispatchFollowUpActionsFinished()

    // Check the indicator presence and position.
    await checkReparentIndicator(renderResult, 697, 636, 2, 123)
  })
})
