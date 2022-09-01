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

// TODO move to a shared helper file
function dragElement(
  renderResult: EditorRenderResult,
  targetTestId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
) {
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

describe('Reparent Spike Tests', () => {
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
    act(() => dragElement(renderResult, 'ccc', dragDelta, emptyModifiers))

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

    act(() => dragElement(renderResult, 'ccc', dragDelta, emptyModifiers))

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
