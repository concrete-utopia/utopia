import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { FOR_TESTS_setNextGeneratedUid } from '../../../../core/model/element-template-utils.test-utils'
import { offsetPoint, windowPoint, WindowPoint } from '../../../../core/shared/math-utils'
import { altModifier, cmdModifier, Modifiers } from '../../../../utils/modifiers'
import { mouseClickAtPoint, mouseDragFromPointToPoint } from '../../event-helpers.test-utils'

function dragElement(
  renderResult: EditorRenderResult,
  targetTestId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
) {
  const targetElement = renderResult.renderedDOM.getByTestId(targetTestId)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
  const endPoint = offsetPoint(startPoint, dragDelta)

  mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
  mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint, {
    modifiers: modifiers,
  })
}

describe('Absolute Duplicate Strategy', () => {
  it('duplicates the selected absolute element when pressing alt', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%', position: 'relative' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    FOR_TESTS_setNextGeneratedUid('hello')
    const dragDelta = windowPoint({ x: 40, y: -25 })
    dragElement(renderResult, 'bbb', dragDelta, altModifier)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%', position: 'relative' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 80, top: 25, width: 200, height: 120 }}
            data-uid='hello'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })

  it('duplicates the selected absolute element when pressing alt, even if the parent is static', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    FOR_TESTS_setNextGeneratedUid('hello')
    const dragDelta = windowPoint({ x: 40, y: -25 })
    dragElement(renderResult, 'bbb', dragDelta, altModifier)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 80, top: 25, width: 200, height: 120 }}
            data-uid='hello'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })
})
