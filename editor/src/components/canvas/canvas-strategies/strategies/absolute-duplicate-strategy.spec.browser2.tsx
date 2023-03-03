import {
  EditorRenderResult,
  formatTestProjectCode,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import {
  FOR_TESTS_setNextGeneratedUid,
  FOR_TESTS_setNextGeneratedUids,
} from '../../../../core/model/element-template-utils.test-utils'
import { offsetPoint, windowPoint, WindowPoint } from '../../../../core/shared/math-utils'
import { altModifier, cmdModifier, Modifiers } from '../../../../utils/modifiers'
import { mouseClickAtPoint, mouseDragFromPointToPoint } from '../../event-helpers.test-utils'
import {
  selectComponentsForTest,
  setFeatureForBrowserTests,
} from '../../../../utils/utils.test-utils'
import * as EP from '../../../../core/shared/element-path'

async function dragElement(
  renderResult: EditorRenderResult,
  targetTestId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
): Promise<void> {
  const targetElement = renderResult.renderedDOM.getByTestId(targetTestId)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
  const endPoint = offsetPoint(startPoint, dragDelta)

  await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
  await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint, {
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
    await dragElement(renderResult, 'bbb', dragDelta, altModifier)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%', position: 'relative' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
            data-uid='hello'
            data-testid='bbb'
            />
            <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 80, top: 25, width: 200, height: 120 }}
            data-uid='bbb'
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
    await dragElement(renderResult, 'bbb', dragDelta, altModifier)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
            data-uid='hello'
            data-testid='bbb'
          />
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 80, top: 25, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })
  describe('with fragments', () => {
    setFeatureForBrowserTests('Fragment support', true)
    it('duplicates the selected absolute element when pressing alt, even if it is a fragment', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(
          projectWithFragment(`<React.Fragment data-uid='fragment'>
        <div
          style={{
            backgroundColor: '#d089cc',
            width: 150,
            height: 186,
            contain: 'layout',
            left: 7,
            top: 186,
            position: 'absolute',
          }}
          data-uid='chi'
          data-testid='child'
        >
          second
        </div>
      </React.Fragment>`),
        ),
        'await-first-dom-report',
      )

      FOR_TESTS_setNextGeneratedUid('fragment2')
      const dragDelta = windowPoint({ x: 40, y: -25 })

      const targetElement = renderResult.renderedDOM.getByTestId('child')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
      const endPoint = offsetPoint(startPoint, dragDelta)

      await selectComponentsForTest(renderResult, [EP.fromString('sb/fragment')])

      await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint, {
        modifiers: altModifier,
      })

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        formatTestProjectCode(
          projectWithFragment(`
      <React.Fragment>
      <div
        style={{
          backgroundColor: '#d089cc',
          width: 150,
          height: 186,
          contain: 'layout',
          left: 7,
          top: 186,
          position: 'absolute',
        }}
        data-uid='aaa'
        data-testid='child'
      >
        second
      </div>
      </React.Fragment>
      <React.Fragment>
      <div
        style={{
          backgroundColor: '#d089cc',
          width: 150,
          height: 186,
          contain: 'layout',
          left: 47,
          top: 161,
          position: 'absolute',
        }}
        data-uid='chi'
        data-testid='child'
      >
        second
      </div>
      </React.Fragment>`),
        ),
      )
    })
  })
})

const projectWithFragment = (innards: string) => `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    ${innards}
  </Storyboard>
)

`
