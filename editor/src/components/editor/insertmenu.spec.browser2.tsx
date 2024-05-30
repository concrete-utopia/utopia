import { act, fireEvent, screen } from '@testing-library/react'
import { FOR_TESTS_setNextGeneratedUid } from '../../core/model/element-template-utils.test-utils'
import { forceNotNull } from '../../core/shared/optional-utils'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import {
  mouseDragFromPointToPoint,
  mouseMoveToPoint,
  pressKey,
} from '../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../canvas/ui-jsx.test-utils'
import { setPanelVisibility, setRightMenuTab } from './actions/action-creators'
import { InsertMenuFilterTestId } from './insertmenu'
import { RightMenuTab } from './store/editor-state'

function getInsertItems() {
  return screen.queryAllByTestId(/^insert-item-/gi)
}

const allInsertItemsCount = 23

function openInsertMenu(renderResult: EditorRenderResult) {
  return renderResult.dispatch(
    [setPanelVisibility('rightmenu', true), setRightMenuTab(RightMenuTab.Insert)],
    true,
  )
}

describe('insert menu', () => {
  describe('filter search', () => {
    it('can filter by component name', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`<div />`),
        'await-first-dom-report',
      )

      await openInsertMenu(renderResult)

      expect(getInsertItems().length).toEqual(allInsertItemsCount)

      document.execCommand('insertText', false, 'span')

      expect(getInsertItems().length).toEqual(1)
      expect(getInsertItems()[0].innerText).toEqual('span')
    })
    it('can filter by group name', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`<div />`),
        'await-first-dom-report',
      )

      await openInsertMenu(renderResult)

      document.execCommand('insertText', false, 'elem')

      expect(getInsertItems().length).toEqual(9)
      expect(getInsertItems().map((s) => s.innerText)).toEqual([
        // html elements group
        'span',
        'h1',
        'h2',
        'p',
        'button',
        'input',
        'video',
        'img',
        // sample group
        'Sample text',
      ])
    })
    describe('no match', () => {
      it('shows all elements', async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`<div />`),
          'await-first-dom-report',
        )

        await openInsertMenu(renderResult)

        document.execCommand('insertText', false, 'wr0ng')
        expect(getInsertItems().length).toEqual(allInsertItemsCount)
      })
    })
    it('does not show insertables that cannot be inserted', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`<div />`),
        'await-first-dom-report',
      )

      await openInsertMenu(renderResult)

      document.execCommand('insertText', false, 'group')
      expect(getInsertItems().length).toEqual(allInsertItemsCount)
    })
  })

  it('supports inserting via keyboard', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
            data-uid='root'
            data-testid='root'
            style={{
                position: 'absolute',
                width: 400,
                height: 400,
                background: '#fff',
            }}
        />
    `),
      'await-first-dom-report',
    )

    await openInsertMenu(renderResult)

    FOR_TESTS_setNextGeneratedUid('the-div')

    expect(getInsertItems().length).toEqual(allInsertItemsCount)

    document.execCommand('insertText', false, 'div')

    const filterBox = await screen.findByTestId(InsertMenuFilterTestId)
    forceNotNull('the filter box must not be null', filterBox)

    await pressKey('Enter', { targetElement: filterBox })

    const targetElement = renderResult.renderedDOM.getByTestId('root')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = {
      x: targetElementBounds.x + 5,
      y: targetElementBounds.y + 5,
    }
    const endPoint = {
      x: targetElementBounds.x + 100,
      y: targetElementBounds.y + 150,
    }

    await mouseMoveToPoint(canvasControlsLayer, startPoint)
    await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
            data-uid='root'
            data-testid='root'
            style={{
                position: 'absolute',
                width: 400,
                height: 400,
                background: '#fff',
            }}
        >
        <div
            style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 6,
                top: 6,
                width: 95,
                height: 145,
            }}
            data-uid='the-div'
        />
        </div>
      `),
    )
  })
})
