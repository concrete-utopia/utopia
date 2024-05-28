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
  TestAppUID,
  TestSceneUID,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../canvas/ui-jsx.test-utils'
import { setPanelVisibility, setRightMenuTab } from './actions/action-creators'
import { InsertMenuFilterTestId } from './insertmenu'
import { RightMenuTab } from './store/editor-state'
import { ComponentPickerTestId } from '../navigator/navigator-item/component-picker'
import * as EP from '../../core/shared/element-path'
import { BakedInStoryboardUID } from '../../core/model/scene-utils'
import { selectComponentsForTest } from '../../utils/utils.test-utils'

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

    await selectComponentsForTest(renderResult, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root`),
    ])

    await openInsertMenu(renderResult)

    FOR_TESTS_setNextGeneratedUid('div')

    expect(getInsertItems().length).toEqual(allInsertItemsCount)

    document.execCommand('insertText', false, 'div')

    const picker = await screen.findByTestId(ComponentPickerTestId)
    forceNotNull('the component picker must not be null', picker)

    await pressKey('Enter', { targetElement: picker })
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
            }}
            data-uid='div'
        />
        </div>
      `),
    )
  })
})
