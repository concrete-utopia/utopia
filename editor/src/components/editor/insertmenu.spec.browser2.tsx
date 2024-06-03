import { screen } from '@testing-library/react'
import { FOR_TESTS_setNextGeneratedUid } from '../../core/model/element-template-utils.test-utils'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import {
  TestAppUID,
  TestSceneUID,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../canvas/ui-jsx.test-utils'
import { setPanelVisibility, setRightMenuTab } from './actions/action-creators'
import { RightMenuTab } from './store/editor-state'
import * as EP from '../../core/shared/element-path'
import { BakedInStoryboardUID } from '../../core/model/scene-utils'
import { searchInComponentPicker, selectComponentsForTest } from '../../utils/utils.test-utils'

function getInsertItems() {
  return screen.queryAllByTestId(/^component-picker-item-/gi)
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

      await searchInComponentPicker(renderResult, 'span')

      expect(getInsertItems().length).toEqual(1)
      expect(getInsertItems()[0].innerText).toEqual('span')
    })
    describe('no match', () => {
      it('shows no elements', async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`<div />`),
          'await-first-dom-report',
        )

        await openInsertMenu(renderResult)

        await searchInComponentPicker(renderResult, 'wr0ng')
        expect(getInsertItems().length).toEqual(0)
      })
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

    await searchInComponentPicker(renderResult, 'div')
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
