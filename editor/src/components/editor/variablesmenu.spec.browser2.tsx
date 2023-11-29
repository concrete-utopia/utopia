import { act, fireEvent, screen } from '@testing-library/react'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import {
  TestAppUID,
  TestSceneUID,
  getPrintedUiJsCode,
  makeTestProjectCodeWithComponentInnards,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../canvas/ui-jsx.test-utils'
import * as EP from '../../core/shared/element-path'
import { setPanelVisibility, setRightMenuTab } from './actions/action-creators'
import { RightMenuTab } from './store/editor-state'
import { selectComponentsForTest } from '../../utils/utils.test-utils'
import { BakedInStoryboardUID } from '../../core/model/scene-utils'
import { forceNotNull } from '../../core/shared/optional-utils'
import { InsertMenuFilterTestId } from './insertmenu'

function getInsertItems() {
  return screen.queryAllByTestId(/^insert-item-/gi)
}

function openVariablesMenu(renderResult: EditorRenderResult) {
  return renderResult.dispatch(
    [setPanelVisibility('rightmenu', true), setRightMenuTab(RightMenuTab.Variables)],
    true,
  )
}

describe('variables menu', () => {
  describe('filter search', () => {
    it('can filter by variable name', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithComponentInnards(`
          const myObj = { test: 'test', num: 5, image: 'img.png' }
          return (
            <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 57,
              top: 168,
              width: 247,
              height: 402,
            }}
            data-uid='container'
          >
            <div data-uid='a3d' />
          </div>
        )`),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
      ])

      await openVariablesMenu(editor)

      expect(getInsertItems().length).toEqual(3)

      document.execCommand('insertText', false, 'myObj.im')

      expect(getInsertItems().length).toEqual(1)
      expect(getInsertItems()[0].innerText).toEqual('myObj.image')
    })

    describe('no match', () => {
      it('shows all elements', async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`<div />`),
          'await-first-dom-report',
        )

        await openVariablesMenu(renderResult)

        expect(getInsertItems().length).toEqual(0)
      })
    })

    it('does not show insertables that cannot be inserted', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithComponentInnards(`
          const myObj = { test: 'test', num: 5, image: 'img.png' }
          return (
            <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 57,
              top: 168,
              width: 247,
              height: 402,
            }}
            data-uid='container'
          >
            <div data-uid='a3d' />
          </div>
        )`),
        'await-first-dom-report',
      )

      await openVariablesMenu(editor)

      expect(getInsertItems().length).toEqual(0)
    })
  })

  describe('insertion', () => {
    it('inserts an image from within an object', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithComponentInnards(`
          const myObj = { test: 'test', num: 5, image: 'img.png' }
          return (
            <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 57,
              top: 168,
              width: 247,
              height: 402,
            }}
            data-uid='container'
          >
            <div data-uid='a3d' />
          </div>
        )`),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
      ])

      await openVariablesMenu(editor)

      document.execCommand('insertText', false, 'myObj.image')

      const filterBox = await screen.findByTestId(InsertMenuFilterTestId)
      forceNotNull('the filter box must not be null', filterBox)

      await act(async () => {
        fireEvent.keyDown(filterBox, { key: 'Enter', keycode: 13 })
      })

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithComponentInnards(`
        const myObj = { test: 'test', num: 5, image: 'img.png' }
        return (
          <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 57,
            top: 168,
            width: 247,
            height: 402,
          }}
          data-uid='container'
        >
          <div data-uid='a3d' />
          <img src={myObj.image} style={{width: 100, height: 100, top:0, left: 0, position: 'absolute'}} data-uid='ele'/>
          </div>
      )`),
      )
    })
    it('inserts a text', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithComponentInnards(`
          const myText = ''
          return (
            <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 57,
              top: 168,
              width: 247,
              height: 402,
            }}
            data-uid='container'
          >
            <div data-uid='a3d' />
          </div>
        )`),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
      ])

      await openVariablesMenu(editor)

      document.execCommand('insertText', false, 'myText')

      const filterBox = await screen.findByTestId(InsertMenuFilterTestId)
      forceNotNull('the filter box must not be null', filterBox)

      await act(async () => {
        fireEvent.keyDown(filterBox, { key: 'Enter', keycode: 13 })
      })

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithComponentInnards(`
        const myText = ''
        return (
          <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 57,
            top: 168,
            width: 247,
            height: 402,
          }}
          data-uid='container'
        >
          <div data-uid='a3d' />
          <span style={{ width: 100, height: 100, top: 0, left: 0, position: 'absolute' }} data-uid='ele'>{myText}</span>
          </div>
      )`),
      )
    })
  })
})
