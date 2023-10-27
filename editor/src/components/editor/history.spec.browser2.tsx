import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../canvas/ui-jsx.test-utils'
import { selectComponents } from './actions/meta-actions'
import * as EP from '../../core/shared/element-path'
import { deleteSelected, redo, undo } from './actions/action-creators'

describe('history', () => {
  describe('undo', () => {
    it('shows a toast when trying to undo at the beginning of history', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          "<div data-uid='foo' style={{ position:'absolute', top: 100, left: 100, backgroundColor: 'red', width: 100, height: 100 }} />",
        ),
        'await-first-dom-report',
      )
      await renderResult.dispatch(
        selectComponents([EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity')], false),
        true,
      )
      await renderResult.dispatch([deleteSelected()], true)
      await renderResult.dispatch([undo()], true)
      expect(renderResult.getEditorState().editor.toasts.length).toBe(0)
      await renderResult.dispatch([undo()], true)
      expect(renderResult.getEditorState().editor.toasts.length).toBe(1)
      expect(renderResult.getEditorState().editor.toasts[0].message).toBe(
        `Can't undo, reached the end of the undo history.`,
      )
    })
  })
  describe('redo', () => {
    it('shows a toast when trying to redo at the end of history', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          "<div data-uid='foo' style={{ position:'absolute', top: 100, left: 100, backgroundColor: 'red', width: 100, height: 100 }} />",
        ),
        'await-first-dom-report',
      )
      await renderResult.dispatch(
        selectComponents([EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity')], false),
        true,
      )
      await renderResult.dispatch([deleteSelected()], true)
      await renderResult.dispatch([undo()], true)
      expect(renderResult.getEditorState().editor.toasts.length).toBe(0)
      await renderResult.dispatch([redo()], true)
      expect(renderResult.getEditorState().editor.toasts.length).toBe(0)
      await renderResult.dispatch([redo()], true)
      expect(renderResult.getEditorState().editor.toasts.length).toBe(1)
      expect(renderResult.getEditorState().editor.toasts[0].message).toBe(
        `Can't redo, reached the end of the undo history.`,
      )
    })
  })
})
