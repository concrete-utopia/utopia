import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../canvas/ui-jsx.test-utils'
import { increaseOnlineStateFailureCount, resetOnlineState } from './actions/action-creators'

describe('Editor is offline banner', () => {
  it('renders when the editor is offline', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<span>Editor Offline Text</span>`),
      'await-first-dom-report',
    )
    await editor.dispatch(
      [
        increaseOnlineStateFailureCount(),
        increaseOnlineStateFailureCount(),
        increaseOnlineStateFailureCount(),
      ],
      true,
    )
    const offlineBanners = editor.renderedDOM.queryAllByText(
      `Utopia is offline, and will reconnect automatically.`,
    )
    expect(offlineBanners).toHaveLength(1)
  })
  it('does not render when the editor is online', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<span>Editor Offline Text</span>`),
      'await-first-dom-report',
    )
    await editor.dispatch([resetOnlineState()], true)
    const offlineBanners = editor.renderedDOM.queryAllByText(
      `Utopia is offline, and will reconnect automatically.`,
    )
    expect(offlineBanners).toHaveLength(0)
  })
})
