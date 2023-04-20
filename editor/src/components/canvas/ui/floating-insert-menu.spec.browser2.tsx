import { act, fireEvent, queryByAttribute } from '@testing-library/react'
import { FOR_TESTS_setNextGeneratedUid } from '../../../core/model/element-template-utils.test-utils'
import { BakedInStoryboardUID } from '../../../core/model/scene-utils'
import * as EP from '../../../core/shared/element-path'
import { selectComponentsForTest, wait } from '../../../utils/utils.test-utils'
import { pressKey } from '../event-helpers.test-utils'
import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../ui-jsx.test-utils'
import { FloatingMenuTestId } from './floating-insert-menu'

describe('Floating insert menu', () => {
  it('can insert a conditional via the floating insert menu', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
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
  </div>`),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
    ])

    await insertViaAddElementPopup(editor, 'cond')

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`<div
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
    {true ? null : null}
  </div>`),
    )
  })

  it('can insert a fragment via the floating insert menu', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
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
  </div>`),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
    ])

    await insertViaAddElementPopup(editor, 'frag')

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`<div
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
    <React.Fragment />
  </div>`),
    )
  })

  it('can insert a div via the floating insert menu', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
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
  </div>`),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
    ])

    FOR_TESTS_setNextGeneratedUid('new-div')

    await insertViaAddElementPopup(editor, 'div')

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`<div
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
    <div
        style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute'
        }}
        data-uid='new-div'
    />

  </div>`),
    )
  })

  it('can insert a span with sample text', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
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
  </div>`),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
    ])

    FOR_TESTS_setNextGeneratedUid('sample-text')

    await insertViaAddElementPopup(editor, 'sampl')

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`<div
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
    <span data-uid='sample-text'>Sample text</span>
  </div>`),
    )
  })
})

async function insertViaAddElementPopup(editor: EditorRenderResult, query: string) {
  await pressKey('a')
  const floatingMenu = editor.renderedDOM.getByTestId(FloatingMenuTestId)
  const searchBox = queryByAttribute('type', floatingMenu, 'text')!

  await act(() => {
    fireEvent.focus(searchBox)
    fireEvent.change(searchBox, { target: { value: query } })
    fireEvent.blur(searchBox)
    fireEvent.keyDown(searchBox, { key: 'Enter', keyCode: 13, metaKey: true })
  })
}
