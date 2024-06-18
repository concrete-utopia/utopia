import type { EditorRenderResult } from '../../components/canvas/ui-jsx.test-utils'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../../components/canvas/ui-jsx.test-utils'
import * as EP from '../shared/element-path'
import type { ElementPath } from '../shared/project-file-types'
import {
  firePasteEvent,
  MockClipboardHandlers,
  pressKey,
} from '../../components/canvas/event-helpers.test-utils'
import { cmdModifier } from '../../utils/modifiers'
import { selectComponentsForTest } from '../../utils/utils.test-utils'

describe('pasteJSXElements', () => {
  const clipboardMock = new MockClipboardHandlers().mock()

  async function runPaste(
    renderResult: EditorRenderResult,
    paths: { elementToCopy: ElementPath; targetParent: ElementPath },
  ) {
    await selectComponentsForTest(renderResult, [paths.elementToCopy])
    await pressKey('c', { modifiers: cmdModifier })

    await selectComponentsForTest(renderResult, [paths.targetParent])

    const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

    firePasteEvent(canvasRoot)

    // Wait for the next frame
    await clipboardMock.pasteDone
    await renderResult.getDispatchFollowUpActionsFinished()

    await pressKey('Esc')
    await renderResult.getDispatchFollowUpActionsFinished()
  }

  it('removes pin related layout props when pasting to flex element', async () => {
    const renderResult = await createStarterEditor()

    await runPaste(renderResult, {
      elementToCopy: EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ccc']),
      targetParent: EP.appendNewElementPath(TestScenePath, ['aaa', 'paste-target']),
    })

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View
          style={{ ...props.style }}
          data-uid='aaa'
        >
          <View
            style={{ backgroundColor: '#DDDDDD', left: 52, top: 61, width: 256, height: 202, display: 'flex' }}
            data-uid='bbb'
          >
            <View style={{ width: 100, height: 100 }} data-uid='ccc' />
            <View style={{ width: 100, height: 100, position: 'absolute' }} data-uid='ddd' />
          </View>
          <View
            style={{ backgroundColor: '#ffcccc', left: 52, top: 61, width: 150, height: 120, display: 'flex' }}
            data-uid='paste-target'
          >
            <View style={{ width: 100, height: 100 }} data-uid='ccd' />
          </View>
        </View>`,
      ),
    )
  })

  it('removes pin related layout props when pasting to flex element, turns position absolute into contain layout', async () => {
    const renderResult = await createStarterEditor()

    await runPaste(renderResult, {
      elementToCopy: EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ddd']),
      targetParent: EP.appendNewElementPath(TestScenePath, ['aaa', 'paste-target']),
    })

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View
          style={{ ...props.style }}
          data-uid='aaa'
        >
          <View
            style={{ backgroundColor: '#DDDDDD', left: 52, top: 61, width: 256, height: 202, display: 'flex' }}
            data-uid='bbb'
          >
            <View style={{ width: 100, height: 100 }} data-uid='ccc' />
            <View style={{ width: 100, height: 100, position: 'absolute' }} data-uid='ddd' />
          </View>
          <View
            style={{ backgroundColor: '#ffcccc', left: 52, top: 61, width: 150, height: 120, display: 'flex' }}
            data-uid='paste-target'
          >
            <View style={{ contain: 'layout', width: 100, height: 100 }} data-uid='dde' />
          </View>
        </View>`,
      ),
    )
  })
})

async function createStarterEditor() {
  const renderResult = await renderTestEditorWithCode(
    makeTestProjectCodeWithSnippet(`
    <View style={{ ...props.style }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#DDDDDD', left: 52, top: 61, width: 256, height: 202, display: 'flex' }}
        data-uid='bbb'
      >
        <View style={{ width: 100, height: 100 }} data-uid='ccc' />
        <View style={{ width: 100, height: 100, position: 'absolute' }} data-uid='ddd' />
      </View>
      <View
        style={{ backgroundColor: '#ffcccc', left: 52, top: 61, width: 150, height: 120, display: 'flex' }}
        data-uid='paste-target'
      />
    </View>
    `),
    'await-first-dom-report',
  )
  return renderResult
}
