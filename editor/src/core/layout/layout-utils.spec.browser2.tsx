import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../../components/canvas/ui-jsx.test-utils'
import {
  elementPaste,
  pasteJSXElements,
  selectComponents,
} from '../../components/editor/actions/action-creators'
import * as EP from '../shared/element-path'
import { ElementInstanceMetadataMap } from '../shared/element-template'
import { FOR_TESTS_setNextGeneratedUid } from '../model/element-template-utils.test-utils'
import { isLeft } from '../shared/either'
import { emptyImports } from '../workers/common/project-file-utils'
import { MetadataUtils } from '../model/element-metadata-utils'
import { ElementPath } from '../shared/project-file-types'
import { childInsertionPath } from '../../components/editor/store/insertion-path'
import { canvasPoint } from '../shared/math-utils'

describe('pasteJSXElements', () => {
  it('removes pin related layout props when pasting to flex element', async () => {
    const renderResult = await createStarterEditor()

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'paste-target'])], false)],
      false,
    )

    const pasteElements = createPasteElementAction(
      renderResult.getEditorState().editor.jsxMetadata,
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ccc']),
    )

    await renderResult.dispatch([pasteElements], true)

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
            <View style={{ width: 100, height: 100 }} data-uid='aaf' />
          </View>
        </View>`,
      ),
    )
  })

  it('removes pin related layout props when pasting to flex element, turns position absolute into contain layout', async () => {
    const renderResult = await createStarterEditor()

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'paste-target'])], false)],
      false,
    )

    const pasteElements = createPasteElementAction(
      renderResult.getEditorState().editor.jsxMetadata,
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ddd']),
    )

    await renderResult.dispatch([pasteElements], true)

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
            <View style={{ contain: 'layout', width: 100, height: 100 }} data-uid='aag' />
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

function createPasteElementAction(metadata: ElementInstanceMetadataMap, elementPath: ElementPath) {
  const elementToPasteMetadata = MetadataUtils.findElementByElementPath(metadata, elementPath)

  if (elementToPasteMetadata == null || isLeft(elementToPasteMetadata.element)) {
    throw new Error('Element to be Pasted was not found')
  }

  const elementToPaste = elementToPasteMetadata.element.value

  const pasteElements = pasteJSXElements(
    childInsertionPath(EP.appendNewElementPath(TestScenePath, ['aaa', 'paste-target'])),
    [elementPaste(elementToPaste, emptyImports(), elementPath)],
    metadata,
    canvasPoint({ x: 300, y: 300 }),
    {},
    [],
  )

  return pasteElements
}
