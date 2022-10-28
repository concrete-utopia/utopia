import { sides } from 'utopia-api/core'
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
import {
  jsxAttributeNestedObjectSimple,
  jsxAttributeValue,
  jsxElement,
  specialSizeMeasurements,
  emptyComputedStyle,
  ElementInstanceMetadataMap,
  jsxAttributesFromMap,
  emptyAttributeMetadatada,
  emptySpecialSizeMeasurements,
  emptyComments,
} from '../shared/element-template'
import { FOR_TESTS_setNextGeneratedUid } from '../model/element-template-utils.test-utils'
import { left, right } from '../shared/either'
import { CanvasRectangle, LocalRectangle } from '../shared/math-utils'
import { emptyImports } from '../workers/common/project-file-utils'

const NewUID = 'catdog'

describe('pasteJSXElements', () => {
  it('removes pin related layout props when pasting to flex element', async () => {
    const renderResult = await createStarterEditor()

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )
    FOR_TESTS_setNextGeneratedUid(NewUID)

    const pasteElements = createPasteElementAction('element-with-no-position')

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
            <View style={{ width: 100, height: 100 }} data-uid='catdog' />
          </View>
        </View>`,
      ),
    )
  })

  it('removes pin related layout props when pasting to flex element, turns position absolute into relative', async () => {
    const renderResult = await createStarterEditor()

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )
    FOR_TESTS_setNextGeneratedUid(NewUID)

    const pasteElements = createPasteElementAction('element-with-position-absolute')

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
            <View style={{ width: 100, height: 100, contain: 'layout' }} data-uid='catdog' />
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
      />
    </View>
    `),
    'await-first-dom-report',
  )
  return renderResult
}

// TODO: copy from the document, and then we don't need a fake metadata
function createPasteElementAction(
  positionProp: 'element-with-position-absolute' | 'element-with-no-position',
) {
  const elementToPaste = jsxElement(
    'View',
    NewUID,
    jsxAttributesFromMap({
      style: jsxAttributeNestedObjectSimple(
        jsxAttributesFromMap({
          bottom: jsxAttributeValue(50, emptyComments),
          right: jsxAttributeValue(50, emptyComments),
          width: jsxAttributeValue(100, emptyComments),
          height: jsxAttributeValue(100, emptyComments),
          ...(positionProp === 'element-with-position-absolute'
            ? { position: jsxAttributeValue('absolute', emptyComments) }
            : {}),
        }),
        emptyComments,
      ),
      'data-uid': jsxAttributeValue(NewUID, emptyComments),
    }),
    [],
  )
  const elementPath = EP.appendNewElementPath(TestScenePath, [NewUID])

  const metadata: ElementInstanceMetadataMap = {
    [EP.toString(TestScenePath)]: {
      elementPath: TestScenePath,
      element: left('Scene'),
      globalFrame: { x: 0, y: 0, width: 375, height: 812 } as CanvasRectangle,
      localFrame: { x: 0, y: 0, width: 375, height: 812 } as LocalRectangle,
      componentInstance: false,
      isEmotionOrStyledComponent: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements,
      computedStyle: emptyComputedStyle,
      attributeMetadatada: emptyAttributeMetadatada,
      label: null,
      importInfo: null,
    },
    [EP.toString(elementPath)]: {
      elementPath: elementPath,
      element: right(elementToPaste),
      globalFrame: { x: 0, y: 0, width: 375, height: 812 } as CanvasRectangle,
      localFrame: { x: 0, y: 0, width: 375, height: 812 } as LocalRectangle,
      componentInstance: true,
      isEmotionOrStyledComponent: false,
      specialSizeMeasurements: specialSizeMeasurements(
        { x: 0, y: 0 } as any,
        null,
        null,
        true,
        EP.emptyElementPath,
        true,
        positionProp === 'element-with-no-position' ? 'flex' : 'none',
        'none',
        false,
        'block',
        positionProp === 'element-with-position-absolute' ? 'absolute' : 'static',
        sides(undefined, undefined, undefined, undefined),
        sides(undefined, undefined, undefined, undefined),
        null,
        null,
        0,
        0,
        null,
        0,
        null,
        'div',
        0,
        null,
        'none',
        false,
        'initial',
      ),
      computedStyle: emptyComputedStyle,
      attributeMetadatada: emptyAttributeMetadatada,
      label: null,
      importInfo: null,
    },
  }

  const pasteElements = pasteJSXElements(
    EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
    [
      elementPaste(
        elementToPaste,
        emptyImports(),
        EP.appendNewElementPath(TestScenePath, [NewUID]),
      ),
    ],
    metadata,
  )

  return pasteElements
}
