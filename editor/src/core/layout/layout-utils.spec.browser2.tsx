import { sides } from 'utopia-api/core'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../../components/canvas/ui-jsx.test-utils'
import { pasteJSXElements, selectComponents } from '../../components/editor/actions/action-creators'
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
import { FOR_TESTS_setNextGeneratedUid } from '../model/element-template-utils'
import { left, right } from '../shared/either'
import { CanvasRectangle, LocalRectangle } from '../shared/math-utils'
import { ElementProps } from 'src/components/editor/store/editor-state'

const NewUID = 'catdog'

describe('maybeSwitchLayoutProps', () => {
  it('removes pin related layout props when pasting to flex element', async () => {
    // Code kept commented for any future person who needs it.
    //const currentWindow = require('electron').remote.getCurrentWindow()
    //currentWindow.show()
    //currentWindow.setPosition(500, 200)
    //currentWindow.setSize(2200, 1000)
    //currentWindow.openDevTools()
    // This is necessary because the test code races against the Electron process
    // opening the window it would appear.
    //await wait(20000)
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

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )
    FOR_TESTS_setNextGeneratedUid(NewUID)
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
          'none',
          'none',
          false,
          'block',
          'absolute',
          sides(undefined, undefined, undefined, undefined),
          sides(undefined, undefined, undefined, undefined),
          null,
          null,
          0,
          0,
          null,
          null,
          'div',
          0,
          null,
        ),
        computedStyle: emptyComputedStyle,
        attributeMetadatada: emptyAttributeMetadatada,
        label: null,
        importInfo: null,
      },
    }

    const pasteElements = pasteJSXElements(
      [elementToPaste],
      [EP.appendNewElementPath(TestScenePath, [NewUID])],
      metadata,
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
            <View style={{ position: 'relative' }} data-uid='catdog' />
          </View>
        </View>`,
      ),
    )
  })
})
