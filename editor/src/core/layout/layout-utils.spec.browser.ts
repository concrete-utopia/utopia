import { LayoutSystem, sides } from 'utopia-api'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../../components/canvas/ui-jsx.test-utils'
import { pasteJSXElements, selectComponents } from '../../components/editor/actions/action-creators'
import * as TP from '../shared/template-path'
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
} from '../shared/element-template'
import { generateUidWithExistingComponents } from '../model/element-template-utils'
import { left, right } from '../shared/either'
import { CanvasRectangle, LocalRectangle } from '../shared/math-utils'
import { BakedInStoryboardUID } from '../model/scene-utils'
import { emptyComments } from '../workers/parser-printer/parser-printer-comments'
import { InstancePath } from '../shared/project-file-types'

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
    )

    await renderResult.dispatch(
      [selectComponents([TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )
    ;(generateUidWithExistingComponents as any) = jest.fn().mockReturnValue(NewUID)
    const elementToPaste = jsxElement(
      'View',
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
    const elementPath = TP.appendNewElementPath(TestScenePath, [NewUID]) as InstancePath

    const sceneElementPath = TP.instancePathForElementAtPathDontThrowOnScene(TestScenePath)

    const metadata: ElementInstanceMetadataMap = {
      [TP.toString(sceneElementPath)]: {
        templatePath: sceneElementPath,
        element: left('Scene'),
        props: {
          style: {
            width: 375,
            height: 812,
          },
        },
        globalFrame: { x: 0, y: 0, width: 375, height: 812 } as CanvasRectangle,
        localFrame: { x: 0, y: 0, width: 375, height: 812 } as LocalRectangle,
        children: [],
        rootElements: [elementPath],
        componentInstance: false,
        isEmotionOrStyledComponent: false,
        specialSizeMeasurements: emptySpecialSizeMeasurements,
        computedStyle: emptyComputedStyle,
        attributeMetadatada: emptyAttributeMetadatada,
        label: null,
      },
      [TP.toString(elementPath)]: {
        templatePath: elementPath,
        element: right(elementToPaste),
        props: {
          'data-uid': NewUID,
        },
        globalFrame: { x: 0, y: 0, width: 375, height: 812 } as CanvasRectangle,
        localFrame: { x: 0, y: 0, width: 375, height: 812 } as LocalRectangle,
        children: [],
        rootElements: [],
        componentInstance: true,
        isEmotionOrStyledComponent: false,
        specialSizeMeasurements: specialSizeMeasurements(
          { x: 0, y: 0 } as any,
          null,
          null,
          true,
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
        ),
        computedStyle: emptyComputedStyle,
        attributeMetadatada: emptyAttributeMetadatada,
        label: null,
      },
    }

    const pasteElements = pasteJSXElements(
      [elementToPaste],
      [TP.appendNewElementPath(TestScenePath, [NewUID])],
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
