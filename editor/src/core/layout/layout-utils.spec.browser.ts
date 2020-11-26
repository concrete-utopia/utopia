import { LayoutSystem, sides } from 'utopia-api'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../components/canvas/ui-jsx.test-utils'
import { pasteJSXElements, selectComponents } from '../../components/editor/actions/actions'
import * as TP from '../shared/template-path'
import {
  ComponentMetadata,
  jsxAttributeNestedObjectSimple,
  jsxAttributeValue,
  jsxElement,
  specialSizeMeasurements,
  emptyComputedStyle,
  ElementInstanceMetadataMap,
  jsxMetadata,
} from '../shared/element-template'
import { generateUidWithExistingComponents } from '../model/element-template-utils'
import { right } from '../shared/either'
import { CanvasRectangle, LocalRectangle } from '../shared/math-utils'
import { BakedInStoryboardUID } from '../model/scene-utils'

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
      <View style={{ ...props.style }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#DDDDDD', left: 52, top: 61, width: 256, height: 202, display: 'flex' }}
          data-uid={'bbb'}
        />
      </View>
      `),
    )

    await renderResult.dispatch(
      [
        selectComponents(
          [TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['aaa', 'bbb'])],
          false,
        ),
      ],
      false,
    )
    ;(generateUidWithExistingComponents as any) = jest.fn().mockReturnValue(NewUID)
    const elementToPaste = jsxElement(
      'View',
      {
        style: jsxAttributeNestedObjectSimple({
          bottom: jsxAttributeValue(50),
          right: jsxAttributeValue(50),
          width: jsxAttributeValue(100),
          height: jsxAttributeValue(100),
        }),
        'data-uid': jsxAttributeValue(NewUID),
      },
      [],
    )
    const elementPath = TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], [NewUID])
    const elements: ElementInstanceMetadataMap = {
      [TP.toString(elementPath)]: {
        templatePath: TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], [NewUID]),
        element: right(elementToPaste),
        props: {
          'data-uid': NewUID,
        },
        globalFrame: { x: 0, y: 0, width: 375, height: 812 } as CanvasRectangle,
        localFrame: { x: 0, y: 0, width: 375, height: 812 } as LocalRectangle,
        children: [],
        componentInstance: true,
        specialSizeMeasurements: specialSizeMeasurements(
          { x: 0, y: 0 } as any,
          null,
          null,
          true,
          true,
          'none',
          'none',
          false,
          'absolute',
          sides(undefined, undefined, undefined, undefined),
          sides(undefined, undefined, undefined, undefined),
          null,
          null,
          0,
          0,
          null,
        ),
        computedStyle: emptyComputedStyle,
      },
    }

    const components: ComponentMetadata[] = [
      {
        scenePath: TP.scenePath([BakedInStoryboardUID, 'scene-aaa']),
        templatePath: TP.instancePath([], [BakedInStoryboardUID, 'scene-aaa']),
        component: 'Component1',
        container: {
          layoutSystem: LayoutSystem.PinSystem,
        },
        sceneResizesContent: false,
        globalFrame: { x: 0, y: 0, width: 375, height: 812 } as CanvasRectangle,
        style: { width: 375, height: 812 },
        rootElements: [elementPath],
      },
    ]
    const metadata = jsxMetadata(components, elements)
    const pasteElements = pasteJSXElements(
      [elementToPaste],
      [TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], [NewUID])],
      metadata,
    )
    await renderResult.dispatch([pasteElements], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View
          style={{ ...props.style }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'aaa'}
        >
          <View
            style={{ backgroundColor: '#DDDDDD', left: 52, top: 61, width: 256, height: 202, display: 'flex' }}
            data-uid={'bbb'}
          >
            <View style={{}} data-uid={'catdog'} />
          </View>
        </View>`,
      ),
    )
  })
})
