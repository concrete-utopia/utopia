import * as TP from '../shared/template-path'
import {
  canvasRectangle,
  localRectangle,
  zeroRectangle,
  LocalRectangle,
  CanvasRectangle,
} from '../shared/math-utils'
import { right } from '../shared/either'
import { MetadataUtils } from './element-metadata-utils'
import {
  ComponentMetadata,
  ElementInstanceMetadata,
  emptySpecialSizeMeasurements,
  JSXElementName,
  jsxElementName,
  jsxTestElement,
  jsxTextBlock,
  jsxElement,
  jsxAttributeValue,
  elementInstanceMetadata,
  emptyComputedStyle,
  ElementInstanceMetadataMap,
  jsxMetadata,
  jsxAttributesFromMap,
  emptyAttributeMetadatada,
} from '../shared/element-template'
import { sampleImportsForTests } from './test-ui-js-file'
import { BakedInStoryboardUID } from './scene-utils'
import { TemplatePath } from '../shared/project-file-types'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath as TestScenePathForTestProject,
  TestStaticScenePath as TestStaticScenePathForTestProject,
} from '../../components/canvas/ui-jsx.test-utils'
import { createIndexedUid } from '../shared/uid-utils'
import { emptyComments } from '../workers/parser-printer/parser-printer-comments'
import { testStaticInstancePath } from '../shared/template-path.test-utils'

const TestScenePath = 'scene-aaa'

const testComponentMetadataChild1: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  templatePath: TP.instancePath(TP.scenePath([[BakedInStoryboardUID, TestScenePath]]), [
    'View',
    'View0',
  ]),
  props: {},
  element: right(jsxTestElement('View', [], [])),
  children: [],
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadatada,
}
const testComponentMetadataChild2: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  templatePath: TP.instancePath(TP.scenePath([[BakedInStoryboardUID, TestScenePath]]), [
    'View',
    'View1',
  ]),
  props: {},
  element: right(jsxTestElement('View', [], [])),
  children: [],
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadatada,
}

const testComponentMetadataGrandchild: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  templatePath: TP.instancePath(TP.scenePath([[BakedInStoryboardUID, TestScenePath]]), [
    'View',
    'View2',
    'View0',
  ]),
  props: {
    cica: 'hello',
  },
  element: right(jsxTestElement('View', [], [])),
  children: [],
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadatada,
}

const testComponentMetadataChild3: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  templatePath: TP.instancePath(TP.scenePath([[BakedInStoryboardUID, TestScenePath]]), [
    'View',
    'View2',
  ]),
  props: {},
  element: right(jsxTestElement('View', [], [])),
  children: [testComponentMetadataGrandchild.templatePath],
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadatada,
}

const testComponentRoot1: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  templatePath: TP.instancePath(TP.scenePath([[BakedInStoryboardUID, TestScenePath]]), ['View']),
  props: {},
  element: right(jsxTestElement('View', [], [])),
  children: [
    testComponentMetadataChild1.templatePath,
    testComponentMetadataChild2.templatePath,
    testComponentMetadataChild3.templatePath,
  ],
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadatada,
}

const testComponentScene: ComponentMetadata = {
  scenePath: TP.scenePath([[BakedInStoryboardUID, TestScenePath]]),
  templatePath: TP.instancePath(TP.emptyScenePath, [BakedInStoryboardUID, 'scene-aaa']),
  component: 'MyView',
  rootElements: [testComponentRoot1.templatePath],
  sceneResizesContent: false,
  globalFrame: canvasRectangle({
    x: 0,
    y: 0,
    width: 100,
    height: 100,
  }),
  style: {
    width: 100,
    height: 100,
  },
}

const testComponentMetadata: Array<ComponentMetadata> = [testComponentScene]
const testElementMetadataMap: ElementInstanceMetadataMap = {
  [TP.toString(testComponentMetadataChild1.templatePath)]: testComponentMetadataChild1,
  [TP.toString(testComponentMetadataChild2.templatePath)]: testComponentMetadataChild2,
  [TP.toString(testComponentMetadataChild3.templatePath)]: testComponentMetadataChild3,
  [TP.toString(testComponentMetadataGrandchild.templatePath)]: testComponentMetadataGrandchild,
  [TP.toString(testComponentRoot1.templatePath)]: testComponentRoot1,
}

const testJsxMetadata = jsxMetadata(testComponentMetadata, testElementMetadataMap)

describe('findElements', () => {
  it('Finds the element metadata', () => {
    const foundViewsWithHelloProp = MetadataUtils.findElements(
      testElementMetadataMap,
      (element) => {
        return element.props['cica'] != null
      },
    )

    expect(foundViewsWithHelloProp).toHaveLength(1)
    expect(foundViewsWithHelloProp[0].props.cica).toEqual('hello')

    const notFoundViews = MetadataUtils.findElements(
      testElementMetadataMap,
      (element) => element.props.nonexistentProp != null,
    )
    expect(notFoundViews).toHaveLength(0)
  })
})

describe('getElementByInstancePathMaybe', () => {
  it('works with an empty object', () => {
    const actualResult = MetadataUtils.getElementByInstancePathMaybe(
      {},
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, TestScenePath]]), ['View', 'View0']),
    )
    expect(actualResult).toBeNull()
  })
  it('returns null for a nonsense path', () => {
    const actualResult = MetadataUtils.getElementByInstancePathMaybe(
      {},
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, TestScenePath]]), ['Hats', 'Cats']),
    )
    expect(actualResult).toBeNull()
  })
  it('returns null for a partially nonsense path', () => {
    const actualResult = MetadataUtils.getElementByInstancePathMaybe(
      {},
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, TestScenePath]]), ['View', 'Cats']),
    )
    expect(actualResult).toBeNull()
  })
  it('returns the element from the map', () => {
    const actualResult = MetadataUtils.getElementByInstancePathMaybe(
      testElementMetadataMap,
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, TestScenePath]]), ['View']),
    )
    expect(actualResult).toBe(testComponentRoot1)
  })
  it('returns the element for a child of the root', () => {
    const actualResult = MetadataUtils.getElementByInstancePathMaybe(
      testElementMetadataMap,
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, TestScenePath]]), ['View', 'View1']),
    )
    expect(actualResult).toBe(testComponentMetadataChild2)
  })
})

describe('targetElementSupportsChildren', () => {
  const dummyInstanceDataForElementType = (
    elementName: JSXElementName | string,
  ): ElementInstanceMetadata => {
    return {
      globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
      localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
      templatePath: TP.instancePath(TP.scenePath([[BakedInStoryboardUID, TestScenePath]]), [
        'Dummy',
        'Element',
      ]),
      props: {},
      element: right(jsxTestElement(elementName, [], [])),
      children: [],
      componentInstance: false,
      isEmotionOrStyledComponent: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements,
      computedStyle: emptyComputedStyle,
      attributeMetadatada: emptyAttributeMetadatada,
    }
  }

  it('Returns true for a utopia-api View', () => {
    const element = dummyInstanceDataForElementType('View')
    const actualResult = MetadataUtils.targetElementSupportsChildren(sampleImportsForTests, element)
    expect(actualResult).toBeTruthy()
  })
  it('Returns true for a button', () => {
    const element = dummyInstanceDataForElementType('button')
    const actualResult = MetadataUtils.targetElementSupportsChildren(sampleImportsForTests, element)
    expect(actualResult).toBeTruthy()
  })
  it('Returns true for a div', () => {
    const element = dummyInstanceDataForElementType('div')
    const actualResult = MetadataUtils.targetElementSupportsChildren(sampleImportsForTests, element)
    expect(actualResult).toBeTruthy()
  })
  it('Returns true for a span', () => {
    const element = dummyInstanceDataForElementType('span')
    const actualResult = MetadataUtils.targetElementSupportsChildren(sampleImportsForTests, element)
    expect(actualResult).toBeTruthy()
  })
  it('Returns true for an animated.div', () => {
    const element = dummyInstanceDataForElementType(jsxElementName('animated', ['div']))
    const actualResult = MetadataUtils.targetElementSupportsChildren(sampleImportsForTests, element)
    expect(actualResult).toBeTruthy()
  })
})

describe('isPinnedAndNotAbsolutePositioned', () => {
  it('returns true for a pinned element that is not absolute positioned', () => {
    const elementMapForTest: ElementInstanceMetadataMap = {
      [TP.toString(testComponentRoot1.templatePath)]: {
        ...testComponentRoot1,
        specialSizeMeasurements: {
          ...testComponentRoot1.specialSizeMeasurements,
          parentLayoutSystem: 'flow',
          position: 'static',
        },
      },
    }
    expect(
      MetadataUtils.isPinnedAndNotAbsolutePositioned(
        jsxMetadata(testComponentMetadata, elementMapForTest),
        TP.instancePath(TP.scenePath([[BakedInStoryboardUID, TestScenePath]]), ['View']),
      ),
    ).toEqual(true)
  })
  it('returns false for a flex element that is not absolute positioned', () => {
    const elementMapForTest: ElementInstanceMetadataMap = {
      [TP.toString(testComponentRoot1.templatePath)]: {
        ...testComponentRoot1,
        specialSizeMeasurements: {
          ...testComponentRoot1.specialSizeMeasurements,
          parentLayoutSystem: 'flex',
          position: 'static',
        },
      },
    }
    expect(
      MetadataUtils.isPinnedAndNotAbsolutePositioned(
        jsxMetadata(testComponentMetadata, elementMapForTest),
        TP.instancePath(TP.scenePath([[BakedInStoryboardUID, TestScenePath]]), ['View']),
      ),
    ).toEqual(false)
  })
  it('returns false for a pinned element that is absolute positioned', () => {
    const elementMapForTest: ElementInstanceMetadataMap = {
      [TP.toString(testComponentRoot1.templatePath)]: {
        ...testComponentRoot1,
        specialSizeMeasurements: {
          ...testComponentRoot1.specialSizeMeasurements,
          parentLayoutSystem: 'flow',
          position: 'absolute',
        },
      },
    }
    expect(
      MetadataUtils.isPinnedAndNotAbsolutePositioned(
        jsxMetadata(testComponentMetadata, elementMapForTest),
        TP.instancePath(TP.scenePath([[BakedInStoryboardUID, TestScenePath]]), ['View']),
      ),
    ).toEqual(false)
  })
  it('returns false for a flex element that is absolute positioned', () => {
    const elementMapForTest: ElementInstanceMetadataMap = {
      [TP.toString(testComponentRoot1.templatePath)]: {
        ...testComponentRoot1,
        specialSizeMeasurements: {
          ...testComponentRoot1.specialSizeMeasurements,
          parentLayoutSystem: 'flex',
          position: 'absolute',
        },
      },
    }
    expect(
      MetadataUtils.isPinnedAndNotAbsolutePositioned(
        jsxMetadata(testComponentMetadata, elementMapForTest),
        TP.instancePath(TP.scenePath([[BakedInStoryboardUID, TestScenePath]]), ['View']),
      ),
    ).toEqual(false)
  })
})

describe('getElementLabel', () => {
  const scenePath = TP.scenePath([[BakedInStoryboardUID, 'scene-0']])
  const instancePath = TP.instancePath(TP.emptyScenePath, [BakedInStoryboardUID, `scene-0`])
  const divPath = TP.appendToPath(scenePath, 'div-1')
  const spanPath = TP.appendToPath(divPath, 'span-1')
  const textBlock = jsxTextBlock('test text')
  const spanElement = jsxElement(
    'span',
    jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('span-1', emptyComments) }),
    [textBlock],
  )
  const spanElementMetadata = elementInstanceMetadata(
    spanPath,
    right(spanElement),
    {
      'data-uid': 'span-1',
    },
    zeroRectangle as CanvasRectangle,
    zeroRectangle as LocalRectangle,
    [],
    false,
    false,
    emptySpecialSizeMeasurements,
    emptyComputedStyle,
    emptyAttributeMetadatada,
  )
  const divElement = jsxElement(
    'div',
    jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('div-1', emptyComments) }),
    [spanElement],
  )
  const divElementMetadata = elementInstanceMetadata(
    divPath,
    right(divElement),
    {
      'data-uid': 'div-1',
    },
    zeroRectangle as CanvasRectangle,
    zeroRectangle as LocalRectangle,
    [spanElementMetadata.templatePath],
    false,
    false,
    emptySpecialSizeMeasurements,
    emptyComputedStyle,
    emptyAttributeMetadatada,
  )
  const elements: ElementInstanceMetadataMap = {
    [TP.toString(spanElementMetadata.templatePath)]: spanElementMetadata,
    [TP.toString(divElementMetadata.templatePath)]: divElementMetadata,
  }
  const components: Array<ComponentMetadata> = [
    {
      scenePath: scenePath,
      templatePath: instancePath,
      component: 'App',
      sceneResizesContent: false,
      rootElements: [divElementMetadata.templatePath],
      globalFrame: canvasRectangle({
        x: 0,
        y: 0,
        width: 100,
        height: 100,
      }),
      style: {
        width: 100,
        height: 100,
      },
    },
  ]
  const metadata = jsxMetadata(components, elements)
  it('the label of a spin containing text is that text', () => {
    const actualResult = MetadataUtils.getElementLabel(spanPath, metadata)
    expect(actualResult).toEqual('test text')
  })
})

describe('getAllPaths', () => {
  it('returns the paths in a depth first manner', () => {
    const actualResult = MetadataUtils.getAllPaths(testJsxMetadata)
    const expectedResult: Array<TemplatePath> = [
      testComponentScene.scenePath,
      testComponentRoot1.templatePath,
      testComponentMetadataChild1.templatePath,
      testComponentMetadataChild2.templatePath,
      testComponentMetadataChild3.templatePath,
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, TestScenePath]]), [
        'View',
        'View2',
        'View0',
      ]),
    ]
    expect(actualResult).toEqual(expectedResult)
  })
})

describe('dynamicPathToStaticPath', () => {
  it('converts a dynamic path to static', async () => {
    const staticPath = MetadataUtils.dynamicPathToStaticPath(
      TP.instancePath(TestScenePathForTestProject, ['aaa', createIndexedUid('bbb', 1)]),
    )
    expect(staticPath).toEqual(
      testStaticInstancePath(TestStaticScenePathForTestProject, ['aaa', 'bbb']),
    )
  })

  it('finds an already static path all right', async () => {
    const staticPath = MetadataUtils.dynamicPathToStaticPath(
      TP.instancePath(TestScenePathForTestProject, ['aaa', 'ccc']),
    )
    expect(staticPath).toEqual(
      testStaticInstancePath(TestStaticScenePathForTestProject, ['aaa', 'ccc']),
    )
  })
})
