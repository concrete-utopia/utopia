import * as EP from '../shared/element-path'
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
  jsxAttributesFromMap,
  emptyAttributeMetadatada,
  emptyComments,
} from '../shared/element-template'
import { sampleImportsForTests } from './test-ui-js-file.test-utils'
import { BakedInStoryboardUID } from './scene-utils'
import { ElementPath } from '../shared/project-file-types'
import { AllElementProps, ElementProps } from 'src/components/editor/store/editor-state'
import { ElementInstanceMetadataMapKeepDeepEquality } from '../../components/editor/store/store-deep-equality-instances'

const TestScenePath = 'scene-aaa'

const testComponentMetadataChild1: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  elementPath: EP.elementPath([
    [BakedInStoryboardUID, TestScenePath],
    ['View', 'View0'],
  ]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadatada,
  label: null,
  importInfo: null,
}
const testComponentMetadataChild2: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  elementPath: EP.elementPath([
    [BakedInStoryboardUID, TestScenePath],
    ['View', 'View1'],
  ]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadatada,
  label: null,
  importInfo: null,
}

const testComponentMetadataGrandchild: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  elementPath: EP.elementPath([
    [BakedInStoryboardUID, TestScenePath],
    ['View', 'View2', 'View0'],
  ]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadatada,
  label: null,
  importInfo: null,
}

const testComponentPropsGrandchild: ElementProps = {
  cica: 'hello',
}

const testComponentMetadataChild3: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  elementPath: EP.elementPath([
    [BakedInStoryboardUID, TestScenePath],
    ['View', 'View2'],
  ]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadatada,
  label: null,
  importInfo: null,
}

const testComponentRoot1: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  elementPath: EP.elementPath([[BakedInStoryboardUID, TestScenePath], ['View']]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadatada,
  label: null,
  importInfo: null,
}

const testComponentSceneChildElementRootChild: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  elementPath: EP.elementPath([
    [BakedInStoryboardUID, TestScenePath, 'Scene-Child'],
    ['Scene-Child-Root', 'Child'],
  ]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadatada,
  label: null,
  importInfo: null,
}

const testComponentSceneChildElementRoot: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  elementPath: EP.elementPath([
    [BakedInStoryboardUID, TestScenePath, 'Scene-Child'],
    ['Scene-Child-Root'],
  ]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadatada,
  label: null,
  importInfo: null,
}

const testComponentSceneChildElement: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  elementPath: EP.elementPath([[BakedInStoryboardUID, TestScenePath, 'Scene-Child']]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadatada,
  label: null,
  importInfo: null,
}

const testComponentSceneElement: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  elementPath: EP.elementPath([[BakedInStoryboardUID, TestScenePath]]),
  element: right(jsxTestElement('Scene', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadatada,
  label: null,
  importInfo: null,
}

const testComponentSceneElementProps: ElementProps = {
  style: {
    width: 100,
    height: 100,
  },
}

const testStoryboardGrandChildElement: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  elementPath: EP.elementPath([[BakedInStoryboardUID, 'Child', 'GrandChild']]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadatada,
  label: null,
  importInfo: null,
}

const testStoryboardChildElement: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  elementPath: EP.elementPath([[BakedInStoryboardUID, 'Child']]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadatada,
  label: null,
  importInfo: null,
}

const testStoryboardElement: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 0, height: 0 }),
  elementPath: EP.elementPath([[BakedInStoryboardUID]]),
  element: right(jsxTestElement('Storyboard', [], [])),
  componentInstance: true,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadatada,
  label: null,
  importInfo: null,
}

const testElementMetadataMap: ElementInstanceMetadataMap = {
  [EP.toString(testComponentMetadataChild1.elementPath)]: testComponentMetadataChild1,
  [EP.toString(testComponentMetadataChild2.elementPath)]: testComponentMetadataChild2,
  [EP.toString(testComponentMetadataChild3.elementPath)]: testComponentMetadataChild3,
  [EP.toString(testComponentMetadataGrandchild.elementPath)]: testComponentMetadataGrandchild,
  [EP.toString(testComponentRoot1.elementPath)]: testComponentRoot1,
  [EP.toString(testComponentSceneChildElementRootChild.elementPath)]:
    testComponentSceneChildElementRootChild,
  [EP.toString(testComponentSceneChildElementRoot.elementPath)]: testComponentSceneChildElementRoot,
  [EP.toString(testComponentSceneChildElement.elementPath)]: testComponentSceneChildElement,
  [EP.toString(testComponentSceneElement.elementPath)]: testComponentSceneElement,
  [EP.toString(testStoryboardGrandChildElement.elementPath)]: testStoryboardGrandChildElement,
  [EP.toString(testStoryboardChildElement.elementPath)]: testStoryboardChildElement,
  [EP.toString(testStoryboardElement.elementPath)]: testStoryboardElement,
}

const testJsxMetadata = testElementMetadataMap

describe('findElementByElementPath', () => {
  it('works with an empty object', () => {
    const actualResult = MetadataUtils.findElementByElementPath(
      {},
      EP.elementPath([
        [BakedInStoryboardUID, TestScenePath],
        ['View', 'View0'],
      ]),
    )
    expect(actualResult).toBeNull()
  })
  it('returns null for a nonsense path', () => {
    const actualResult = MetadataUtils.findElementByElementPath(
      {},
      EP.elementPath([
        [BakedInStoryboardUID, TestScenePath],
        ['Hats', 'Cats'],
      ]),
    )
    expect(actualResult).toBeNull()
  })
  it('returns null for a partially nonsense path', () => {
    const actualResult = MetadataUtils.findElementByElementPath(
      {},
      EP.elementPath([
        [BakedInStoryboardUID, TestScenePath],
        ['View', 'Cats'],
      ]),
    )
    expect(actualResult).toBeNull()
  })
  it('returns the element from the map', () => {
    const actualResult = MetadataUtils.findElementByElementPath(
      testElementMetadataMap,
      EP.elementPath([[BakedInStoryboardUID, TestScenePath], ['View']]),
    )
    expect(actualResult).toBe(testComponentRoot1)
  })
  it('returns the element for a child of the root', () => {
    const actualResult = MetadataUtils.findElementByElementPath(
      testElementMetadataMap,
      EP.elementPath([
        [BakedInStoryboardUID, TestScenePath],
        ['View', 'View1'],
      ]),
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
      elementPath: EP.elementPath([
        [BakedInStoryboardUID, TestScenePath],
        ['Dummy', 'Element'],
      ]),
      element: right(jsxTestElement(elementName, [], [])),
      componentInstance: false,
      isEmotionOrStyledComponent: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements,
      computedStyle: emptyComputedStyle,
      attributeMetadatada: emptyAttributeMetadatada,
      label: null,
      importInfo: null,
    }
  }

  it('Returns true for a utopia-api View', () => {
    const element = dummyInstanceDataForElementType('View')
    const actualResult = MetadataUtils.targetElementSupportsChildren(element)
    expect(actualResult).toBeTruthy()
  })
  it('Returns true for a button', () => {
    const element = dummyInstanceDataForElementType('button')
    const actualResult = MetadataUtils.targetElementSupportsChildren(element)
    expect(actualResult).toBeTruthy()
  })
  it('Returns true for a div', () => {
    const element = dummyInstanceDataForElementType('div')
    const actualResult = MetadataUtils.targetElementSupportsChildren(element)
    expect(actualResult).toBeTruthy()
  })
  it('Returns true for a span', () => {
    const element = dummyInstanceDataForElementType('span')
    const actualResult = MetadataUtils.targetElementSupportsChildren(element)
    expect(actualResult).toBeTruthy()
  })
  it('Returns true for an animated.div', () => {
    const element = dummyInstanceDataForElementType(jsxElementName('animated', ['div']))
    const actualResult = MetadataUtils.targetElementSupportsChildren(element)
    expect(actualResult).toBeTruthy()
  })
})

describe('isPinnedAndNotAbsolutePositioned', () => {
  it('returns true for a pinned element that is not absolute positioned', () => {
    const elementMapForTest: ElementInstanceMetadataMap = {
      [EP.toString(testComponentRoot1.elementPath)]: {
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
        elementMapForTest,
        EP.elementPath([[BakedInStoryboardUID, TestScenePath], ['View']]),
      ),
    ).toEqual(true)
  })
  it('returns false for a flex element that is not absolute positioned', () => {
    const elementMapForTest: ElementInstanceMetadataMap = {
      [EP.toString(testComponentRoot1.elementPath)]: {
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
        elementMapForTest,
        EP.elementPath([[BakedInStoryboardUID, TestScenePath], ['View']]),
      ),
    ).toEqual(false)
  })
  it('returns false for a pinned element that is absolute positioned', () => {
    const elementMapForTest: ElementInstanceMetadataMap = {
      [EP.toString(testComponentRoot1.elementPath)]: {
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
        elementMapForTest,
        EP.elementPath([[BakedInStoryboardUID, TestScenePath], ['View']]),
      ),
    ).toEqual(false)
  })
  it('returns false for a flex element that is absolute positioned', () => {
    const elementMapForTest: ElementInstanceMetadataMap = {
      [EP.toString(testComponentRoot1.elementPath)]: {
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
        elementMapForTest,
        EP.elementPath([[BakedInStoryboardUID, TestScenePath], ['View']]),
      ),
    ).toEqual(false)
  })
})

describe('getElementLabel', () => {
  const divPath = EP.elementPath([[BakedInStoryboardUID, 'scene-0'], ['div-1']])
  const spanPath = EP.appendToPath(divPath, 'span-1')
  const textBlock = jsxTextBlock('test text')
  const spanElement = jsxElement(
    'span',
    'span-1',
    jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('span-1', emptyComments) }),
    [textBlock],
  )
  const spanElementMetadata = elementInstanceMetadata(
    spanPath,
    right(spanElement),
    zeroRectangle as CanvasRectangle,
    zeroRectangle as LocalRectangle,
    false,
    false,
    emptySpecialSizeMeasurements,
    emptyComputedStyle,
    emptyAttributeMetadatada,
    null,
    null,
  )
  const spanElementProps: ElementProps = {
    'data-uid': 'span-1',
  }
  const divElement = jsxElement(
    'div',
    'div-1',
    jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('div-1', emptyComments) }),
    [spanElement],
  )
  const divElementMetadata = elementInstanceMetadata(
    divPath,
    right(divElement),
    zeroRectangle as CanvasRectangle,
    zeroRectangle as LocalRectangle,
    false,
    false,
    emptySpecialSizeMeasurements,
    emptyComputedStyle,
    emptyAttributeMetadatada,
    null,
    null,
  )
  const divElementProps: ElementProps = {
    'data-uid': 'div-1',
  }
  const metadata: ElementInstanceMetadataMap = {
    [EP.toString(spanElementMetadata.elementPath)]: spanElementMetadata,
    [EP.toString(divElementMetadata.elementPath)]: divElementMetadata,
  }
  const allElementProps: AllElementProps = {
    [EP.toString(spanElementMetadata.elementPath)]: spanElementProps,
    [EP.toString(divElementMetadata.elementPath)]: divElementProps,
  }
  it('the label of a spin containing text is that text', () => {
    const actualResult = MetadataUtils.getElementLabel(allElementProps, spanPath, metadata)
    expect(actualResult).toEqual('test text')
  })
})

describe('getStoryboardMetadata', () => {
  it('finds the storyboard instance metadata', () => {
    const actualResult = MetadataUtils.getStoryboardMetadata(testJsxMetadata)
    expect(actualResult).toEqual(testStoryboardElement)
  })
})

describe('getting the root paths', () => {
  it('getAllStoryboardChildren returns instance metadata of all children of the storyboard', () => {
    const actualResult = MetadataUtils.getAllStoryboardChildren(testJsxMetadata)
    const expectedResult: Array<ElementInstanceMetadata> = [
      testComponentSceneElement,
      testStoryboardChildElement,
    ]
    expect(actualResult).toEqual(expectedResult)
  })

  it('getAllStoryboardChildrenPaths returns paths of all children of the storyboard', () => {
    const actualResult = MetadataUtils.getAllStoryboardChildrenPaths(testJsxMetadata)
    const expectedResult: Array<ElementPath> = [
      testComponentSceneElement.elementPath,
      testStoryboardChildElement.elementPath,
    ]
    expect(actualResult).toEqual(expectedResult)
  })

  it('getAllCanvasRootPaths returns paths of the top level children of the storyboard, replacing scenes with their root views', () => {
    const actualResult = MetadataUtils.getAllCanvasRootPaths(testJsxMetadata)
    const expectedResult: Array<ElementPath> = [
      testComponentRoot1.elementPath,
      testStoryboardChildElement.elementPath,
    ]
    expect(actualResult).toEqual(expectedResult)
  })

  it('getAllPaths returns the instance paths in a depth first manner', () => {
    const actualResult = MetadataUtils.getAllPaths(testJsxMetadata)
    const expectedResult: Array<ElementPath> = [
      testComponentSceneElement.elementPath,
      testComponentRoot1.elementPath,
      testComponentMetadataChild1.elementPath,
      testComponentMetadataChild2.elementPath,
      testComponentMetadataChild3.elementPath,
      testComponentMetadataGrandchild.elementPath,
      testComponentSceneChildElement.elementPath,
      testComponentSceneChildElementRoot.elementPath,
      testComponentSceneChildElementRootChild.elementPath,
      testStoryboardChildElement.elementPath,
      testStoryboardGrandChildElement.elementPath,
    ]
    expect(actualResult).toEqual(expectedResult)
  })
})

describe('createOrderedElementPathsFromElements returns all of the ordered navigator targets, visible and not', () => {
  const expectedNavigatorTargets: Array<ElementPath> = [
    testComponentSceneElement.elementPath,
    testComponentSceneChildElement.elementPath,
    testComponentSceneChildElementRoot.elementPath,
    testComponentSceneChildElementRootChild.elementPath,
    testComponentRoot1.elementPath,
    testComponentMetadataChild1.elementPath,
    testComponentMetadataChild2.elementPath,
    testComponentMetadataChild3.elementPath,
    testComponentMetadataGrandchild.elementPath,
    testStoryboardChildElement.elementPath,
    testStoryboardGrandChildElement.elementPath,
  ]

  it('with no collapsed paths', () => {
    const actualResult = MetadataUtils.createOrderedElementPathsFromElements(testJsxMetadata, [])

    expect(actualResult.navigatorTargets).toEqual(expectedNavigatorTargets)
    expect(actualResult.visibleNavigatorTargets).toEqual(expectedNavigatorTargets)
  })

  it('with the scene collapsed', () => {
    const actualResult = MetadataUtils.createOrderedElementPathsFromElements(testJsxMetadata, [
      testComponentSceneElement.elementPath,
    ])

    expect(actualResult.navigatorTargets).toEqual(expectedNavigatorTargets)
    expect(actualResult.visibleNavigatorTargets).toEqual([
      testComponentSceneElement.elementPath,
      testStoryboardChildElement.elementPath,
      testStoryboardGrandChildElement.elementPath,
    ])
  })

  it('with collapsed roots', () => {
    const actualResult = MetadataUtils.createOrderedElementPathsFromElements(testJsxMetadata, [
      testComponentRoot1.elementPath,
      testComponentSceneChildElement.elementPath,
    ])

    expect(actualResult.navigatorTargets).toEqual(expectedNavigatorTargets)
    expect(actualResult.visibleNavigatorTargets).toEqual([
      testComponentSceneElement.elementPath,
      testComponentSceneChildElement.elementPath,
      testComponentRoot1.elementPath,
      testStoryboardChildElement.elementPath,
      testStoryboardGrandChildElement.elementPath,
    ])
  })
})

describe('Re-ordering elements', () => {
  const oldMetadata: ElementInstanceMetadataMap = {
    [EP.toString(testComponentMetadataChild1.elementPath)]: testComponentMetadataChild1,
    [EP.toString(testComponentMetadataChild2.elementPath)]: testComponentMetadataChild2,
    [EP.toString(testComponentMetadataChild3.elementPath)]: testComponentMetadataChild3,
    [EP.toString(testComponentRoot1.elementPath)]: testComponentRoot1,
  }
  const newMetadata: ElementInstanceMetadataMap = {
    [EP.toString(testComponentMetadataChild1.elementPath)]: testComponentMetadataChild1,
    [EP.toString(testComponentMetadataChild3.elementPath)]: testComponentMetadataChild3,
    [EP.toString(testComponentMetadataChild2.elementPath)]: testComponentMetadataChild2,
    [EP.toString(testComponentRoot1.elementPath)]: testComponentRoot1,
  }
  const keptMetadata = ElementInstanceMetadataMapKeepDeepEquality(oldMetadata, newMetadata).value

  const reorderedElementPath = testComponentMetadataChild2.elementPath
  const parentPath = testComponentRoot1.elementPath

  it('is correctly reflected in getViewZIndexFromMetadata', () => {
    expect(MetadataUtils.getViewZIndexFromMetadata(keptMetadata, reorderedElementPath)).toEqual(
      MetadataUtils.getViewZIndexFromMetadata(newMetadata, reorderedElementPath),
    )
  })
  it('is correctly reflected in getSiblings', () => {
    expect(MetadataUtils.getSiblings(keptMetadata, reorderedElementPath)).toEqual(
      MetadataUtils.getSiblings(newMetadata, reorderedElementPath),
    )
  })
  it('is correctly reflected in getChildrenPaths', () => {
    expect(MetadataUtils.getChildrenPaths(keptMetadata, parentPath)).toEqual(
      MetadataUtils.getChildrenPaths(newMetadata, parentPath),
    )
  })
  it('is correctly reflected in getChildren', () => {
    expect(MetadataUtils.getChildren(keptMetadata, parentPath)).toEqual(
      MetadataUtils.getChildren(newMetadata, parentPath),
    )
  })
  it('is correctly reflected in getImmediateChildrenPaths', () => {
    expect(MetadataUtils.getImmediateChildrenPaths(keptMetadata, parentPath)).toEqual(
      MetadataUtils.getImmediateChildrenPaths(newMetadata, parentPath),
    )
  })
  it('is correctly reflected in getImmediateChildren', () => {
    expect(MetadataUtils.getImmediateChildren(keptMetadata, parentPath)).toEqual(
      MetadataUtils.getImmediateChildren(newMetadata, parentPath),
    )
  })
  it('is correctly reflected in getAllChildrenIncludingUnfurledFocusedComponents', () => {
    expect(
      MetadataUtils.getAllChildrenIncludingUnfurledFocusedComponents(parentPath, keptMetadata),
    ).toEqual(
      MetadataUtils.getAllChildrenIncludingUnfurledFocusedComponents(parentPath, newMetadata),
    )
  })
  it('is correctly reflected in getAllChildrenElementsIncludingUnfurledFocusedComponents', () => {
    expect(
      MetadataUtils.getAllChildrenElementsIncludingUnfurledFocusedComponents(
        parentPath,
        keptMetadata,
      ),
    ).toEqual(
      MetadataUtils.getAllChildrenElementsIncludingUnfurledFocusedComponents(
        parentPath,
        newMetadata,
      ),
    )
  })
  it('is correctly reflected in getAllPaths', () => {
    expect(MetadataUtils.getAllPaths(keptMetadata)).toEqual(MetadataUtils.getAllPaths(newMetadata))
  })
  it('is correctly reflected in getAllPathsIncludingUnfurledFocusedComponents', () => {
    expect(MetadataUtils.getAllPathsIncludingUnfurledFocusedComponents(keptMetadata)).toEqual(
      MetadataUtils.getAllPathsIncludingUnfurledFocusedComponents(newMetadata),
    )
  })
  it('is correctly reflected in createOrderedElementPathsFromElements', () => {
    expect(MetadataUtils.createOrderedElementPathsFromElements(keptMetadata, [])).toEqual(
      MetadataUtils.createOrderedElementPathsFromElements(newMetadata, []),
    )
  })
})
