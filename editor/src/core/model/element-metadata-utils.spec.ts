import { LayoutSystem } from 'utopia-api'
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
} from '../shared/element-template'
import { sampleImportsForTests } from './test-ui-js-file'
import { BakedInStoryboardUID } from './scene-utils'
import { TemplatePath } from '../shared/project-file-types'

const TestScenePath = 'scene-aaa'

const testComponentMetadataChild1: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  templatePath: TP.instancePath([BakedInStoryboardUID, TestScenePath], ['View', 'View0']),
  props: {},
  element: right(jsxTestElement('View', {}, [])),
  children: [],
  componentInstance: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
}
const testComponentMetadataChild2: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  templatePath: TP.instancePath([BakedInStoryboardUID, TestScenePath], ['View', 'View1']),
  props: {},
  element: right(jsxTestElement('View', {}, [])),
  children: [],
  componentInstance: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
}

const testComponentMetadataChild3: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  templatePath: TP.instancePath([BakedInStoryboardUID, TestScenePath], ['View', 'View2']),
  props: {},
  element: right(jsxTestElement('View', {}, [])),
  children: [
    {
      globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
      localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
      templatePath: TP.instancePath(
        [BakedInStoryboardUID, TestScenePath],
        ['View', 'View2', 'View0'],
      ),
      props: {
        cica: 'hello',
      },
      element: right(jsxTestElement('View', {}, [])),
      children: [],
      componentInstance: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements,
      computedStyle: emptyComputedStyle,
    },
  ],
  componentInstance: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
}

const testComponentRoot1: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  templatePath: TP.instancePath([BakedInStoryboardUID, TestScenePath], ['View']),
  props: {},
  element: right(jsxTestElement('View', {}, [])),
  children: [testComponentMetadataChild1, testComponentMetadataChild2, testComponentMetadataChild3],
  componentInstance: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
}

const testComponentScene: ComponentMetadata = {
  scenePath: TP.scenePath([BakedInStoryboardUID, TestScenePath]),
  templatePath: TP.instancePath([], [BakedInStoryboardUID, 'scene-aaa']),
  component: 'MyView',
  container: {
    layoutSystem: LayoutSystem.PinSystem,
  },
  rootElements: [testComponentRoot1],
  type: 'static',
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

describe('findElements', () => {
  it('Finds the element metadata', () => {
    const foundViewsWithHelloProp = MetadataUtils.findElements(testComponentMetadata, (element) => {
      return element.props['cica'] != null
    })

    expect(foundViewsWithHelloProp).toHaveLength(1)
    expect(foundViewsWithHelloProp[0].props.cica).toEqual('hello')

    const notFoundViews = MetadataUtils.findElements(
      testComponentMetadata,
      (element) => element.props.nonexistentProp != null,
    )
    expect(notFoundViews).toHaveLength(0)
  })
})

describe('getElementByInstancePathMaybe', () => {
  it('works with an empty array', () => {
    const actualResult = MetadataUtils.getElementByInstancePathMaybe(
      [],
      TP.instancePath([BakedInStoryboardUID, TestScenePath], ['View', 'View0']),
    )
    expect(actualResult).toBeNull()
  })
  it('returns null for a nonsense path', () => {
    const actualResult = MetadataUtils.getElementByInstancePathMaybe(
      [],
      TP.instancePath([BakedInStoryboardUID, TestScenePath], ['Hats', 'Cats']),
    )
    expect(actualResult).toBeNull()
  })
  it('returns null for a partially nonsense path', () => {
    const actualResult = MetadataUtils.getElementByInstancePathMaybe(
      [],
      TP.instancePath([BakedInStoryboardUID, TestScenePath], ['View', 'Cats']),
    )
    expect(actualResult).toBeNull()
  })
  it('returns the element from the root', () => {
    const actualResult = MetadataUtils.getElementByInstancePathMaybe(
      testComponentMetadata,
      TP.instancePath([BakedInStoryboardUID, TestScenePath], ['View']),
    )
    expect(actualResult).toBe(testComponentRoot1)
  })
  it('returns the element for a child of the root', () => {
    const actualResult = MetadataUtils.getElementByInstancePathMaybe(
      testComponentMetadata,
      TP.instancePath([BakedInStoryboardUID, TestScenePath], ['View', 'View1']),
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
      templatePath: TP.instancePath([BakedInStoryboardUID, TestScenePath], ['Dummy', 'Element']),
      props: {},
      element: right(jsxTestElement(elementName, {}, [])),
      children: [],
      componentInstance: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements,
      computedStyle: emptyComputedStyle,
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
    const metadataForTest: Array<ComponentMetadata> = [
      {
        ...testComponentScene,
        rootElements: [
          {
            ...testComponentRoot1,
            specialSizeMeasurements: {
              ...testComponentRoot1.specialSizeMeasurements,
              parentLayoutSystem: 'nonfixed',
              position: 'static',
            },
          },
        ],
      },
    ]
    expect(
      MetadataUtils.isPinnedAndNotAbsolutePositioned(
        metadataForTest,
        TP.instancePath([BakedInStoryboardUID, TestScenePath], ['View']),
      ),
    ).toEqual(true)
  })
  it('returns false for a flex element that is not absolute positioned', () => {
    const metadataForTest: Array<ComponentMetadata> = [
      {
        ...testComponentScene,
        rootElements: [
          {
            ...testComponentRoot1,
            specialSizeMeasurements: {
              ...testComponentRoot1.specialSizeMeasurements,
              parentLayoutSystem: 'flex',
              position: 'static',
            },
          },
        ],
      },
    ]
    expect(
      MetadataUtils.isPinnedAndNotAbsolutePositioned(
        metadataForTest,
        TP.instancePath([BakedInStoryboardUID, TestScenePath], ['View']),
      ),
    ).toEqual(false)
  })
  it('returns false for a pinned element that is absolute positioned', () => {
    const metadataForTest: Array<ComponentMetadata> = [
      {
        ...testComponentScene,
        rootElements: [
          {
            ...testComponentRoot1,
            specialSizeMeasurements: {
              ...testComponentRoot1.specialSizeMeasurements,
              parentLayoutSystem: 'nonfixed',
              position: 'absolute',
            },
          },
        ],
      },
    ]
    expect(
      MetadataUtils.isPinnedAndNotAbsolutePositioned(
        metadataForTest,
        TP.instancePath([BakedInStoryboardUID, TestScenePath], ['View']),
      ),
    ).toEqual(false)
  })
  it('returns false for a flex element that is absolute positioned', () => {
    const metadataForTest: Array<ComponentMetadata> = [
      {
        ...testComponentScene,
        rootElements: [
          {
            ...testComponentRoot1,
            specialSizeMeasurements: {
              ...testComponentRoot1.specialSizeMeasurements,
              parentLayoutSystem: 'flex',
              position: 'absolute',
            },
          },
        ],
      },
    ]
    expect(
      MetadataUtils.isPinnedAndNotAbsolutePositioned(
        metadataForTest,
        TP.instancePath([BakedInStoryboardUID, TestScenePath], ['View']),
      ),
    ).toEqual(false)
  })
})

describe('getElementLabel', () => {
  const scenePath = TP.scenePath([BakedInStoryboardUID, 'scene-0'])
  const instancePath = TP.instancePath([], [BakedInStoryboardUID, `scene-0`])
  const divPath = TP.appendToPath(scenePath, 'div-1')
  const spanPath = TP.appendToPath(divPath, 'span-1')
  const textBlock = jsxTextBlock('test text')
  const spanElement = jsxElement(
    'span',
    { 'data-uid': jsxAttributeValue('span-1') },
    [textBlock],
    null,
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
    emptySpecialSizeMeasurements,
    emptyComputedStyle,
  )
  const divElement = jsxElement(
    'div',
    { 'data-uid': jsxAttributeValue('div-1') },
    [spanElement],
    null,
  )
  const divElementMetadata = elementInstanceMetadata(
    divPath,
    right(divElement),
    {
      'data-uid': 'div-1',
    },
    zeroRectangle as CanvasRectangle,
    zeroRectangle as LocalRectangle,
    [spanElementMetadata],
    false,
    emptySpecialSizeMeasurements,
    emptyComputedStyle,
  )
  const metadata: Array<ComponentMetadata> = [
    {
      scenePath: scenePath,
      templatePath: instancePath,
      component: 'App',
      container: { layoutSystem: LayoutSystem.PinSystem },
      type: 'static',
      rootElements: [divElementMetadata],
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
  it('the label of a spin containing text is that text', () => {
    const actualResult = MetadataUtils.getElementLabel(spanPath, metadata)
    expect(actualResult).toEqual('test text')
  })
})

describe('getAllPaths', () => {
  it('returns the paths in a depth first manner', () => {
    const actualResult = MetadataUtils.getAllPaths(testComponentMetadata)
    const expectedResult: Array<TemplatePath> = [
      testComponentScene.scenePath,
      testComponentRoot1.templatePath,
      testComponentMetadataChild1.templatePath,
      testComponentMetadataChild2.templatePath,
      testComponentMetadataChild3.templatePath,
      TP.instancePath([BakedInStoryboardUID, TestScenePath], ['View', 'View2', 'View0']),
    ]
    expect(actualResult).toEqual(expectedResult)
  })
})
