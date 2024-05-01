import * as EP from '../shared/element-path'
import type { LocalRectangle, CanvasRectangle } from '../shared/math-utils'
import { canvasRectangle, localRectangle, zeroRectangle } from '../shared/math-utils'
import { right } from '../shared/either'
import { MetadataUtils } from './element-metadata-utils'
import type {
  ElementInstanceMetadata,
  JSXElementName,
  ElementInstanceMetadataMap,
  JSXElementChildren,
  JSXElement,
  ImportInfo,
} from '../shared/element-template'
import {
  emptySpecialSizeMeasurements,
  jsxElementName,
  jsxTestElement,
  jsxTextBlock,
  jsxElement,
  jsExpressionValue,
  elementInstanceMetadata,
  emptyComputedStyle,
  jsxAttributesFromMap,
  emptyAttributeMetadata,
  emptyComments,
  jsxFragment,
  jsExpressionOtherJavaScript,
  isJSXElement,
  importedOrigin,
  jsOpaqueArbitraryStatement,
} from '../shared/element-template'
import { sampleImportsForTests } from './test-ui-js-file.test-utils'
import { BakedInStoryboardUID } from './scene-utils'
import type { ElementPath, ParseSuccess, ParsedTextFile } from '../shared/project-file-types'
import {
  isParseSuccess,
  RevisionsState,
  textFile,
  textFileContents,
} from '../shared/project-file-types'
import type { AllElementProps, ElementProps } from '../../components/editor/store/editor-state'
import {
  NavigatorEntry,
  regularNavigatorEntry,
  StoryboardFilePath,
} from '../../components/editor/store/editor-state'
import { parseCode } from '../workers/parser-printer/parser-printer'
import { TestAppUID, TestSceneUID } from '../../components/canvas/ui-jsx.test-utils'
import { contentsToTree } from '../../components/assets'
import { SampleNodeModules } from '../../components/custom-code/code-file.test-utils'
import { findJSXElementAtStaticPath } from './element-template-utils'
import { getUtopiaJSXComponentsFromSuccess } from './project-file-utils'
import type { ElementPathTrees } from '../shared/element-path-tree'
import { elementPathTree } from '../shared/element-path-tree'

const TestScenePath = 'scene-aaa'

const testComponentMetadataChild1: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  nonRoundedGlobalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
  elementPath: EP.elementPath([
    [BakedInStoryboardUID, TestScenePath],
    ['View', 'View0'],
  ]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadata,
  label: null,
  importInfo: null,
  conditionValue: 'not-a-conditional',
  textContent: null,
  earlyReturn: null,
  assignedToProp: null,
}
const testComponentMetadataChild2: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  nonRoundedGlobalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
  elementPath: EP.elementPath([
    [BakedInStoryboardUID, TestScenePath],
    ['View', 'View1'],
  ]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadata,
  label: null,
  importInfo: null,
  conditionValue: 'not-a-conditional',
  textContent: null,
  earlyReturn: null,
  assignedToProp: null,
}

const testComponentMetadataGrandchild: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  nonRoundedGlobalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
  elementPath: EP.elementPath([
    [BakedInStoryboardUID, TestScenePath],
    ['View', 'View2', 'View0'],
  ]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadata,
  label: null,
  importInfo: null,
  conditionValue: 'not-a-conditional',
  textContent: null,
  earlyReturn: null,
  assignedToProp: null,
}

const testComponentPropsGrandchild: ElementProps = {
  cica: 'hello',
}

const testComponentMetadataChild3: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  nonRoundedGlobalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
  elementPath: EP.elementPath([
    [BakedInStoryboardUID, TestScenePath],
    ['View', 'View2'],
  ]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadata,
  label: null,
  importInfo: null,
  conditionValue: 'not-a-conditional',
  textContent: null,
  earlyReturn: null,
  assignedToProp: null,
}

const testComponentRoot1: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  nonRoundedGlobalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
  elementPath: EP.elementPath([[BakedInStoryboardUID, TestScenePath], ['View']]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadata,
  label: null,
  importInfo: null,
  conditionValue: 'not-a-conditional',
  textContent: null,
  earlyReturn: null,
  assignedToProp: null,
}

const testComponentSceneChildElementRootChild: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  nonRoundedGlobalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
  elementPath: EP.elementPath([
    [BakedInStoryboardUID, TestScenePath, 'Scene-Child'],
    ['Scene-Child-Root', 'Child'],
  ]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadata,
  label: null,
  importInfo: null,
  conditionValue: 'not-a-conditional',
  textContent: null,
  earlyReturn: null,
  assignedToProp: null,
}

const testComponentSceneChildElementRoot: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  nonRoundedGlobalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
  elementPath: EP.elementPath([
    [BakedInStoryboardUID, TestScenePath, 'Scene-Child'],
    ['Scene-Child-Root'],
  ]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadata,
  label: null,
  importInfo: null,
  conditionValue: 'not-a-conditional',
  textContent: null,
  earlyReturn: null,
  assignedToProp: null,
}

const testComponentSceneChildElement: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  nonRoundedGlobalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
  elementPath: EP.elementPath([[BakedInStoryboardUID, TestScenePath, 'Scene-Child']]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadata,
  label: null,
  importInfo: null,
  conditionValue: 'not-a-conditional',
  textContent: null,
  earlyReturn: null,
  assignedToProp: null,
}

const testComponentSceneElement: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  nonRoundedGlobalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
  elementPath: EP.elementPath([[BakedInStoryboardUID, TestScenePath]]),
  element: right(jsxTestElement('Scene', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadata,
  label: null,
  importInfo: null,
  conditionValue: 'not-a-conditional',
  textContent: null,
  earlyReturn: null,
  assignedToProp: null,
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
  nonRoundedGlobalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
  elementPath: EP.elementPath([[BakedInStoryboardUID, 'Child', 'GrandChild']]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadata,
  label: null,
  importInfo: null,
  conditionValue: 'not-a-conditional',
  textContent: null,
  earlyReturn: null,
  assignedToProp: null,
}

const testStoryboardChildElement: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
  nonRoundedGlobalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
  elementPath: EP.elementPath([[BakedInStoryboardUID, 'Child']]),
  element: right(jsxTestElement('View', [], [])),
  componentInstance: false,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadata,
  label: null,
  importInfo: null,
  conditionValue: 'not-a-conditional',
  textContent: null,
  earlyReturn: null,
  assignedToProp: null,
}

const testStoryboardElement: ElementInstanceMetadata = {
  globalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
  localFrame: localRectangle({ x: 0, y: 0, width: 0, height: 0 }),
  nonRoundedGlobalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
  elementPath: EP.elementPath([[BakedInStoryboardUID]]),
  element: right(jsxTestElement('Storyboard', [], [])),
  componentInstance: true,
  isEmotionOrStyledComponent: false,
  specialSizeMeasurements: emptySpecialSizeMeasurements,
  computedStyle: emptyComputedStyle,
  attributeMetadatada: emptyAttributeMetadata,
  label: null,
  importInfo: null,
  conditionValue: 'not-a-conditional',
  textContent: null,
  earlyReturn: null,
  assignedToProp: null,
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

function dummyInstanceDataForElementType(
  elementName: JSXElementName | string,
  elementPath: ElementPath,
  children: JSXElementChildren = [],
  importInfo: ImportInfo | null = null,
): ElementInstanceMetadata {
  return {
    globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
    localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
    nonRoundedGlobalFrame: canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
    elementPath: elementPath,
    element: right(jsxTestElement(elementName, [], children)),
    componentInstance: false,
    isEmotionOrStyledComponent: false,
    specialSizeMeasurements: emptySpecialSizeMeasurements,
    computedStyle: emptyComputedStyle,
    attributeMetadatada: emptyAttributeMetadata,
    label: null,
    importInfo: importInfo,
    conditionValue: 'not-a-conditional',
    textContent: null,
    earlyReturn: null,
    assignedToProp: null,
  }
}

function parseResultFromCode(filename: string, code: string): ParsedTextFile {
  const parseResult = parseCode(filename, code, null, new Set(), 'do-not-apply-steganography')
  if (isParseSuccess(parseResult)) {
    return parseResult
  } else {
    throw new Error(`Not a parse success: ${parseResult.type}`)
  }
}

describe('targetElementSupportsChildren', () => {
  it('returns true for a utopia-api View', () => {
    const path = EP.elementPath([
      [BakedInStoryboardUID, TestScenePath],
      ['Dummy', 'Element'],
    ])
    const element = dummyInstanceDataForElementType('View', path)
    const actualResult = MetadataUtils.targetElementSupportsChildren(
      {},
      path,
      { [EP.toString(path)]: element },
      {},
    )
    expect(actualResult).toEqual(true)
  })
  it('returns true for an unparsed button', () => {
    const path = EP.elementPath([
      [BakedInStoryboardUID, TestScenePath],
      ['Dummy', 'Element'],
    ])
    const element = dummyInstanceDataForElementType('button', path)
    const actualResult = MetadataUtils.targetElementSupportsChildren(
      {},
      path,
      { [EP.toString(path)]: element },
      {},
    )
    expect(actualResult).toEqual(true)
  })
  it('returns true for a parsed button', () => {
    const path = EP.elementPath([
      [BakedInStoryboardUID, TestScenePath],
      ['Dummy', 'Element'],
    ])
    const element = dummyInstanceDataForElementType(jsxElementName('button', []), path)
    const actualResult = MetadataUtils.targetElementSupportsChildren(
      {},
      path,
      { [EP.toString(path)]: element },
      {},
    )
    expect(actualResult).toEqual(true)
  })
  it('returns true for an unparsed div', () => {
    const path = EP.elementPath([
      [BakedInStoryboardUID, TestScenePath],
      ['Dummy', 'Element'],
    ])
    const element = dummyInstanceDataForElementType('div', path)
    const actualResult = MetadataUtils.targetElementSupportsChildren(
      {},
      path,
      { [EP.toString(path)]: element },
      {},
    )
    expect(actualResult).toEqual(true)
  })
  it('returns true for a parsed div', () => {
    const path = EP.elementPath([
      [BakedInStoryboardUID, TestScenePath],
      ['Dummy', 'Element'],
    ])
    const element = dummyInstanceDataForElementType(jsxElementName('div', []), path)
    const actualResult = MetadataUtils.targetElementSupportsChildren(
      {},
      path,
      { [EP.toString(path)]: element },
      {},
    )
    expect(actualResult).toEqual(true)
  })
  it('returns true for a parsed div with an arbitrary jsx block child', () => {
    const path = EP.elementPath([
      [BakedInStoryboardUID, TestScenePath],
      ['Dummy', 'Element'],
    ])
    const element = dummyInstanceDataForElementType(
      jsxElementName('div', []),
      path,
      [
        jsExpressionOtherJavaScript(
          [],
          '<div />',
          '<div />;',
          'return <div />;',
          [],
          null,
          {},
          emptyComments,
        ),
      ], // Whatever, close enough
    )
    const actualResult = MetadataUtils.targetElementSupportsChildren(
      {},
      path,
      { [EP.toString(path)]: element },
      {},
    )
    expect(actualResult).toEqual(true)
  })
  it('returns true for a parsed div with another parsed div child', () => {
    const path = EP.elementPath([
      [BakedInStoryboardUID, TestScenePath],
      ['Dummy', 'Element'],
    ])
    const element = dummyInstanceDataForElementType(jsxElementName('div', []), path, [
      jsxTestElement('div', [], []),
    ])
    const actualResult = MetadataUtils.targetElementSupportsChildren(
      {},
      path,
      { [EP.toString(path)]: element },
      {},
    )
    expect(actualResult).toEqual(true)
  })
  it('returns true for a parsed div with an empty fragment child', () => {
    const path = EP.elementPath([
      [BakedInStoryboardUID, TestScenePath],
      ['Dummy', 'Element'],
    ])
    const element = dummyInstanceDataForElementType(jsxElementName('div', []), path, [
      jsxFragment('fff', [], true),
    ])
    const actualResult = MetadataUtils.targetElementSupportsChildren(
      {},
      path,
      { [EP.toString(path)]: element },
      {},
    )
    expect(actualResult).toEqual(true)
  })
  it('returns true for a parsed div with a fragment child containing another parsed div', () => {
    const path = EP.elementPath([
      [BakedInStoryboardUID, TestScenePath],
      ['Dummy', 'Element'],
    ])
    const element = dummyInstanceDataForElementType(jsxElementName('div', []), path, [
      jsxFragment('fff', [jsxTestElement('div', [], [])], true),
    ])
    const actualResult = MetadataUtils.targetElementSupportsChildren(
      {},
      path,
      { [EP.toString(path)]: element },
      {},
    )
    expect(actualResult).toEqual(true)
  })
  it('returns true for a parsed div with a fragment child containing an arbitrary jsx block', () => {
    const path = EP.elementPath([
      [BakedInStoryboardUID, TestScenePath],
      ['Dummy', 'Element'],
    ])
    const element = dummyInstanceDataForElementType(jsxElementName('div', []), path, [
      jsxFragment(
        'fff',
        [
          jsxTestElement(
            'div',
            [],
            [
              jsExpressionOtherJavaScript(
                [],
                '<div />',
                '<div />;',
                'return <div />;',
                [],
                null,
                {},
                emptyComments,
              ), // Whatever, close enough
            ],
          ),
        ],
        true,
      ),
    ])
    const actualResult = MetadataUtils.targetElementSupportsChildren(
      {},
      path,
      { [EP.toString(path)]: element },
      {},
    )
    expect(actualResult).toEqual(true)
  })
  it('returns true for an unparsed span', () => {
    const path = EP.elementPath([
      [BakedInStoryboardUID, TestScenePath],
      ['Dummy', 'Element'],
    ])
    const element = dummyInstanceDataForElementType('span', path)
    const actualResult = MetadataUtils.targetElementSupportsChildren(
      {},
      path,
      { [EP.toString(path)]: element },
      {},
    )
    expect(actualResult).toEqual(true)
  })
  it('returns true for a parsed span', () => {
    const path = EP.elementPath([
      [BakedInStoryboardUID, TestScenePath],
      ['Dummy', 'Element'],
    ])
    const element = dummyInstanceDataForElementType(jsxElementName('span', []), path)
    const actualResult = MetadataUtils.targetElementSupportsChildren(
      {},
      path,
      { [EP.toString(path)]: element },
      {},
    )
    expect(actualResult).toEqual(true)
  })
  it('returns true for an animated.div', () => {
    const path = EP.elementPath([
      [BakedInStoryboardUID, TestScenePath],
      ['Dummy', 'Element'],
    ])
    const element = dummyInstanceDataForElementType(jsxElementName('animated', ['div']), path)
    const actualResult = MetadataUtils.targetElementSupportsChildren(
      {},
      path,
      { [EP.toString(path)]: element },
      {},
    )
    expect(actualResult).toEqual(true)
  })
  it('returns false for an unparsed img', () => {
    const path = EP.elementPath([
      [BakedInStoryboardUID, TestScenePath],
      ['Dummy', 'Element'],
    ])
    const element = dummyInstanceDataForElementType('img', path)
    const actualResult = MetadataUtils.targetElementSupportsChildren(
      {},
      path,
      { [EP.toString(path)]: element },
      {},
    )
    expect(actualResult).toEqual(false)
  })
  it('returns false for a parsed img', () => {
    const path = EP.elementPath([
      [BakedInStoryboardUID, TestScenePath],
      ['Dummy', 'Element'],
    ])
    const element = dummyInstanceDataForElementType(jsxElementName('img', []), path)
    const actualResult = MetadataUtils.targetElementSupportsChildren(
      {},
      path,
      { [EP.toString(path)]: element },
      {},
    )
    expect(actualResult).toEqual(false)
  })
  it('returns true for a component used from a different file that uses props.children', () => {
    const storyboardCode = `import React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
export var storyboard = (
  <Storyboard data-uid='${BakedInStoryboardUID}'>
    <Scene
      data-uid='${TestSceneUID}'
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='${TestAppUID}' />
    </Scene>
  </Storyboard>
`
    const storyboardJS = parseResultFromCode(StoryboardFilePath, storyboardCode)
    const appCode = `import React from 'react'
export const App = (props) => {
  return <div>{props.children}</div>
}
`
    const appJS = parseResultFromCode('/src/app.js', appCode)
    const projectContents = contentsToTree({
      [StoryboardFilePath]: textFile(
        textFileContents(storyboardCode, storyboardJS, RevisionsState.BothMatch),
        null,
        null,
        0,
      ),
      ['/src/app.js']: textFile(
        textFileContents(appCode, appJS, RevisionsState.BothMatch),
        null,
        null,
        0,
      ),
    })
    const path = EP.elementPath([[BakedInStoryboardUID, TestScenePath, TestAppUID]])
    const element = dummyInstanceDataForElementType(jsxElementName('App', []), path)
    const actualResult = MetadataUtils.targetElementSupportsChildren(
      projectContents,
      path,
      { [EP.toString(path)]: element },
      {},
    )
    expect(actualResult).toEqual(true)
  })
  it('returns false for a component used from a different file that uses props.children but has only text children', () => {
    const storyboardCode = `import React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
export var storyboard = (
  <Storyboard data-uid='${BakedInStoryboardUID}'>
    <Scene
      data-uid='${TestSceneUID}'
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='${TestAppUID}'>
        Some Dummy Text
      </App>
    </Scene>
  </Storyboard>
`
    const storyboardJS = parseResultFromCode(StoryboardFilePath, storyboardCode)
    const appCode = `import React from 'react'
export const App = (props) => {
  return <div>{props.children}</div>
}
`
    const appJS = parseResultFromCode('/src/app.js', appCode)
    const projectContents = contentsToTree({
      [StoryboardFilePath]: textFile(
        textFileContents(storyboardCode, storyboardJS, RevisionsState.BothMatch),
        null,
        null,
        0,
      ),
      ['/src/app.js']: textFile(
        textFileContents(appCode, appJS, RevisionsState.BothMatch),
        null,
        null,
        0,
      ),
    })

    // Painfully pull out the parsed children so that we can add them into the dummy metadata
    expect(isParseSuccess(storyboardJS)).toBeTruthy()
    const parsedStoryboard = storyboardJS as ParseSuccess
    const parsedElement = findJSXElementAtStaticPath(
      getUtopiaJSXComponentsFromSuccess(parsedStoryboard),
      EP.elementPath([EP.staticElementPath([BakedInStoryboardUID, TestSceneUID, TestAppUID])]),
    )
    expect(parsedElement).toBeDefined()
    expect(isJSXElement(parsedElement!)).toBeTruthy()
    const parsedChildren = (parsedElement! as JSXElement).children

    const path = EP.elementPath([[BakedInStoryboardUID, TestScenePath, TestAppUID]])
    const element = dummyInstanceDataForElementType(jsxElementName('App', []), path, parsedChildren)
    const actualResult = MetadataUtils.targetElementSupportsChildren(
      projectContents,
      path,
      {
        [EP.toString(path)]: element,
      },
      {},
    )
    expect(actualResult).toEqual(false)
  })
  it('returns false for a component used from a different file that does not use props.children', () => {
    const storyboardCode = `import React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
export var storyboard = (
  <Storyboard data-uid='${BakedInStoryboardUID}'>
    <Scene
      data-uid='${TestSceneUID}'
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='${TestAppUID}' />
    </Scene>
  </Storyboard>
`
    const storyboardJS = parseResultFromCode(StoryboardFilePath, storyboardCode)
    const appCode = `import React from 'react'
export const App = (props) => {
  return <div>A REALLY NICE BANNER</div>
}
`
    const appJS = parseResultFromCode('/src/app.js', appCode)
    const projectContents = contentsToTree({
      [StoryboardFilePath]: textFile(
        textFileContents(storyboardCode, storyboardJS, RevisionsState.BothMatch),
        null,
        null,
        0,
      ),
      ['/src/app.js']: textFile(
        textFileContents(appCode, appJS, RevisionsState.BothMatch),
        null,
        null,
        0,
      ),
    })
    const path = EP.elementPath([[BakedInStoryboardUID, TestScenePath, TestAppUID]])
    const element = dummyInstanceDataForElementType(
      jsxElementName('App', []),
      path,
      [],
      importedOrigin('/src/app.js', 'App', 'App'),
    )

    const actualResult = MetadataUtils.targetElementSupportsChildren(
      projectContents,
      path,
      {
        [EP.toString(path)]: element,
      },
      {},
    )
    expect(actualResult).toEqual(false)
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
      MetadataUtils.isLayoutedByFlowAndNotAbsolutePositioned(
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
      MetadataUtils.isLayoutedByFlowAndNotAbsolutePositioned(
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
      MetadataUtils.isLayoutedByFlowAndNotAbsolutePositioned(
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
      MetadataUtils.isLayoutedByFlowAndNotAbsolutePositioned(
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
    jsxAttributesFromMap({ 'data-uid': jsExpressionValue('span-1', emptyComments) }),
    [textBlock],
  )
  const spanElementMetadata = elementInstanceMetadata(
    spanPath,
    right(spanElement),
    zeroRectangle as CanvasRectangle,
    zeroRectangle as LocalRectangle,
    zeroRectangle as CanvasRectangle,
    false,
    false,
    emptySpecialSizeMeasurements,
    emptyComputedStyle,
    emptyAttributeMetadata,
    null,
    null,
    'not-a-conditional',
    null,
    null,
    null,
  )
  const spanElementProps: ElementProps = {
    'data-uid': 'span-1',
  }
  const divElement = jsxElement(
    'div',
    'div-1',
    jsxAttributesFromMap({ 'data-uid': jsExpressionValue('div-1', emptyComments) }),
    [spanElement],
  )
  const divElementMetadata = elementInstanceMetadata(
    divPath,
    right(divElement),
    zeroRectangle as CanvasRectangle,
    zeroRectangle as LocalRectangle,
    zeroRectangle as CanvasRectangle,
    false,
    false,
    emptySpecialSizeMeasurements,
    emptyComputedStyle,
    emptyAttributeMetadata,
    null,
    null,
    'not-a-conditional',
    null,
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
  const pathTrees: ElementPathTrees = {
    [EP.toString(spanElementMetadata.elementPath)]: elementPathTree(
      spanElementMetadata.elementPath,
      EP.toString(spanElementMetadata.elementPath),
      [],
    ),
    [EP.toString(divElementMetadata.elementPath)]: elementPathTree(
      divElementMetadata.elementPath,
      EP.toString(divElementMetadata.elementPath),
      [
        elementPathTree(
          spanElementMetadata.elementPath,
          EP.toString(spanElementMetadata.elementPath),
          [],
        ),
      ],
    ),
  }
  it('the label of a spin containing text is that text', () => {
    const actualResult = MetadataUtils.getElementLabel(
      allElementProps,
      spanPath,
      pathTrees,
      metadata,
    )
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
  it('getAllStoryboardChildrenPaths returns paths of all children of the storyboard', () => {
    const testComponentSceneTree = elementPathTree(
      testComponentSceneElement.elementPath,
      EP.toString(testComponentSceneElement.elementPath),
      [],
    )
    const testStoryboardChildTree = elementPathTree(
      testStoryboardChildElement.elementPath,
      EP.toString(testStoryboardChildElement.elementPath),
      [],
    )
    const storyboardTree = elementPathTree(
      EP.elementPath([[BakedInStoryboardUID]]),
      EP.toString(EP.elementPath([[BakedInStoryboardUID]])),
      [testComponentSceneTree, testStoryboardChildTree],
    )
    const pathTrees: ElementPathTrees = {
      [EP.toString(EP.elementPath([[BakedInStoryboardUID]]))]: storyboardTree,
      [EP.toString(testComponentSceneElement.elementPath)]: testComponentSceneTree,
      [EP.toString(testStoryboardChildElement.elementPath)]: testStoryboardChildTree,
    }
    const actualResult = MetadataUtils.getAllStoryboardChildrenPathsOrdered(
      testJsxMetadata,
      pathTrees,
    )
    const expectedResult: Array<ElementPath> = [
      testComponentSceneElement.elementPath,
      testStoryboardChildElement.elementPath,
    ]
    expect(actualResult).toEqual(expectedResult)
  })

  it('getAllCanvasSelectablePathsUnordered returns paths of the top level children of the storyboard, replacing scenes with their root views', () => {
    const testComponentSceneChildTree = elementPathTree(
      testComponentSceneChildElement.elementPath,
      EP.toString(testComponentSceneChildElement.elementPath),
      [],
    )
    const testComponentChild1Tree = elementPathTree(
      testComponentMetadataChild1.elementPath,
      EP.toString(testComponentMetadataChild1.elementPath),
      [],
    )
    const testComponentChild2Tree = elementPathTree(
      testComponentMetadataChild2.elementPath,
      EP.toString(testComponentMetadataChild2.elementPath),
      [],
    )
    const testComponentChild3Tree = elementPathTree(
      testComponentMetadataChild3.elementPath,
      EP.toString(testComponentMetadataChild3.elementPath),
      [],
    )
    const testComponentRoot1Tree = elementPathTree(
      testComponentRoot1.elementPath,
      EP.toString(testComponentRoot1.elementPath),
      [testComponentChild1Tree, testComponentChild2Tree, testComponentChild3Tree],
    )
    const testComponentSceneTree = elementPathTree(
      testComponentSceneElement.elementPath,
      EP.toString(testComponentSceneElement.elementPath),
      [testComponentSceneChildTree, testComponentRoot1Tree],
    )
    const testStoryboardChildTree = elementPathTree(
      testStoryboardChildElement.elementPath,
      EP.toString(testStoryboardChildElement.elementPath),
      [],
    )
    const storyboardTree = elementPathTree(
      EP.elementPath([[BakedInStoryboardUID]]),
      EP.toString(EP.elementPath([[BakedInStoryboardUID]])),
      [testComponentSceneChildTree, testStoryboardChildTree],
    )
    const pathTrees: ElementPathTrees = {
      [EP.toString(EP.elementPath([[BakedInStoryboardUID]]))]: storyboardTree,
      [EP.toString(testComponentSceneChildElement.elementPath)]: testComponentSceneChildTree,
      [EP.toString(testStoryboardChildElement.elementPath)]: testStoryboardChildTree,
      [EP.toString(testComponentSceneElement.elementPath)]: testComponentSceneTree,
      [EP.toString(testComponentRoot1.elementPath)]: testComponentRoot1Tree,
      [EP.toString(testComponentMetadataChild1.elementPath)]: testComponentChild1Tree,
      [EP.toString(testComponentMetadataChild2.elementPath)]: testComponentChild2Tree,
      [EP.toString(testComponentMetadataChild3.elementPath)]: testComponentChild3Tree,
    }
    const actualResult = MetadataUtils.getAllCanvasSelectablePathsOrdered(
      testJsxMetadata,
      pathTrees,
    )
    const expectedResult: Array<ElementPath> = [
      testComponentRoot1.elementPath,
      testComponentSceneChildElement.elementPath,
      testStoryboardChildElement.elementPath,
    ]
    expect(actualResult).toEqual(expectedResult)
  })

  it('getAllPaths returns the instance paths in a depth first manner', () => {
    const actualResult = MetadataUtils.getAllPaths(
      testJsxMetadata,
      MetadataUtils.createElementPathTreeFromMetadata(testJsxMetadata),
    )
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
    const actualResult = MetadataUtils.createOrderedElementPathsFromElements(
      testJsxMetadata,
      MetadataUtils.createElementPathTreeFromMetadata(testJsxMetadata),
      [],
      [],
    )

    expect(actualResult.navigatorTargets).toEqual(expectedNavigatorTargets)
    expect(actualResult.visibleNavigatorTargets).toEqual(expectedNavigatorTargets)
  })

  it('with the scene collapsed', () => {
    const actualResult = MetadataUtils.createOrderedElementPathsFromElements(
      testJsxMetadata,
      MetadataUtils.createElementPathTreeFromMetadata(testJsxMetadata),
      [testComponentSceneElement.elementPath],
      [],
    )

    expect(actualResult.navigatorTargets).toEqual(expectedNavigatorTargets)
    expect(actualResult.visibleNavigatorTargets).toEqual([
      testComponentSceneElement.elementPath,
      testStoryboardChildElement.elementPath,
      testStoryboardGrandChildElement.elementPath,
    ])
  })

  it('with collapsed roots', () => {
    const actualResult = MetadataUtils.createOrderedElementPathsFromElements(
      testJsxMetadata,
      MetadataUtils.createElementPathTreeFromMetadata(testJsxMetadata),
      [testComponentRoot1.elementPath, testComponentSceneChildElement.elementPath],
      [],
    )

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
