import * as FastCheck from 'fast-check'
import { getSourceMapConsumer } from '../../../third-party/react-error-overlay/utils/getSourceMap'
import {
  arbitraryJSBlock,
  isArbitraryJSBlock,
  isJSXAttributeOtherJavaScript,
  isJSXElement,
  isUtopiaJSXComponent,
  jsxArbitraryBlock,
  jsxArraySpread,
  jsxArrayValue,
  jsxAttributeFunctionCall,
  jsxAttributeNestedArray,
  jsxAttributeNestedArraySimple,
  jsxAttributeNestedObject,
  jsxAttributeNestedObjectSimple,
  jsxAttributeOtherJavaScript,
  JSXAttributes,
  jsxAttributeValue,
  jsxElement,
  jsxPropertyAssignment,
  jsxSpreadAssignment,
  jsxTextBlock,
  utopiaJSXComponent,
  UtopiaJSXComponent,
  defaultPropsParam,
  clearArbitraryJSBlockUniqueIDs,
  jsxAttributesFromMap,
  getJSXAttributeForced,
  isJSXAttributesEntry,
} from '../../shared/element-template'
import { sampleCode } from '../../model/new-project-files'
import { addImport, emptyImports, parseSuccess } from '../common/project-file-utils'
import { onlyImportReact, sampleImportsForTests } from '../../model/test-ui-js-file.test-utils'
import {
  isParseSuccess,
  importAlias,
  foldParsedTextFile,
  exportsDetail,
  addNamedExportToDetail,
  EmptyExportsDetail,
  setNamedDefaultExportInDetail,
  addModifierExportToDetail,
  setModifierDefaultExportInDetail,
} from '../../shared/project-file-types'
import {
  lintAndParse,
  parseCode,
  printCode,
  printCodeOptions,
  getHighlightBoundsWithoutUID,
} from './parser-printer'
import { applyPrettier } from 'utopia-vscode-common'
import { transpileJavascriptFromCode } from './parser-printer-transpiling'
import {
  clearParseResultPassTimes,
  clearParseResultUniqueIDsAndEmptyBlocks,
  clearTopLevelElementUniqueIDsAndEmptyBlocks,
  elementsStructure,
  ensureElementsHaveUID,
  JustImportViewAndReact,
  PrintableProjectContent,
  printableProjectContentArbitrary,
  testParseCode,
} from './parser-printer.test-utils'
import { InfiniteLoopError, InfiniteLoopMaxIterations } from './transform-prevent-infinite-loops'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../model/scene-utils'
import { emptyComments } from './parser-printer-comments'
import { optionalMap } from '../../shared/optional-utils'
import { StoryboardFilePath } from '../../../components/editor/store/editor-state'
import { JSX_CANVAS_LOOKUP_FUNCTION_NAME } from './parser-printer-utils'
import { emptySet } from '../../shared/set-utils'

describe('JSX parser', () => {
  it('parses the code when it is a var', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import { cake } from 'cake'
export var whatever = (props) => <View data-uid='aaa'>
  <cake data-uid='aab' style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
        {},
      ),
      right: jsxAttributeValue(20, emptyComments),
      top: jsxAttributeValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'expression',
      defaultPropsParam,
      ['leftOfTheCake'],
      view,
      null,
      false,
      emptyComments,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it is a var no params', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import { cake } from 'cake'
export var whatever = () => <View data-uid='aaa'>
  <cake data-uid='aab' style={{backgroundColor: 'red'}} left={20} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      left: jsxAttributeValue(20, emptyComments),
      right: jsxAttributeValue(20, emptyComments),
      top: jsxAttributeValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'expression',
      null,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it is a function', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import { cake } from 'cake'
export function whatever(props) {
  return (
    <View data-uid='aaa'>
      <cake data-uid='aab' style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
    </View>
  )
}
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
        {},
      ),
      right: jsxAttributeValue(20, emptyComments),
      top: jsxAttributeValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'function',
      'block',
      defaultPropsParam,
      ['leftOfTheCake'],
      view,
      null,
      false,
      emptyComments,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it is a function without any params', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import { cake } from 'cake'
export function whatever() {
  return (
    <View data-uid='aaa'>
      <cake data-uid='aab' style={{backgroundColor: 'red'}} left={20} right={20} top={-20} />
    </View>
  )
}
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      left: jsxAttributeValue(20, emptyComments),
      right: jsxAttributeValue(20, emptyComments),
      top: jsxAttributeValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'function',
      'block',
      null,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it is an export default function', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import { cake } from 'cake'
export default function whatever(props) {
  return (
    <View data-uid='aaa'>
      <cake data-uid='aab' style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
    </View>
  )
}
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
        {},
      ),
      right: jsxAttributeValue(20, emptyComments),
      top: jsxAttributeValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'function',
      'block',
      defaultPropsParam,
      ['leftOfTheCake'],
      view,
      null,
      false,
      emptyComments,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      setModifierDefaultExportInDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it is an export default function with no params', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import { cake } from 'cake'
export default function whatever() {
  return (
    <View data-uid='aaa'>
      <cake data-uid='aab' style={{backgroundColor: 'red'}} left={20} right={20} top={-20} />
    </View>
  )
}
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      left: jsxAttributeValue(20, emptyComments),
      right: jsxAttributeValue(20, emptyComments),
      top: jsxAttributeValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'function',
      'block',
      null,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      setModifierDefaultExportInDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it includes a default import', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import cake from 'cake'
import './style.css'
export var whatever = (props) => <View data-uid='aaa'>
  <cake data-uid='aab' style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
        {},
      ),
      right: jsxAttributeValue(20, emptyComments),
      top: jsxAttributeValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'expression',
      defaultPropsParam,
      ['leftOfTheCake'],
      view,
      null,
      false,
      emptyComments,
    )
    const importsWithCake = addImport('/code.js', 'cake', 'cake', [], null, sampleImportsForTests)
    const importsWithStylecss = addImport(
      '/code.js',
      './style.css',
      null,
      [],
      null,
      importsWithCake,
    )
    const expectedResult = parseSuccess(
      importsWithStylecss,
      expect.arrayContaining([exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it includes a mixed import', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import cake, { cake2 } from 'cake'
export var whatever = (props) => <View data-uid='aaa'>
  <cake data-uid='aab' style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
  <cake2 data-uid='aac' style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
        {},
      ),
      right: jsxAttributeValue(20, emptyComments),
      top: jsxAttributeValue(-20, emptyComments),
    })
    const cake2Attributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aac', emptyComments),
      style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
        {},
      ),
      right: jsxAttributeValue(20, emptyComments),
      top: jsxAttributeValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const cake2 = jsxElement('cake2', 'aac', cake2Attributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake, cake2],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'expression',
      defaultPropsParam,
      ['leftOfTheCake'],
      view,
      null,
      false,
      emptyComments,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      'cake',
      [importAlias('cake2')],
      null,
      sampleImportsForTests,
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  }),
    it('parses the code when it includes an import with alias', () => {
      const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import { cake as cake2 } from 'cake'
export var whatever = (props) => <View data-uid='aaa'>
  <cake2 data-uid='aac' style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
</View>
`
      const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
      const cake2Attributes: JSXAttributes = jsxAttributesFromMap({
        'data-uid': jsxAttributeValue('aac', emptyComments),
        style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
        left: jsxAttributeOtherJavaScript(
          'props.leftOfTheCake[0].hat',
          'return props.leftOfTheCake[0].hat;',
          ['props'],
          expect.objectContaining({}),
          {},
        ),
        right: jsxAttributeValue(20, emptyComments),
        top: jsxAttributeValue(-20, emptyComments),
      })
      const cake2 = jsxElement('cake2', 'aac', cake2Attributes, [])
      const view = jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
        [cake2],
      )
      const exported = utopiaJSXComponent(
        'whatever',
        true,
        'var',
        'expression',
        defaultPropsParam,
        ['leftOfTheCake'],
        view,
        null,
        false,
        emptyComments,
      )
      const imports = addImport(
        '/code.js',
        'cake',
        null,
        [importAlias('cake', 'cake2')],
        null,
        sampleImportsForTests,
      )
      const expectedResult = parseSuccess(
        imports,
        expect.arrayContaining([exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      )
      expect(actualResult).toEqual(expectedResult)
    }),
    it('parses the code when it is a function, with metadata', () => {
      const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import { cake } from 'cake'
export var whatever = (props) => <View data-uid='aaa'>
  <cake data-uid='aab' data-label='First cake' style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
  <cake data-uid='111' data-label='Second cake' style={{backgroundColor: 'blue'}} left={props.rightOfTheCake[0].hat} right={10} top={-10} />
</View>
`
      const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
      const firstCakeAttributes: JSXAttributes = jsxAttributesFromMap({
        'data-uid': jsxAttributeValue('aab', emptyComments),
        'data-label': jsxAttributeValue('First cake', emptyComments),
        style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
        left: jsxAttributeOtherJavaScript(
          'props.leftOfTheCake[0].hat',
          'return props.leftOfTheCake[0].hat;',
          ['props'],
          expect.objectContaining({}),
          {},
        ),
        right: jsxAttributeValue(20, emptyComments),
        top: jsxAttributeValue(-20, emptyComments),
      })
      const firstCake = jsxElement('cake', 'aab', firstCakeAttributes, [])
      const secondCakeAttributes: JSXAttributes = jsxAttributesFromMap({
        'data-uid': jsxAttributeValue('111', emptyComments),
        'data-label': jsxAttributeValue('Second cake', emptyComments),
        style: jsxAttributeValue({ backgroundColor: 'blue' }, emptyComments),
        left: jsxAttributeOtherJavaScript(
          'props.rightOfTheCake[0].hat',
          'return props.rightOfTheCake[0].hat;',
          ['props'],
          expect.objectContaining({}),
          {},
        ),
        right: jsxAttributeValue(10, emptyComments),
        top: jsxAttributeValue(-10, emptyComments),
      })
      const secondCake = jsxElement('cake', '111', secondCakeAttributes, [])
      const view = jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
        [firstCake, secondCake],
      )
      const exported = utopiaJSXComponent(
        'whatever',
        true,
        'var',
        'expression',
        defaultPropsParam,
        ['leftOfTheCake', 'rightOfTheCake'],
        view,
        null,
        false,
        emptyComments,
      )
      const imports = addImport(
        '/code.js',
        'cake',
        null,
        [importAlias('cake')],
        null,
        sampleImportsForTests,
      )
      const expectedResult = parseSuccess(
        imports,
        expect.arrayContaining([exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      )
      expect(actualResult).toEqual(expectedResult)
    })
  it('parses the code when it is a function, with undefined and null as props', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import { cake } from 'cake'
export var whatever = (props) => <View data-uid='aaa'>
  <cake data-uid='aab' style={{backgroundColor: 'red' }} left={props.leftOfTheCake[0].hat} right={20} top={-20} nullProp={null} undefinedProp={undefined} trueProp={true} falseProp={false} />
</View>
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      style: jsxAttributeValue(
        {
          backgroundColor: 'red',
        },
        emptyComments,
      ),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
        {},
      ),
      right: jsxAttributeValue(20, emptyComments),
      top: jsxAttributeValue(-20, emptyComments),
      nullProp: jsxAttributeValue(null, emptyComments),
      undefinedProp: jsxAttributeValue(undefined, emptyComments),
      trueProp: jsxAttributeValue(true, emptyComments),
      falseProp: jsxAttributeValue(false, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'expression',
      defaultPropsParam,
      ['leftOfTheCake'],
      view,
      null,
      false,
      emptyComments,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it is a function, with some arbitrary JavaScript', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import { cake } from 'cake'
function getSizing(n) {
  return 100 + n
}
var spacing = 20
export var whatever = (props) => <View data-uid='aaa'>
  <cake data-uid='aab' style={{backgroundColor: 'red'}} left={getSizing(spacing)} right={20} top={-20} onClick={function(){console.log('click')}} />
</View>
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      left: jsxAttributeOtherJavaScript(
        'getSizing(spacing)',
        'return getSizing(spacing);',
        ['getSizing', 'spacing'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
      ),
      right: jsxAttributeValue(20, emptyComments),
      top: jsxAttributeValue(-20, emptyComments),
      onClick: jsxAttributeOtherJavaScript(
        `function(){console.log('click')}`,
        `return (function () {
  console.log('click');
});`,
        ['console'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
      ),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'expression',
      defaultPropsParam,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const jsCode1 = `function getSizing(n) {
  return 100 + n
}`
    const transpiledJsCode1 = `function getSizing(n) {
  return 100 + n;
}
return { getSizing: getSizing };`
    const arbitraryBlock1 = arbitraryJSBlock(
      jsCode1,
      transpiledJsCode1,
      ['getSizing'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const jsCode2 = `var spacing = 20`
    const transpiledJsCode2 = `var spacing = 20;
return { spacing: spacing };`
    const arbitraryBlock2 = arbitraryJSBlock(
      jsCode2,
      transpiledJsCode2,
      ['spacing'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const combinedJsCode = `function getSizing(n) {
  return 100 + n
}
var spacing = 20`
    const transpiledcombinedJsCode = `function getSizing(n) {
  return 100 + n;
}

var spacing = 20;
return { getSizing: getSizing, spacing: spacing };`
    const combinedArbitraryBlock = arbitraryJSBlock(
      combinedJsCode,
      transpiledcombinedJsCode,
      ['getSizing', 'spacing'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const topLevelElements = [arbitraryBlock1, arbitraryBlock2, exported].map(
      clearTopLevelElementUniqueIDsAndEmptyBlocks,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(combinedArbitraryBlock),
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it has an export default function', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import { cake } from 'cake'
export default function getSizing(n) {
  return 100 + n
}
export var whatever = (props) => <View data-uid='aaa'>
  <cake data-uid='aab' style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
        {},
      ),
      right: jsxAttributeValue(20, emptyComments),
      top: jsxAttributeValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'expression',
      defaultPropsParam,
      ['leftOfTheCake'],
      view,
      null,
      false,
      emptyComments,
    )
    const jsCode = `export default function getSizing(n) {
  return 100 + n
}`
    const transpiledJsCode = `function getSizing(n) {
  return 100 + n;
}
return { getSizing: getSizing };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['getSizing'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const topLevelElements = [arbitraryBlock, exported].map(
      clearTopLevelElementUniqueIDsAndEmptyBlocks,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(arbitraryBlock),
      setModifierDefaultExportInDetail(
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
        'getSizing',
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it has a default keyword elsewhere', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import { cake } from 'cake'
export function getSizing(n) {
  switch (n) {
    case 100:
      return 1
    default:
      return 100 + n
  }
}
export var whatever = (props) => <View data-uid='aaa'>
  <cake data-uid='aab' style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
        {},
      ),
      right: jsxAttributeValue(20, emptyComments),
      top: jsxAttributeValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'expression',
      defaultPropsParam,
      ['leftOfTheCake'],
      view,
      null,
      false,
      emptyComments,
    )
    const jsCode = `export function getSizing(n) {
  switch (n) {
    case 100:
      return 1
    default:
      return 100 + n
  }
}`
    const transpiledJsCode = `function getSizing(n) {
  switch (n) {
    case 100:
      return 1;

    default:
      return 100 + n;
  }
}
return { getSizing: getSizing };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['getSizing'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const topLevelElements = [arbitraryBlock, exported].map(
      clearTopLevelElementUniqueIDsAndEmptyBlocks,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(arbitraryBlock),
      addModifierExportToDetail(
        addModifierExportToDetail(EmptyExportsDetail, 'getSizing'),
        'whatever',
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it has an export default anonymous function', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import { cake } from 'cake'
export default (n) => {
  return 100 + n
}
export var whatever = (props) => <View data-uid='aaa'>
  <cake data-uid='aab' style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
        {},
      ),
      right: jsxAttributeValue(20, emptyComments),
      top: jsxAttributeValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'expression',
      defaultPropsParam,
      ['leftOfTheCake'],
      view,
      null,
      false,
      emptyComments,
    )
    const jsCode = `export default (n) => {
  return 100 + n
}`
    const transpiledJsCode = `(function (n) {
  return 100 + n;
});
return {  };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      [],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const topLevelElements = [arbitraryBlock, exported].map(
      clearTopLevelElementUniqueIDsAndEmptyBlocks,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(arbitraryBlock),
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it is a function, with some arbitrary JavaScript (variable declaration)', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import { cake } from 'cake'
var spacing = 20
export var whatever = (props) => <View data-uid='aaa'>
  <cake data-uid='aab' style={{backgroundColor: 'red'}} left={spacing} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      left: jsxAttributeOtherJavaScript(
        'spacing',
        'return spacing;',
        ['spacing'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
      ),
      right: jsxAttributeValue(20, emptyComments),
      top: jsxAttributeValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'expression',
      defaultPropsParam,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const transpiledJSCode = `var spacing = 20;
return { spacing: spacing };`
    const jsVariable = arbitraryJSBlock(
      'var spacing = 20',
      transpiledJSCode,
      ['spacing'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const topLevelElements = [jsVariable, exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(jsVariable),
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, with some arbitrary JavaScript (array literals + element access)', () => {
    const code = `
import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  const bgs = ['black', 'grey']
  const bg = bgs[0]
  return (
    <View data-uid='aaa' style={{ backgroundColor: bgs[0] }} />
  )
}
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const viewAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aaa', emptyComments),
      style: jsxAttributeNestedObjectSimple(
        jsxAttributesFromMap({
          backgroundColor: jsxAttributeOtherJavaScript(
            'bgs[0]',
            'return bgs[0];',
            ['bgs'],
            expect.objectContaining({
              sources: ['code.tsx'],
              version: 3,
              file: 'code.tsx',
            }),
            {},
          ),
        }),
        emptyComments,
      ),
    })
    const view = jsxElement('View', 'aaa', viewAttributes, [])
    const jsCode = `const bgs = ['black', 'grey']
  const bg = bgs[0]`
    const transpiledJsCode = `var bgs = ['black', 'grey'];
var bg = bgs[0];
return { bgs: bgs, bg: bg };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['bgs', 'bg'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
      false,
      emptyComments,
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, with some arbitrary JavaScript (array spread)', () => {
    const code = `
import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  const greys = ['lightGrey', 'grey']
  return (
    <View data-uid='aaa' colors={['black', ...greys]}/>
  )
}
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const viewAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aaa', emptyComments),
      colors: jsxAttributeNestedArray(
        [
          jsxArrayValue(jsxAttributeValue('black', emptyComments), emptyComments),
          jsxArraySpread(
            jsxAttributeOtherJavaScript(
              'greys',
              'return greys;',
              ['greys'],
              expect.objectContaining({
                sources: ['code.tsx'],
                version: 3,
                file: 'code.tsx',
              }),
              {},
            ),
            emptyComments,
          ),
        ],
        emptyComments,
      ),
    })
    const view = jsxElement('View', 'aaa', viewAttributes, [])
    const jsCode = `const greys = ['lightGrey', 'grey']`
    const transpiledJsCode = `var greys = ['lightGrey', 'grey'];
return { greys: greys };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['greys'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
      false,
      emptyComments,
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, with some arbitrary JavaScript (binary expressions)', () => {
    const code = `
import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  const a = 10
  const b = 20
  return (
    <View data-uid='aaa' left={a + b} />
  )
}
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const viewAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aaa', emptyComments),
      left: jsxAttributeOtherJavaScript(
        'a + b',
        'return a + b;',
        ['a', 'b'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
      ),
    })
    const view = jsxElement('View', 'aaa', viewAttributes, [])
    const jsCode = `const a = 10
  const b = 20`
    const transpiledJsCode = `var a = 10;
var b = 20;
return { a: a, b: b };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['a', 'b'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
      false,
      emptyComments,
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, with some arbitrary JavaScript (conditional expressions)', () => {
    const code = `
import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  const a = true
  const b = 10
  const c = 20
  return (
    <View data-uid='aaa' left={a ? b : c} />
  )
}
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const viewAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aaa', emptyComments),
      left: jsxAttributeOtherJavaScript(
        'a ? b : c',
        'return a ? b : c;',
        ['a', 'b', 'c'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
      ),
    })
    const view = jsxElement('View', 'aaa', viewAttributes, [])
    const jsCode = `const a = true
  const b = 10
  const c = 20`
    const transpiledJsCode = `var a = true;
var b = 10;
var c = 20;
return { a: a, b: b, c: c };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['a', 'b', 'c'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
      false,
      emptyComments,
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, with some arbitrary JavaScript (postfix and prefix)', () => {
    const code = `
import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  let a = 10
  return (
    <View data-uid='aaa' left={a++} right={++a} />
  )
}
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const viewAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aaa', emptyComments),
      left: jsxAttributeOtherJavaScript(
        'a++',
        'return a++;',
        ['a'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
      ),
      right: jsxAttributeOtherJavaScript(
        '++a',
        'return ++a;',
        ['a'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
      ),
    })
    const view = jsxElement('View', 'aaa', viewAttributes, [])
    const jsCode = `let a = 10`
    const transpiledJsCode = `var a = 10;
return { a: a };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['a'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
      false,
      emptyComments,
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, with some arbitrary JavaScript (object literals)', () => {
    const code = `
import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  const a = 10
  const b = { a: a }
  return (
    <View data-uid='aaa' left={b.a} />
  )
}
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const viewAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aaa', emptyComments),
      left: jsxAttributeOtherJavaScript(
        'b.a',
        'return b.a;',
        ['b'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
      ),
    })
    const view = jsxElement('View', 'aaa', viewAttributes, [])
    const jsCode = `const a = 10
  const b = { a: a }`
    const transpiledJsCode = `var a = 10;
var b = {
  a: a
};
return { a: a, b: b };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['a', 'b'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
      false,
      emptyComments,
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, with some arbitrary JavaScript (object spread)', () => {
    const code = `
import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  const bg = { backgroundColor: 'grey' }
  return (
    <View data-uid='aaa' style={{...bg}} />
  )
}
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const viewAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aaa', emptyComments),
      style: jsxAttributeNestedObject(
        [
          jsxSpreadAssignment(
            jsxAttributeOtherJavaScript(
              'bg',
              'return bg;',
              ['bg'],
              expect.objectContaining({
                sources: ['code.tsx'],
                version: 3,
                file: 'code.tsx',
              }),
              {},
            ),
            emptyComments,
          ),
        ],
        emptyComments,
      ),
    })
    const view = jsxElement('View', 'aaa', viewAttributes, [])
    const jsCode = `const bg = { backgroundColor: 'grey' }`
    const transpiledJsCode = `var bg = {
  backgroundColor: 'grey'
};
return { bg: bg };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['bg'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
      false,
      emptyComments,
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, with some arbitrary JavaScript (string interpolation)', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import { cake } from 'cake'
var count = 10
export var whatever = (props) => <View data-uid='aaa'>
  <cake data-uid='aab' style={{backgroundColor: 'red'}} text={\`Count \${count}\`} />
</View>
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      text: jsxAttributeOtherJavaScript(
        '`Count ${count}`',
        'return "Count ".concat(count);',
        ['count'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
      ),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'expression',
      defaultPropsParam,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const jsCode = `var count = 10`
    const transpiledJSCode = `var count = 10;
return { count: count };`
    const jsVariable = arbitraryJSBlock(
      jsCode,
      transpiledJSCode,
      ['count'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const topLevelElements = [jsVariable, exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(jsVariable),
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, with a ternary referencing arbitrary JS', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import { cake } from 'cake'
var use20 = true
export var whatever = (props) => <View data-uid='aaa'>
  <cake data-uid='aab' style={{backgroundColor: 'red'}} left={use20 ? 20 : 10} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      left: jsxAttributeOtherJavaScript(
        'use20 ? 20 : 10',
        'return use20 ? 20 : 10;',
        ['use20'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
      ),
      right: jsxAttributeValue(20, emptyComments),
      top: jsxAttributeValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'expression',
      defaultPropsParam,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const transpiledJSCode = `var use20 = true;
return { use20: use20 };`
    const jsVariable = arbitraryJSBlock(
      'var use20 = true',
      transpiledJSCode,
      ['use20'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const topLevelElements = [jsVariable, exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(jsVariable),
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, with arbitrary JS with Set constructor', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
var mySet = new Set()
export var whatever = (props) => <View data-uid='aaa'>
</View>
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'expression',
      defaultPropsParam,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const transpiledJSCode = `var mySet = new Set();
return { mySet: mySet };`
    const jsVariable = arbitraryJSBlock(
      'var mySet = new Set()',
      transpiledJSCode,
      ['mySet'],
      ['Set', JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const topLevelElements = [jsVariable, exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)
    const expectedResult = parseSuccess(
      sampleImportsForTests,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(jsVariable),
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, with some arbitrary JavaScript combined with props', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import { cake } from 'cake'
var spacing = 20
export var whatever = (props) => <View data-uid='aaa'>
  <cake data-uid='aab' style={{backgroundColor: 'red'}} left={props.left + spacing} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      left: jsxAttributeOtherJavaScript(
        'props.left + spacing',
        'return props.left + spacing;',
        ['spacing', 'props'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
      ),
      right: jsxAttributeValue(20, emptyComments),
      top: jsxAttributeValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'expression',
      defaultPropsParam,
      ['left'],
      view,
      null,
      false,
      emptyComments,
    )
    const transpiledJSCode = `var spacing = 20;
return { spacing: spacing };`
    const jsVariable = arbitraryJSBlock(
      'var spacing = 20',
      transpiledJSCode,
      ['spacing'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const topLevelElements = [jsVariable, exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(jsVariable),
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, with code component', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
var MyComp = props => {
  return React.createElement(
    "div",
    {
      style: {
        position: "absolute",
        left: props.layout.left,
        backgroundColor: "hotpink"
      }
    },
    "hello"
  );
};
export var whatever = (props) => <View data-uid='aaa'>
  <MyComp data-uid='aab' layout={{left: 100}} />
</View>
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const jsCode = `var MyComp = props => {
  return React.createElement(
    "div",
    {
      style: {
        position: "absolute",
        left: props.layout.left,
        backgroundColor: "hotpink"
      }
    },
    "hello"
  );
};`
    const transpiledJsCode = `var MyComp = function MyComp(props) {
  return React.createElement("div", {
    style: {
      position: "absolute",
      left: props.layout.left,
      backgroundColor: "hotpink"
    }
  }, "hello");
};
return { MyComp: MyComp };`
    const definedElseWhere = ['React', JSX_CANVAS_LOOKUP_FUNCTION_NAME]
    const MyComp = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['MyComp'],
      definedElseWhere,
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const myCompAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      layout: jsxAttributeValue({ left: 100 }, emptyComments),
    })
    const myCompElement = jsxElement('MyComp', 'aab', myCompAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [myCompElement],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'expression',
      defaultPropsParam,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const topLevelElements = [MyComp, exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)
    const expectedResult = parseSuccess(
      sampleImportsForTests,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(MyComp),
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, with jsx code component', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
var MyComp = (props) => {
  return (
    <div
      data-uid='abc'
      style={{
        position: "absolute",
        left: props.layout.left,
        top: props.layout.top,
        width: 100,
        height: 100,
        backgroundColor: "hotpink"
      }}
    >hello</div>
  );
};
export var whatever = props => (
  <View data-uid='aaa'>
    <MyComp data-uid='aab' layout={{left: 100}} />
  </View>
)
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))

    const rootDivAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('abc', emptyComments),
      style: jsxAttributeNestedObject(
        [
          jsxPropertyAssignment(
            'position',
            jsxAttributeValue('absolute', emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'left',
            jsxAttributeOtherJavaScript(
              'props.layout.left',
              'return props.layout.left;',
              ['props'],
              expect.objectContaining({}),
              {},
            ),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'top',
            jsxAttributeOtherJavaScript(
              'props.layout.top',
              'return props.layout.top;',
              ['props'],
              expect.objectContaining({}),
              {},
            ),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'width',
            jsxAttributeValue(100, emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'height',
            jsxAttributeValue(100, emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'backgroundColor',
            jsxAttributeValue('hotpink', emptyComments),
            emptyComments,
            emptyComments,
          ),
        ],
        emptyComments,
      ),
    })

    const rootDiv = jsxElement('div', 'abc', rootDivAttributes, [jsxTextBlock('hello')])

    const myComp = utopiaJSXComponent(
      'MyComp',
      true,
      'var',
      'block',
      defaultPropsParam,
      ['layout'],
      rootDiv,
      null,
      false,
      emptyComments,
    )

    const myCompAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      layout: jsxAttributeValue({ left: 100 }, emptyComments),
    })
    const myCompElement = jsxElement('MyComp', 'aab', myCompAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [myCompElement],
    )
    const whatever = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'parenthesized-expression',
      defaultPropsParam,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const topLevelElements = [myComp, whatever].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)
    const expectedResult = parseSuccess(
      sampleImportsForTests,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, component with unknown jsx element is arbitrary js', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
export var Whatever = (props) => <View>
  <MyComp layout={{left: 100}} />
</View>
`
    const actualResult = parseCode('code.tsx', code, null, emptySet())
    if (isParseSuccess(actualResult)) {
      expect(actualResult.topLevelElements.filter(isArbitraryJSBlock).length).toEqual(1)
      expect(actualResult.topLevelElements.filter(isUtopiaJSXComponent).length).toEqual(0)
    } else {
      fail('Parse result is not a success.')
    }
  })

  it('parses the code when it is a function, with an arbitrary piece of JavaScript', () => {
    const code = `import * as React from "react";
import { View } from "utopia-api";
import { cake } from 'cake'
export var whatever = (props) => <View data-uid='aaa'>
<cake data-uid='aab' left={2 + 2} />
</View>
`
    const actualResult = testParseCode(code)
    if (isParseSuccess(actualResult)) {
      expect(actualResult.topLevelElements.filter(isArbitraryJSBlock).length).toEqual(0)
      expect(actualResult.topLevelElements.filter(isUtopiaJSXComponent).length).toEqual(1)

      const result = actualResult.topLevelElements.find(isUtopiaJSXComponent)!
      if (isJSXElement(result.rootElement)) {
        if (result.rootElement.children.length === 1) {
          const child = result.rootElement.children[0]
          if (isJSXElement(child)) {
            const childPropKeys = child.props.filter(isJSXAttributesEntry).map((prop) => prop.key)
            expect(childPropKeys).toEqual(['data-uid', 'left'])
            const leftProp = getJSXAttributeForced(child.props, 'left')
            expect(leftProp.type).toEqual('ATTRIBUTE_OTHER_JAVASCRIPT')
          } else {
            fail(`First child is not an element ${child}`)
          }
        } else {
          fail(`Unexpected number of children returned: ${result.rootElement.children.length}`)
        }
      } else {
        fail(`Root element is not an element ${result.rootElement}`)
      }
    }
  })
  it('parses the code when it is a function, with a nested object', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import { cake } from 'cake'
export var whatever = (props) => <View data-uid='aaa'>
<cake data-uid='aab' style={{backgroundColor: 'red', color: [props.color, -200]}} />
</View>
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const cake = jsxElement(
      'cake',
      'aab',
      jsxAttributesFromMap({
        'data-uid': jsxAttributeValue('aab', emptyComments),
        style: jsxAttributeNestedObjectSimple(
          jsxAttributesFromMap({
            backgroundColor: jsxAttributeValue('red', emptyComments),
            color: jsxAttributeNestedArraySimple([
              jsxAttributeOtherJavaScript(
                'props.color',
                'return props.color;',
                ['props'],
                expect.objectContaining({}),
                {},
              ),
              jsxAttributeValue(-200, emptyComments),
            ]),
          }),
          emptyComments,
        ),
      }),
      [],
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'expression',
      defaultPropsParam,
      ['color'],
      view,
      null,
      false,
      emptyComments,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it is a var', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
import { cake } from 'cake'
export var whatever = () => <View data-uid='aaa'>
<cake data-uid='aab' style={{backgroundColor: 'red'}} />
</View>
`
    const actualResult = testParseCode(code)
    const cake = jsxElement(
      'cake',
      'aab',
      jsxAttributesFromMap({
        'data-uid': jsxAttributeValue('aab', emptyComments),
        style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      }),
      [],
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'expression',
      null,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported]),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it has empty brackets {} as jsx child', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
export var App = (props) => <View data-uid='bbb'>
  {}
</View>
`
    const actualResult = testParseCode(code)
    const emptyBrackets = {
      ...jsxArbitraryBlock('', '', 'return undefined', [], null, {}),
      uniqueID: expect.any(String),
    }
    const view = jsxElement(
      'View',
      'bbb',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('bbb', emptyComments) }),
      [emptyBrackets],
    )
    const exported = utopiaJSXComponent(
      'App',
      true,
      'var',
      'expression',
      defaultPropsParam,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const expectedResult = parseSuccess(
      sampleImportsForTests,
      expect.arrayContaining([exported]),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'App'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it has a JSX block with an object defined inside', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
const a = "cake"
export var App = (props) => <View data-uid='bbb'>
  {{a: a}}
</View>
`
    const actualResult = testParseCode(code)
    expect(clearParseResultUniqueIDsAndEmptyBlocks(actualResult)).toMatchSnapshot()
  })

  it('parses back and forth as a var', () => {
    const cake = jsxElement(
      'cake',
      'aab',
      jsxAttributesFromMap({
        'data-uid': jsxAttributeValue('aab', emptyComments),
        style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
        left: jsxAttributeValue(10, emptyComments),
        name: jsxAttributeValue('test', emptyComments),
      }),
      [],
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'parenthesized-expression',
      null,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const detailOfExports = addModifierExportToDetail(EmptyExportsDetail, 'whatever')
    const printedCode = printCode(
      printCodeOptions(false, true, true),
      imports,
      [exported],
      null,
      detailOfExports,
    )
    const actualResult = testParseCode(printedCode)
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported]),
      expect.objectContaining({}),
      null,
      null,
      detailOfExports,
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses back and forth as a var, with some arbitrary javascript', () => {
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aab', emptyComments),
      style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      left: jsxAttributeOtherJavaScript(
        'getSizing(spacing)',
        'return getSizing(spacing);',
        ['getSizing', 'spacing'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
      ),
      right: jsxAttributeValue(20, emptyComments),
      top: jsxAttributeValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'parenthesized-expression',
      defaultPropsParam,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const jsCode1 = `function getSizing(n) {
  return 100 + n
}`
    const transpiledJSCode1 = `function getSizing(n) {
  return 100 + n;
}
return { getSizing: getSizing };`
    const arbitraryBlock1 = arbitraryJSBlock(
      jsCode1,
      transpiledJSCode1,
      ['getSizing'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const jsCode2 = `var spacing = 20`
    const transpiledJSCode2 = `var spacing = 20;
return { spacing: spacing };`
    const arbitraryBlock2 = arbitraryJSBlock(
      jsCode2,
      transpiledJSCode2,
      ['spacing'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const combinedJSCode = `function getSizing(n) {
  return 100 + n
}
var spacing = 20`
    const transpiledcombinedJSCode = `function getSizing(n) {
  return 100 + n;
}

var spacing = 20;
return { getSizing: getSizing, spacing: spacing };`
    const combinedArbitraryBlock = arbitraryJSBlock(
      combinedJSCode,
      transpiledcombinedJSCode,
      ['getSizing', 'spacing'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const topLevelElements = [arbitraryBlock1, arbitraryBlock2, exported].map(
      clearTopLevelElementUniqueIDsAndEmptyBlocks,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const detailOfExports = addModifierExportToDetail(EmptyExportsDetail, 'whatever')
    const printedCode = printCode(
      printCodeOptions(false, true, true, false, true),
      imports,
      [...topLevelElements],
      null,
      detailOfExports,
    )
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(printedCode))
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(combinedArbitraryBlock),
      detailOfExports,
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses back and forth as a var and includes canvas metadata', () => {
    const cake = jsxElement(
      'cake',
      'aab',
      jsxAttributesFromMap({
        'data-uid': jsxAttributeValue('aab', emptyComments),
        style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      }),
      [],
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'parenthesized-expression',
      null,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const detailOfExports = addModifierExportToDetail(EmptyExportsDetail, 'whatever')
    const printedCode = printCode(
      printCodeOptions(false, true, true),
      imports,
      [exported],
      null,
      detailOfExports,
    )
    const actualResult = testParseCode(printedCode)
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported]),
      expect.objectContaining({}),
      null,
      null,
      detailOfExports,
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses back and forth as a function, with an arbitrary piece of JavaScript', () => {
    const code = applyPrettier(
      `import { cake } from "cake";
import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
function cakeFn(n) {
  return n
}
function otherFn(n) {
  return n
}
export var whatever = props => {
  return (
    <View data-uid="aaa">
      <cake data-uid="aab" left={cakeFn(otherFn("b") + 2)} />
    </View>
  );
};
`,
      false,
    ).formatted
    const parsedCode = testParseCode(code)
    expect(clearParseResultUniqueIDsAndEmptyBlocks(parsedCode)).toMatchSnapshot()
  })
  it('parses back and forth as a function, with an empty bracket jsx child {}', () => {
    const code = applyPrettier(
      `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from 'utopia-api'

export var whatever = props => {
  return (
    <View data-uid="aaa">
      {}
    </View>
  );
};
`,
      false,
    ).formatted
    const parsedCode = testParseCode(code)
    if (isParseSuccess(parsedCode)) {
      const printedCode = printCode(
        printCodeOptions(false, true, true),
        sampleImportsForTests,
        parsedCode.topLevelElements,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      )
      expect(printedCode).toEqual(code)
    } else {
      fail('Parse result is not a success.')
    }
  })
  it('parses back and forth as a function', () => {
    const cake = jsxElement(
      'cake',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsxAttributeValue('aaa', emptyComments),
        style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
      }),
      [],
    )
    const view = jsxElement(
      'View',
      'aab',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aab', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'parenthesized-expression',
      defaultPropsParam,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const detailOfExports = addModifierExportToDetail(EmptyExportsDetail, 'whatever')
    const printedCode = printCode(
      printCodeOptions(false, true, true),
      imports,
      [exported],
      null,
      detailOfExports,
    )
    const actualResult = testParseCode(printedCode)
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported]),
      expect.objectContaining({}),
      null,
      null,
      detailOfExports,
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses back and forth as a function, with null and undefined values for props', () => {
    const cake = jsxElement(
      'cake',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsxAttributeValue('aaa', emptyComments),
        style: jsxAttributeValue({ backgroundColor: 'red' }, emptyComments),
        trueProp: jsxAttributeValue(true, emptyComments),
        falseProp: jsxAttributeValue(false, emptyComments),
        nullProp: jsxAttributeValue(null, emptyComments),
        undefinedProp: jsxAttributeValue(undefined, emptyComments),
      }),
      [],
    )
    const view = jsxElement(
      'View',
      'aab',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aab', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'parenthesized-expression',
      defaultPropsParam,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const detailOfExports = addModifierExportToDetail(EmptyExportsDetail, 'whatever')
    const printedCode = printCode(
      printCodeOptions(false, true, true),
      imports,
      [exported],
      null,
      detailOfExports,
    )
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(printedCode))
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported]),
      expect.objectContaining({}),
      null,
      null,
      detailOfExports,
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses back and forth as a function, with a nested object', () => {
    const cake = jsxElement(
      'cake',
      'aab',
      jsxAttributesFromMap({
        'data-uid': jsxAttributeValue('aab', emptyComments),
        style: jsxAttributeNestedObjectSimple(
          jsxAttributesFromMap({
            backgroundColor: jsxAttributeValue('red', emptyComments),
            color: jsxAttributeNestedArraySimple([
              jsxAttributeOtherJavaScript(
                'props.color',
                'return props.color;',
                ['props'],
                expect.objectContaining({}),
                {},
              ),
              jsxAttributeValue(-200, emptyComments),
            ]),
            boxShadow: jsxAttributeFunctionCall('createShadow', [
              jsxAttributeValue(15, emptyComments),
              jsxAttributeOtherJavaScript(
                'props.shadowValue',
                'return props.shadowValue;',
                ['props'],
                expect.objectContaining({}),
                {},
              ),
              jsxAttributeNestedArraySimple([
                jsxAttributeValue('hello', emptyComments),
                jsxAttributeOtherJavaScript(
                  'props.there',
                  'return props.there;',
                  ['props'],
                  expect.objectContaining({}),
                  {},
                ),
              ]),
            ]),
          }),
          emptyComments,
        ),
      }),
      [],
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [cake],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'parenthesized-expression',
      defaultPropsParam,
      ['color', 'shadowValue', 'there'],
      view,
      null,
      false,
      emptyComments,
    )
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const detailOfExports = addModifierExportToDetail(EmptyExportsDetail, 'whatever')
    const printedCode = printCode(
      printCodeOptions(false, true, true),
      imports,
      [exported],
      null,
      detailOfExports,
    )
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(printedCode))
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      detailOfExports,
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('preserves modifiers on variables', () => {
    const code = applyPrettier(
      `const first = 100
let second = 'cake'
export var ${BakedInStoryboardVariableName} = <Storyboard data-uid='${BakedInStoryboardUID}' />
`,
      false,
    ).formatted
    const expectedCode = applyPrettier(
      `
const first = 100;
let second = "cake";
export var ${BakedInStoryboardVariableName} = <Storyboard data-uid='${BakedInStoryboardUID}' />
`,
      false,
    ).formatted
    const parsedCode = testParseCode(code)
    if (isParseSuccess(parsedCode)) {
      const printedCode = printCode(
        printCodeOptions(false, true, true),
        emptyImports(),
        parsedCode.topLevelElements,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      )
      expect(printedCode).toEqual(expectedCode)
    } else {
      throw new Error('Did not parse successfully.')
    }
  })
  it('parses code within a component block', () => {
    const code = `import { cake } from 'cake'
import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
export var whatever = props => {
  function test(n) {
    return n * 2
  }
  return (
    <View data-uid="aaa">
      <cake data-uid="aab" left={test(100)} />
    </View>
  );
};
`
    const jsCode = `function test(n) {
    return n * 2
  }`
    const transpiledJSCode = `function test(n) {
  return n * 2;
}
return { test: test };`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const imports = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJSCode,
      ['test'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining(
        [
          utopiaJSXComponent(
            'whatever',
            true,
            'var',
            'block',
            defaultPropsParam,
            [],
            jsxElement(
              'View',
              'aaa',
              jsxAttributesFromMap({
                'data-uid': jsxAttributeValue('aaa', emptyComments),
              }),
              [
                jsxElement(
                  'cake',
                  'aab',
                  jsxAttributesFromMap({
                    'data-uid': jsxAttributeValue('aab', emptyComments),
                    left: jsxAttributeOtherJavaScript(
                      'test(100)',
                      'return test(100);',
                      ['test'],
                      expect.objectContaining({
                        sources: ['code.tsx'],
                        version: 3,
                        file: 'code.tsx',
                      }),
                      {},
                    ),
                  }),
                  [],
                ),
              ],
            ),
            clearArbitraryJSBlockUniqueIDs(arbitraryBlock),
            false,
            emptyComments,
          ),
        ].map(clearTopLevelElementUniqueIDsAndEmptyBlocks),
      ),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )

    expect(actualResult).toEqual(expectedResult)
  })
  it('parses arbitrary code in a component back and forth', () => {
    const jsCode = `function test(n) {
    return n * 2
  }`
    const transpiledJSCode = `function test(n) {
  return n * 2;
}
return { test: test };`
    const imports = addImport(
      'code.jsx',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const components = [
      clearTopLevelElementUniqueIDsAndEmptyBlocks(
        utopiaJSXComponent(
          'whatever',
          true,
          'var',
          'block',
          defaultPropsParam,
          [],
          jsxElement(
            'View',
            'aaa',
            jsxAttributesFromMap({
              'data-uid': jsxAttributeValue('aaa', emptyComments),
            }),
            [
              jsxElement(
                'cake',
                'aab',
                jsxAttributesFromMap({
                  'data-uid': jsxAttributeValue('aab', emptyComments),
                  left: jsxAttributeOtherJavaScript(
                    'test(100)',
                    'return test(100);',
                    ['test'],
                    expect.objectContaining({
                      sources: ['code.tsx'],
                      version: 3,
                      file: 'code.tsx',
                    }),
                    {},
                  ),
                }),
                [],
              ),
            ],
          ),
          arbitraryJSBlock(
            jsCode,
            transpiledJSCode,
            ['test'],
            [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
            expect.objectContaining({
              sources: ['code.tsx'],
              version: 3,
              file: 'code.tsx',
            }),
            {},
          ),
          false,
          emptyComments,
        ),
      ),
      clearTopLevelElementUniqueIDsAndEmptyBlocks(
        utopiaJSXComponent(
          BakedInStoryboardVariableName,
          false,
          'var',
          'parenthesized-expression',
          null,
          [],
          jsxElement(
            'Storyboard',
            BakedInStoryboardUID,
            jsxAttributesFromMap({
              'data-uid': jsxAttributeValue(BakedInStoryboardUID, emptyComments),
            }),
            [],
          ),
          null,
          false,
          emptyComments,
        ),
      ),
    ]
    const detailOfExports = addModifierExportToDetail(
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      'storyboard',
    )
    const printedCode = printCode(
      printCodeOptions(false, true, true),
      imports,
      components,
      null,
      detailOfExports,
    )
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(printedCode))
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining(components.map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      detailOfExports,
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('preserve children of an element that is a block of random JavaScript', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
export var App = props => {
  return (
    <View data-uid='aaa'>
      {[1,2,3].map(x=> <View data-uid='abc' />)}
    </View>
  );
};`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const component = utopiaJSXComponent(
      'App',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({
          'data-uid': jsxAttributeValue('aaa', emptyComments),
        }),
        [
          jsxArbitraryBlock(
            `[1,2,3].map(x=> <View data-uid='abc' />)`,
            `[1, 2, 3].map(x => <View data-uid='abc' />);`,
            `return [1, 2, 3].map(function (x) {
  return utopiaCanvasJSXLookup("abc", {
    callerThis: this
  });
});`,
            ['React', 'View', 'utopiaCanvasJSXLookup'],
            expect.objectContaining({
              sources: ['code.tsx'],
              version: 3,
              file: 'code.tsx',
            }),
            {
              abc: jsxElement(
                'View',
                'abc',
                jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('abc', emptyComments) }),
                [],
              ),
            },
          ),
        ],
      ),
      null,
      false,
      emptyComments,
    )
    const expectedResult = parseSuccess(
      sampleImportsForTests,
      expect.arrayContaining([component].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'App'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('preserve text within a JSX element', () => {
    const code = `
import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
export var App = props => {
  return (
    <View data-uid='aaa'>cake</View>
  );
};`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const component = utopiaJSXComponent(
      'App',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({
          'data-uid': jsxAttributeValue('aaa', emptyComments),
        }),
        [jsxTextBlock('cake')],
      ),
      null,
      false,
      emptyComments,
    )
    const expectedResult = parseSuccess(
      sampleImportsForTests,
      expect.arrayContaining([component].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'App'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('captures an expression within a JSX element', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
export var App = props => {
  return (
    <View data-uid='aaa'>
      {[1, 2, 3].map(n => (
        <div data-uid="abc" />
      ))}
    </View>
  );
};`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const component = utopiaJSXComponent(
      'App',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({
          'data-uid': jsxAttributeValue('aaa', emptyComments),
        }),
        [
          jsxArbitraryBlock(
            `[1, 2, 3].map(n => (
        <div data-uid="abc" />
      ))`,
            `[1, 2, 3].map((n) =>
<div data-uid="abc" />);`,
            `return [1, 2, 3].map(function (n) {
  return utopiaCanvasJSXLookup("abc", {
    callerThis: this
  });
});`,
            ['React', 'utopiaCanvasJSXLookup'],
            expect.objectContaining({
              sources: ['code.tsx'],
              version: 3,
              file: 'code.tsx',
            }),
            {
              abc: jsxElement(
                'div',
                'abc',
                jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('abc', emptyComments) }),
                [],
              ),
            },
          ),
        ],
      ),
      null,
      false,
      emptyComments,
    )
    const expectedResult = parseSuccess(
      sampleImportsForTests,
      expect.arrayContaining([component].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'App'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('ensure the sample file parses successfully', () => {
    const parseResult = testParseCode(sampleCode)
    expect(isParseSuccess(parseResult)).toBe(true)
  })
  it('parses expressions in JSX back and forth', () => {
    const component = utopiaJSXComponent(
      'App',
      true,
      'var',
      'parenthesized-expression',
      defaultPropsParam,
      [],
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({
          'data-uid': jsxAttributeValue('aaa', emptyComments),
        }),
        [
          jsxArbitraryBlock(
            `[1, 2, 3].map((n) => (
      <div data-uid='abc' />
    ))`,
            `[1, 2, 3].map((n) =>
<div data-uid='abc' />);`,
            `return [1, 2, 3].map(function (n) {
  return utopiaCanvasJSXLookup("abc", {
    callerThis: this
  });
});`,
            ['React', 'utopiaCanvasJSXLookup'],
            expect.objectContaining({
              sources: ['code.tsx'],
              version: 3,
              file: 'code.tsx',
            }),
            {
              abc: jsxElement(
                'div',
                'abc',
                jsxAttributesFromMap({
                  'data-uid': jsxAttributeValue('abc', emptyComments),
                }),
                [],
              ),
            },
          ),
        ],
      ),
      null,
      false,
      emptyComments,
    )
    const detailOfExports = addModifierExportToDetail(EmptyExportsDetail, 'App')
    const printedCode = printCode(
      printCodeOptions(false, true, true),
      sampleImportsForTests,
      [component],
      null,
      detailOfExports,
    )
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(printedCode))
    const expectedResult = parseSuccess(
      sampleImportsForTests,
      expect.arrayContaining([component].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      detailOfExports,
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('should handle inner components', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
export var App = props => {
  const a = 20;
  const b = 40;
  const MyCustomCompomnent = props => <View data-uid="abc">{props.children}</View>;

  return (
    <View
      style={{ backgroundColor: "lightgrey", position: "absolute" }}
      layout={{
        height: props.layout.height,
        left: props.layout.left,
        width: props.layout.width,
        top: props.layout.top
      }}
      data-uid="aaa"
    >
      <MyCustomCompomnent data-uid="ddd">
        <Ellipse
          style={{ backgroundColor: "lightgreen" }}
          layout={{ height: 100, left: 150, width: 100, top: 540 }}
          data-uid="bbb"
        />
        <Rectangle
          style={{ backgroundColor: "orange" }}
          layout={{ height: 100, left: 150, width: 100, top: 540 }}
          data-uid="ccc"
        />
      </MyCustomCompomnent>
      <View
        style={{ backgroundColor: "blue", position: "absolute" }}
        layout={{ height: 200, left: 80, width: 100, top: 145 }}
        data-uid="ggg"
      />
    </View>
  );
};`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const ellipse = jsxElement(
      'Ellipse',
      'bbb',
      jsxAttributesFromMap({
        style: jsxAttributeValue({ backgroundColor: 'lightgreen' }, emptyComments),
        layout: jsxAttributeValue({ height: 100, left: 150, width: 100, top: 540 }, emptyComments),
        'data-uid': jsxAttributeValue('bbb', emptyComments),
      }),
      [],
    )
    const rectangle = jsxElement(
      'Rectangle',
      'ccc',
      jsxAttributesFromMap({
        style: jsxAttributeValue({ backgroundColor: 'orange' }, emptyComments),
        layout: jsxAttributeValue({ height: 100, left: 150, width: 100, top: 540 }, emptyComments),
        'data-uid': jsxAttributeValue('ccc', emptyComments),
      }),
      [],
    )
    const myCustomCompomnent = jsxElement(
      'MyCustomCompomnent',
      'ddd',
      jsxAttributesFromMap({
        'data-uid': jsxAttributeValue('ddd', emptyComments),
      }),
      [ellipse, rectangle],
    )
    const view = jsxElement(
      'View',
      'ggg',
      jsxAttributesFromMap({
        style: jsxAttributeValue({ backgroundColor: 'blue', position: 'absolute' }, emptyComments),
        layout: jsxAttributeValue({ height: 200, left: 80, width: 100, top: 145 }, emptyComments),
        'data-uid': jsxAttributeValue('ggg', emptyComments),
      }),
      [],
    )
    const component = utopiaJSXComponent(
      'App',
      true,
      'var',
      'block',
      defaultPropsParam,
      ['layout'],
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({
          style: jsxAttributeValue(
            { backgroundColor: 'lightgrey', position: 'absolute' },
            emptyComments,
          ),
          layout: jsxAttributeNestedObjectSimple(
            jsxAttributesFromMap({
              height: jsxAttributeOtherJavaScript(
                'props.layout.height',
                'return props.layout.height;',
                ['props'],
                expect.objectContaining({}),
                {},
              ),
              left: jsxAttributeOtherJavaScript(
                'props.layout.left',
                'return props.layout.left;',
                ['props'],
                expect.objectContaining({}),
                {},
              ),
              width: jsxAttributeOtherJavaScript(
                'props.layout.width',
                'return props.layout.width;',
                ['props'],
                expect.objectContaining({}),
                {},
              ),
              top: jsxAttributeOtherJavaScript(
                'props.layout.top',
                'return props.layout.top;',
                ['props'],
                expect.objectContaining({}),
                {},
              ),
            }),
            emptyComments,
          ),
          'data-uid': jsxAttributeValue('aaa', emptyComments),
        }),
        [myCustomCompomnent, view],
      ),
      arbitraryJSBlock(
        `const a = 20;
  const b = 40;
  const MyCustomCompomnent = props => <View data-uid="abc">{props.children}</View>;`,
        `var a = 20;
var b = 40;

var MyCustomCompomnent = function MyCustomCompomnent(props) {
  return ${JSX_CANVAS_LOOKUP_FUNCTION_NAME}("abc", {
    props: props,
    callerThis: this
  });
};
return { a: a, b: b, MyCustomCompomnent: MyCustomCompomnent };`,
        ['a', 'b', 'MyCustomCompomnent'],
        ['React', 'View', JSX_CANVAS_LOOKUP_FUNCTION_NAME],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {
          abc: expect.objectContaining({
            name: expect.objectContaining({
              baseVariable: 'View',
            }),
          }),
        },
      ),
      false,
      emptyComments,
    )
    const expectedResult = parseSuccess(
      sampleImportsForTests,
      expect.arrayContaining([component].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'App'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('properties without a value are treated as a property assigned to true', () => {
    const code = `
import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
export var App = props => {
  return (
    <View data-uid='aaa' booleanProperty />
  )
}`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const component = utopiaJSXComponent(
      'App',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({
          'data-uid': jsxAttributeValue('aaa', emptyComments),
          booleanProperty: jsxAttributeValue(true, emptyComments),
        }),
        [],
      ),
      null,
      false,
      emptyComments,
    )
    const expectedResult = parseSuccess(
      sampleImportsForTests,
      expect.arrayContaining([component].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'App'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('true attributes are printed without a value assigned to them directly', () => {
    const expectedResult = applyPrettier(
      `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
export var whatever = props => {
  return <View data-uid="aaa" booleanProperty />;
};
`,
      false,
    ).formatted
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsxAttributeValue('aaa', emptyComments),
        booleanProperty: jsxAttributeValue(true, emptyComments),
      }),
      [],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const actualResult = printCode(
      printCodeOptions(false, true, true),
      sampleImportsForTests,
      [exported],
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('false attributes are printed as explicit assignments', () => {
    const expectedResult = applyPrettier(
      `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";
export var whatever = props => {
  return <View data-uid="aaa" booleanProperty={false} />;
};
`,
      false,
    ).formatted
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsxAttributeValue('aaa', emptyComments),
        booleanProperty: jsxAttributeValue(false, emptyComments),
      }),
      [],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const actualResult = printCode(
      printCodeOptions(false, true, true),
      sampleImportsForTests,
      [exported],
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('injects infinite loop prevention into while and for loops', () => {
    const code = `import * as React from "react"
export var whatever = props => {
  for (var n = 0; n != -1; n++) {
    const n2 = n * 2
  }
  while (true) {
    const a = 1
  }
  return <div data-uid='aaa'></div>
}`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const arbitraryBlockCode = `for (var n = 0; n != -1; n++) {
    const n2 = n * 2
  }
  while (true) {
    const a = 1
  }`
    const arbitraryBlockTranspiledCode = `var _loopIt = 0,
    _loopIt2 = 0;

for (var n = 0; n != -1; n++) {
  if (_loopIt++ > ${InfiniteLoopMaxIterations}) {
    throw new RangeError('${InfiniteLoopError}');
  }

  var n2 = n * 2;
}

while (true) {
  if (_loopIt2++ > ${InfiniteLoopMaxIterations}) {
    throw new RangeError('${InfiniteLoopError}');
  }

  var a = 1;
}
return {  };`
    const arbitraryBlock = arbitraryJSBlock(
      arbitraryBlockCode,
      arbitraryBlockTranspiledCode,
      [],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const view = jsxElement(
      'div',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
      false,
      emptyComments,
    )
    const expectedResult = parseSuccess(
      { react: sampleImportsForTests['react'] },
      expect.arrayContaining([exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('handles variables defined in a for loop declaration', () => {
    const code = `import * as React from "react"
export var whatever = props => {
  let result = []
  for (var n = 0; n < 5; n++) {
    const n2 = n * 2
    result.push(<div style={{ left: n, top: n2 }} data-uid='bbb' />)
  }
  return <div data-uid='aaa'>{result}</div>
}`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const arbitraryBlockCode = `let result = []
  for (var n = 0; n < 5; n++) {
    const n2 = n * 2
    result.push(<div style={{ left: n, top: n2 }} data-uid='bbb' />)
  }`
    const arbitraryBlockTranspiledCode = `var _loopIt = 0;
var result = [];

for (var n = 0; n < 5; n++) {
  if (_loopIt++ > ${InfiniteLoopMaxIterations}) {
    throw new RangeError('${InfiniteLoopError}');
  }

  var n2 = n * 2;
  result.push(${JSX_CANVAS_LOOKUP_FUNCTION_NAME}("bbb", {
    n: n,
    n2: n2,
    callerThis: this
  }));
}
return { result: result };`
    const arbitraryBlock = arbitraryJSBlock(
      arbitraryBlockCode,
      arbitraryBlockTranspiledCode,
      ['result'],
      ['React', JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {
        bbb: expect.objectContaining({
          name: expect.objectContaining({
            baseVariable: 'div',
          }),
        }),
      },
    )
    const innerBlock = jsxArbitraryBlock(
      'result',
      'result;',
      'return result;',
      ['result'],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const view = jsxElement(
      'div',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [innerBlock],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
      false,
      emptyComments,
    )
    const expectedResult = parseSuccess(
      { react: sampleImportsForTests['react'] },
      expect.arrayContaining([exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('defined elsewhere values are assigned for elements inside arbitrary blocks', () => {
    const code = `import * as React from "react"
export var whatever = props => {
  return <div data-uid='aaa'>
    {[1, 2, 3].map(n => {
      return <div style={{left: n * 30, top: n * 30}} data-uid='bbb' />
    })}
  </div>
}`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const arbitraryBlockOriginalCode = `[1, 2, 3].map(n => {
      return <div style={{left: n * 30, top: n * 30}} data-uid='bbb' />
    })`
    const arbitraryBlockCode = `[1, 2, 3].map(n => {
  return <div style={{ left: n * 30, top: n * 30 }} data-uid='bbb' />;
});`
    const arbitraryBlockTranspiledCode = `return [1, 2, 3].map(function (n) {
  return utopiaCanvasJSXLookup("bbb", {
    n: n,
    callerThis: this
  });
});`
    const innerElement = jsxElement(
      'div',
      'bbb',
      jsxAttributesFromMap({
        style: jsxAttributeNestedObject(
          [
            jsxPropertyAssignment(
              'left',
              jsxAttributeOtherJavaScript(
                `n * 30`,
                `return n * 30;`,
                ['n'],
                expect.objectContaining({}),
                {},
              ),
              emptyComments,
              emptyComments,
            ),
            jsxPropertyAssignment(
              'top',
              jsxAttributeOtherJavaScript(
                `n * 30`,
                `return n * 30;`,
                ['n'],
                expect.objectContaining({}),
                {},
              ),
              emptyComments,
              emptyComments,
            ),
          ],
          emptyComments,
        ),
        ['data-uid']: jsxAttributeValue('bbb', emptyComments),
      }),
      [],
    )
    const arbitraryBlock = jsxArbitraryBlock(
      arbitraryBlockOriginalCode,
      arbitraryBlockCode,
      arbitraryBlockTranspiledCode,
      ['React', 'utopiaCanvasJSXLookup'],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      { bbb: innerElement },
    )
    const view = jsxElement(
      'div',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [arbitraryBlock],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const expectedResult = parseSuccess(
      { react: sampleImportsForTests['react'] },
      expect.arrayContaining([exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('defined elsewhere values coming from outside the block are recognised in props', () => {
    const code = `import * as React from "react"
export var whatever = props => {
  const a = 30
  return <div data-uid='aaa'>
    {[1, 2, 3].map(n => {
      return <div style={{left: n * a, top: n * a}} data-uid='bbb' />
    })}
  </div>
}`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const arbitraryBlockOriginalCode = `[1, 2, 3].map(n => {
      return <div style={{left: n * a, top: n * a}} data-uid='bbb' />
    })`
    const arbitraryBlockCode = `[1, 2, 3].map(n => {
  return <div style={{ left: n * a, top: n * a }} data-uid='bbb' />;
});`
    const arbitraryBlockTranspiledCode = `return [1, 2, 3].map(function (n) {
  return utopiaCanvasJSXLookup("bbb", {
    n: n,
    a: a,
    callerThis: this
  });
});`
    const innerElement = jsxElement(
      'div',
      'bbb',
      jsxAttributesFromMap({
        style: jsxAttributeNestedObject(
          [
            jsxPropertyAssignment(
              'left',
              jsxAttributeOtherJavaScript(
                `n * a`,
                `return n * a;`,
                ['n', 'a'],
                expect.objectContaining({}),
                {},
              ),
              emptyComments,
              emptyComments,
            ),
            jsxPropertyAssignment(
              'top',
              jsxAttributeOtherJavaScript(
                `n * a`,
                `return n * a;`,
                ['n', 'a'],
                expect.objectContaining({}),
                {},
              ),
              emptyComments,
              emptyComments,
            ),
          ],
          emptyComments,
        ),
        ['data-uid']: jsxAttributeValue('bbb', emptyComments),
      }),
      [],
    )
    const arbitraryBlock = jsxArbitraryBlock(
      arbitraryBlockOriginalCode,
      arbitraryBlockCode,
      arbitraryBlockTranspiledCode,
      ['a', 'React', 'utopiaCanvasJSXLookup'],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      { bbb: innerElement },
    )

    const topLevelArbitraryBlock = arbitraryJSBlock(
      `const a = 30`,
      `var a = 30;
return { a: a };`,
      ['a'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )

    const view = jsxElement(
      'div',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
      [arbitraryBlock],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      topLevelArbitraryBlock,
      false,
      emptyComments,
    )
    const expectedResult = parseSuccess(
      { react: sampleImportsForTests['react'] },
      expect.arrayContaining([exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('svg elements are accepted', () => {
    const code = `import * as React from "react"
export var whatever = props => {
  return <svg data-uid='abc'/>
}`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const view = jsxElement(
      'svg',
      'abc',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('abc', emptyComments) }),
      [],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      null,
      false,
      emptyComments,
    )
    const expectedResult = parseSuccess(
      { react: sampleImportsForTests['react'] },
      expect.arrayContaining([exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses fine with a circular dependency.', () => {
    const code = `import * as React from "react";
import {
  View
} from "utopia-api";
const a = (n) => n > 0 ? n : b(10)
export var whatever = (props) => <View data-uid='aaa' />
const b = (n) => n > 0 ? n : a(10)
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    expect(clearParseResultPassTimes(actualResult)).toMatchSnapshot()
  })
  it('corrects duplicated data-uid entries', () => {
    const code = `import * as React from "react";
import {
  View
} from "utopia-api";
export var whatever = (props) => <View data-uid='aaa'>
  <View data-uid='aaa' />
</View>
export var whatever2 = (props) => <View data-uid='aaa'>
  <View data-uid='aaa' />
</View>
`
    const actualResult = testParseCode(code)
    foldParsedTextFile(
      (_) => fail('Unable to parse code.'),
      (success) => {
        let uids: Array<string> = []
        for (const topLevelElement of success.topLevelElements) {
          if (isUtopiaJSXComponent(topLevelElement)) {
            ensureElementsHaveUID(topLevelElement.rootElement, uids)
          }
        }
      },
      (_) => fail('Unable to parse code.'),
      actualResult,
    )
  })
  it('inserts data-uid into elements as part of the parse', () => {
    function checkDataUIDsPopulated(printableProjectContent: PrintableProjectContent): boolean {
      const printedCode = printCode(
        printCodeOptions(false, true, false, false, true),
        printableProjectContent.imports,
        printableProjectContent.topLevelElements,
        printableProjectContent.jsxFactoryFunction,
        printableProjectContent.exportsDetail,
      )
      const parseResult = testParseCode(printedCode)
      return foldParsedTextFile(
        (failure) => {
          console.error(failure)
          return false
        },
        (success) => {
          let uids: Array<string> = []
          for (const topLevelElement of success.topLevelElements) {
            if (isUtopiaJSXComponent(topLevelElement)) {
              ensureElementsHaveUID(topLevelElement.rootElement, uids)
            }
          }
          return true
        },
        (unparsed) => {
          console.error(unparsed)
          return false
        },
        parseResult,
      )
    }
    const printableArbitrary = printableProjectContentArbitrary()
    const dataUIDProperty = FastCheck.property(printableArbitrary, checkDataUIDsPopulated)
    FastCheck.assert(dataUIDProperty, { verbose: true })
  })
  it('when react is not imported treat components as arbitrary blocks', () => {
    const code = `
export var whatever = (props) => <View data-uid='aaa'>
  <View data-uid='aaa' />
</View>
`
    const actualResult = testParseCode(code)
    foldParsedTextFile(
      (_) => fail('Unable to parse code.'),
      (success) => {
        expect(elementsStructure(success.topLevelElements)).toMatchInlineSnapshot(`
          "UNPARSED_CODE
          ARBITRARY_JS_BLOCK
          UNPARSED_CODE"
        `)
      },
      (_) => fail('Unable to parse code.'),
      actualResult,
    )
  })
})

describe('SourceMap', () => {
  const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  View,
  Scene
} from "utopia-api";

console.log('hello!') // line 12 char 9

export var App = props => {
  const a = 20;
  const b = 40; // line 16 char 9

  return (
    <View
      style={{ backgroundColor: "lightgrey", position: "absolute" }}
      layout={{
        height: props.layout.height,
        left: props.layout.left,
        width: props.layout.width,
        top: props.layout.top,
      }}
      data-uid="aaa"
      arbitrary={console.log('hi')} // line 28, char 26
    >
    </View>
  )
}`
  it('maps a arbitraryJSBlock correctly', () => {
    const parseResult = testParseCode(code)
    if (!isParseSuccess(parseResult)) {
      fail('expected parseResult to be Right')
    }
    const consoleLogBlock = parseResult.topLevelElements.find(isArbitraryJSBlock)!

    if (
      !isArbitraryJSBlock(consoleLogBlock) ||
      consoleLogBlock.javascript !== `console.log('hello!')`
    ) {
      fail('expected the first topLevelElement to be the console logline')
    }

    const transpiledCharacter = 1 + consoleLogBlock.transpiledJavascript.indexOf('log')
    const transpiledLine = 1

    const consumer = getSourceMapConsumer(consoleLogBlock.sourceMap)

    const position = consumer.getOriginalPosition(transpiledLine, transpiledCharacter)

    expect(position).toEqual(expect.objectContaining({ line: 13, column: 9 }))
  })

  it('maps an arbitraryJSBlock inside a utopiaJSXComponent', () => {
    const parseResult = testParseCode(code)
    if (!isParseSuccess(parseResult)) {
      fail('expected parseResult to be Right')
    }
    const appComponent = parseResult.topLevelElements.find(isUtopiaJSXComponent)!
    if (!isUtopiaJSXComponent(appComponent) || appComponent.name !== `App`) {
      fail('expected the second topLevelElement to be the App component')
    }

    const arbitraryBlock = appComponent.arbitraryJSBlock

    if (arbitraryBlock == null) {
      fail(`expected the App component's arbitraryJSBlock to be defined`)
    }

    const transpiledCharacter = 1 + arbitraryBlock.transpiledJavascript.split('\n')[1].indexOf('b')
    const transpiledLine = 2

    const consumer = getSourceMapConsumer(arbitraryBlock.sourceMap)

    const position = consumer.getOriginalPosition(transpiledLine, transpiledCharacter)

    expect(position).toEqual(expect.objectContaining({ line: 17, column: 10 }))
  })

  it('maps a jsxAttributeOtherJavaScript correctly', () => {
    const parseResult = testParseCode(code)
    if (!isParseSuccess(parseResult)) {
      fail('expected parseResult to be a success')
    }
    const appComponent = parseResult.topLevelElements.find(isUtopiaJSXComponent)!
    if (!isUtopiaJSXComponent(appComponent) || appComponent.name !== `App`) {
      fail('expected the second topLevelElement to be the App component')
    }
    if (!isJSXElement(appComponent.rootElement)) {
      fail(`expected the App component's root element to be a JSXElement`)
    }

    const arbitraryProp = optionalMap(
      (p) => p.value,
      appComponent.rootElement.props
        .filter(isJSXAttributesEntry)
        .find((p) => p.key === 'arbitrary'),
    )

    if (arbitraryProp == null || !isJSXAttributeOtherJavaScript(arbitraryProp)) {
      fail(`expected <View /> to have an arbitrary js prop called props.arbitrary`)
    }

    const transpiledCharacter = 1 + arbitraryProp.transpiledJavascript.indexOf('log')

    const transpiledLine = 1

    const consumer = getSourceMapConsumer(arbitraryProp.sourceMap)

    const position = consumer.getOriginalPosition(transpiledLine, transpiledCharacter)

    expect(position).toEqual(expect.objectContaining({ line: 29, column: 26 }))
  })
})

describe('getHighlightBounds', () => {
  it('gets some bounds', () => {
    const bounds = getHighlightBoundsWithoutUID(
      'test.ts',
      `import * as React from "react";
    import {
      Ellipse,
      UtopiaUtils,
      Image,
      Rectangle,
      Text,
      View,
      Scene
    } from "utopia-api";
    
    console.log('hello!') // line 18 char 9
    
    export var App = props => {
      const a = 20;
      const b = 40; // line 22 char 9
    
      return (
        <View
          style={{ backgroundColor: "lightgrey", position: "absolute" }}
          layout={{
            height: props.layout.height,
            left: props.layout.left,
            width: props.layout.width,
            top: props.layout.top,
          }}
          data-uid="aaa"
          arbitrary={console.log('hi')} // line 34, char 26
        >
        </View>
      )
    })`,
    )
    expect(bounds).toMatchSnapshot()
  })
})

describe('lintAndParse', () => {
  it('returns a syntax error from eslint when something is broken', () => {
    const result = lintAndParse(
      'test.js',
      `
    import {
      Ellipse,
      UtopiaUtils,
      Image,
      Rectangle,
      Text,
      View,
      Scene
    } from "utopia-api";
    
    export var App = props => {
      const a = 20
      const b = 40
    
      return (
        <View
          style={{ backgroundColor: "darkgrey", position: "absolute" }, ...hello}
        >
        </View>
      )
    })`,
      null,
      emptySet(),
    )
    expect(clearParseResultPassTimes(result)).toMatchSnapshot()
  })
})

describe('Babel transpile', () => {
  it('can transpile jsx fragment', () => {
    const file = `
import * as React from 'react'

export var App = (props) => {
  return (
    <>
      <View
        style={{ ...props.style, backgroundColor: '#FFFFFF' }}
        layout={{ layoutSystem: 'pinSystem' }}
        data-uid='aaa'
      ></View>
    </>
  )
}`
    const code = `var App = (props) => {
      return (
        <>
          <View
            style={{ ...props.style, backgroundColor: '#FFFFFF' }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid='aaa'
          ></View>
        </>
      )
    }`

    const sourceMap = {
      version: 3,
      sources: [StoryboardFilePath],
      names: [
        '\n',
        ' ',
        'var',
        'App',
        '=',
        '(',
        'props',
        ')',
        '>',
        '{',
        'return',
        '<',
        'View',
        'style',
        '.',
        '|',
        '}',
        ',',
        'backgroundColor',
        ':',
        "'",
        '#',
        'FFFFFF',
        'layout',
        'layoutSystem',
        'pinSystem',
        'data',
        '-',
        'uid',
        'aaa',
        '/',
      ],
      mappings:
        'AAcEA;AACDA;AACMC,CAACC,GAAGD,CAACE,GAAGF,CAACG,CAACH,CAACI,CAACC,KAAKC,CAACN,CAACG,CAACI,CAACP,CAACQ,CAACT;AAC7BC,CAACA,CAACS,MAAMT,CAACI,CAACL;AACVC,CAACA,CAACA,CAACA,CAACU,CAACH,CAACR;AACNC,CAACA,CAACA,CAACA,CAACA,CAACA,CAACU,CAACC,IAAIZ;AACXC,CAACA,CAACA,CAACA,CAACA,CAACA,CAACA,CAACA,CAACY,KAAKT,CAACK,CAACA,CAACR,CAACa,CAACA,CAACA,CAACT,CAACC,KAAKQ,CAACD,KAAKZ,CAACc,CAACA,CAACd,CAACQ,CAACO,CAACT,CAACU,CAAChB,CAACiB,eAAeC,CAAClB,CAACmB,CAACC,CAACC,MAAMF,CAACnB,CAACe,CAACA,CAAChB;AACtEC,CAACA,CAACA,CAACA,CAACA,CAACA,CAACA,CAACA,CAACsB,MAAMnB,CAACK,CAACA,CAACR,CAACuB,YAAYL,CAAClB,CAACmB,CAACK,SAASL,CAACnB,CAACe,CAACA,CAAChB;AAC9CC,CAACA,CAACA,CAACA,CAACA,CAACA,CAACA,CAACA,CAACyB,IAAIC,CAACC,GAAGxB,CAACK,CAACW,CAACS,GAAGT,CAACJ,CAAChB;AACxBC,CAACA,CAACA,CAACA,CAACA,CAACA,CAACO,CAACG,CAACmB,CAAClB,IAAIJ,CAACR;AACdC,CAACA,CAACA,CAACA,CAACU,CAACmB,CAACtB,CAACR;AACPC,CAACA,CAACM,CAACP;AACHgB',
      file: StoryboardFilePath,
      sourcesContent: [
        "\nimport * as React from 'react'\nimport { View, jsx } from 'utopia-api'\n\nexport var canvasMetadata = {\n  scenes: [\n    {\n      component: 'App',\n      frame: { height: 812, left: 0, width: 375, top: 0 },\n      props: { layout: { top: 0, left: 0, bottom: 0, right: 0 } },\n      container: { layoutSystem: 'pinSystem' },\n    },\n  ],\n  elementMetadata: {},\n}\n\nexport var App = (props) => {\n  return (\n    <>\n      <View\n        style={{ ...props.style, backgroundColor: '#FFFFFF' }}\n        layout={{ layoutSystem: 'pinSystem' }}\n        data-uid='aaa'\n      ></View>\n    </>\n  )\n}\n",
      ],
    }
    expect(
      transpileJavascriptFromCode('test.js', file, code, sourceMap, [], false),
    ).toMatchSnapshot()
  })
})
