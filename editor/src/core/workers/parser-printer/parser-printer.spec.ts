/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "FastCheck.assert", "ensureElementsHaveUID", "checkElementUIDs"] }] */
import * as FastCheck from 'fast-check'
import { getSourceMapConsumer } from '../../../third-party/react-error-overlay/utils/getSourceMap'
import type { JSXAttributes, JSXElement, UtopiaJSXComponent } from '../../shared/element-template'
import {
  arbitraryJSBlock,
  isArbitraryJSBlock,
  modifiableAttributeIsAttributeOtherJavaScript,
  isJSXElement,
  isUtopiaJSXComponent,
  jsxArraySpread,
  jsxArrayValue,
  jsExpressionFunctionCall,
  jsExpressionNestedArray,
  jsxAttributeNestedArraySimple,
  jsExpressionNestedObject,
  jsxAttributeNestedObjectSimple,
  jsExpressionOtherJavaScript,
  jsExpressionValue,
  jsxElement,
  jsxPropertyAssignment,
  jsxSpreadAssignment,
  jsxTextBlock,
  utopiaJSXComponent,
  defaultPropsParam,
  clearArbitraryJSBlockUniqueIDs,
  jsxAttributesFromMap,
  getJSXAttributeForced,
  isJSXAttributesEntry,
  emptyComments,
  clearJSXElementChildUniqueIDs,
  clearJSXElementUniqueIDs,
  jsxMapExpression,
  jsPropertyAccess,
  jsIdentifier,
  jsElementAccess,
  modifiableAttributeIsJsxElement,
  jsOpaqueArbitraryStatement,
  jsAssignment,
  jsAssignmentStatement,
  functionParam,
  regularParam,
  simpleJSAssignmentStatement,
} from '../../shared/element-template'
import { sampleCode } from '../../model/new-project-files'
import { addImport, emptyImports } from '../common/project-file-utils'
import { onlyImportReact, sampleImportsForTests } from '../../model/test-ui-js-file.test-utils'
import type { ParseSuccess, ProjectContents } from '../../shared/project-file-types'
import {
  isParseSuccess,
  importAlias,
  foldParsedTextFile,
  EmptyExportsDetail,
  exportFunction,
  exportDefaultFunctionOrClass,
  exportVariables,
  exportVariable,
  parseSuccess,
  textFileContents,
  RevisionsState,
  textFile,
} from '../../shared/project-file-types'
import { lintAndParse, parseCode, printCode, printCodeOptions } from './parser-printer'
import { applyPrettier } from 'utopia-vscode-common'
import { transpileJavascriptFromCode } from './parser-printer-transpiling'
import {
  clearParseResultPassTimes,
  clearParseResultSourceMapsUniqueIDsAndEmptyBlocks,
  clearParseResultUniqueIDsAndEmptyBlocks,
  clearTopLevelElementUniqueIDsAndEmptyBlocks,
  elementsStructure,
  ensureArbitraryBlocksHaveUID,
  ensureElementsHaveUID,
  isWantedElement,
  JustImportViewAndReact,
  printedProjectContentArbitrary,
  simplifyParsedTextFileAttributes,
  testParseCode,
} from './parser-printer.test-utils'
import type { ArbitraryProject } from './parser-printer.test-utils'
import { InfiniteLoopError, InfiniteLoopMaxIterations } from './transform-prevent-infinite-loops'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../model/scene-utils'
import { optionalMap } from '../../shared/optional-utils'
import { StoryboardFilePath } from '../../../components/editor/store/editor-state'
import { emptySet, setsEqual } from '../../shared/set-utils'
import {
  BLOCK_RAN_TO_END_FUNCTION_NAME,
  EARLY_RETURN_RESULT_FUNCTION_NAME,
  EARLY_RETURN_VOID_FUNCTION_NAME,
  JSX_CANVAS_LOOKUP_FUNCTION_NAME,
} from '../../shared/dom-utils'
import { assertNever } from '../../../core/shared/utils'
import { contentsToTree } from '../../../components/assets'
import { getAllUniqueUids } from '../../../core/model/get-unique-ids'
import {
  filtered,
  fromField,
  fromTypeGuard,
  notNull,
  traverseArray,
} from '../../shared/optics/optic-creators'
import { toFirst } from '../../shared/optics/optic-utilities'

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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aab', emptyComments),
      style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
      left: jsPropertyAccess(
        jsElementAccess(
          jsPropertyAccess(
            jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
            'leftOfTheCake',
            '',
            expect.objectContaining({}),
            emptyComments,
            'props.leftOfTheCake',
            'not-optionally-chained',
          ),
          jsExpressionValue(0, emptyComments, ''),
          '',
          expect.objectContaining({}),
          emptyComments,
          'props.leftOfTheCake[0]',
          'not-optionally-chained',
        ),
        'hat',
        '',
        expect.objectContaining({}),
        emptyComments,
        'props.leftOfTheCake[0].hat',
        'not-optionally-chained',
      ),
      right: jsExpressionValue(20, emptyComments),
      top: jsExpressionValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
    const { imports } = addImport(
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aab', emptyComments),
      style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
      left: jsExpressionValue(20, emptyComments),
      right: jsExpressionValue(20, emptyComments),
      top: jsExpressionValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
    const { imports } = addImport(
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aab', emptyComments),
      style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
      left: jsPropertyAccess(
        jsElementAccess(
          jsPropertyAccess(
            jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
            'leftOfTheCake',
            '',
            expect.objectContaining({}),
            emptyComments,
            'props.leftOfTheCake',
            'not-optionally-chained',
          ),
          jsExpressionValue(0, emptyComments, ''),
          '',
          expect.objectContaining({}),
          emptyComments,
          'props.leftOfTheCake[0]',
          'not-optionally-chained',
        ),
        'hat',
        '',
        expect.objectContaining({}),
        emptyComments,
        'props.leftOfTheCake[0].hat',
        'not-optionally-chained',
      ),
      right: jsExpressionValue(20, emptyComments),
      top: jsExpressionValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
    const { imports } = addImport(
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aab', emptyComments),
      style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
      left: jsExpressionValue(20, emptyComments),
      right: jsExpressionValue(20, emptyComments),
      top: jsExpressionValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
    const { imports } = addImport(
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aab', emptyComments),
      style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
      left: jsPropertyAccess(
        jsElementAccess(
          jsPropertyAccess(
            jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
            'leftOfTheCake',
            '',
            expect.objectContaining({}),
            emptyComments,
            'props.leftOfTheCake',
            'not-optionally-chained',
          ),
          jsExpressionValue(0, emptyComments, ''),
          '',
          expect.objectContaining({}),
          emptyComments,
          'props.leftOfTheCake[0]',
          'not-optionally-chained',
        ),
        'hat',
        '',
        expect.objectContaining({}),
        emptyComments,
        'props.leftOfTheCake[0].hat',
        'not-optionally-chained',
      ),
      right: jsExpressionValue(20, emptyComments),
      top: jsExpressionValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
    const { imports } = addImport(
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
      [exportDefaultFunctionOrClass('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aab', emptyComments),
      style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
      left: jsExpressionValue(20, emptyComments),
      right: jsExpressionValue(20, emptyComments),
      top: jsExpressionValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
    const { imports } = addImport(
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
      [exportDefaultFunctionOrClass('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aab', emptyComments),
      style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
      left: jsPropertyAccess(
        jsElementAccess(
          jsPropertyAccess(
            jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
            'leftOfTheCake',
            '',
            expect.objectContaining({}),
            emptyComments,
            'props.leftOfTheCake',
            'not-optionally-chained',
          ),
          jsExpressionValue(0, emptyComments, ''),
          '',
          expect.objectContaining({}),
          emptyComments,
          'props.leftOfTheCake[0]',
          'not-optionally-chained',
        ),
        'hat',
        '',
        expect.objectContaining({}),
        emptyComments,
        'props.leftOfTheCake[0].hat',
        'not-optionally-chained',
      ),
      right: jsExpressionValue(20, emptyComments),
      top: jsExpressionValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
    const { imports: importsWithCake } = addImport(
      '/code.js',
      'cake',
      'cake',
      [],
      null,
      sampleImportsForTests,
    )
    const { imports: importsWithStylecss } = addImport(
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aab', emptyComments),
      style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
      left: jsPropertyAccess(
        jsElementAccess(
          jsPropertyAccess(
            jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
            'leftOfTheCake',
            '',
            expect.objectContaining({}),
            emptyComments,
            'props.leftOfTheCake',
            'not-optionally-chained',
          ),
          jsExpressionValue(0, emptyComments, ''),
          '',
          expect.objectContaining({}),
          emptyComments,
          'props.leftOfTheCake[0]',
          'not-optionally-chained',
        ),
        'hat',
        '',
        expect.objectContaining({}),
        emptyComments,
        'props.leftOfTheCake[0].hat',
        'not-optionally-chained',
      ),
      right: jsExpressionValue(20, emptyComments),
      top: jsExpressionValue(-20, emptyComments),
    })
    const cake2Attributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aac', emptyComments),
      style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
      left: jsPropertyAccess(
        jsElementAccess(
          jsPropertyAccess(
            jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
            'leftOfTheCake',
            '',
            expect.objectContaining({}),
            emptyComments,
            'props.leftOfTheCake',
            'not-optionally-chained',
          ),
          jsExpressionValue(0, emptyComments, ''),
          '',
          expect.objectContaining({}),
          emptyComments,
          'props.leftOfTheCake[0]',
          'not-optionally-chained',
        ),
        'hat',
        '',
        expect.objectContaining({}),
        emptyComments,
        'props.leftOfTheCake[0].hat',
        'not-optionally-chained',
      ),
      right: jsExpressionValue(20, emptyComments),
      top: jsExpressionValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const cake2 = jsxElement('cake2', 'aac', cake2Attributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
    const { imports } = addImport(
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
      const actualResult = simplifyParsedTextFileAttributes(
        clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
      )
      const cake2Attributes: JSXAttributes = jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aac', emptyComments),
        style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
        left: jsPropertyAccess(
          jsElementAccess(
            jsPropertyAccess(
              jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
              'leftOfTheCake',
              '',
              expect.objectContaining({}),
              emptyComments,
              'props.leftOfTheCake',
              'not-optionally-chained',
            ),
            jsExpressionValue(0, emptyComments, ''),
            '',
            expect.objectContaining({}),
            emptyComments,
            'props.leftOfTheCake[0]',
            'not-optionally-chained',
          ),
          'hat',
          '',
          expect.objectContaining({}),
          emptyComments,
          'props.leftOfTheCake[0].hat',
          'not-optionally-chained',
        ),
        right: jsExpressionValue(20, emptyComments),
        top: jsExpressionValue(-20, emptyComments),
      })
      const cake2 = jsxElement('cake2', 'aac', cake2Attributes, [])
      const view = jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
      const { imports } = addImport(
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
        [exportFunction('whatever')],
        expect.objectContaining({}),
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
      const actualResult = simplifyParsedTextFileAttributes(
        clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
      )
      const firstCakeAttributes: JSXAttributes = jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aab', emptyComments),
        'data-label': jsExpressionValue('First cake', emptyComments),
        style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
        left: jsPropertyAccess(
          jsElementAccess(
            jsPropertyAccess(
              jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
              'leftOfTheCake',
              '',
              expect.objectContaining({}),
              emptyComments,
              'props.leftOfTheCake',
              'not-optionally-chained',
            ),
            jsExpressionValue(0, emptyComments, ''),
            '',
            expect.objectContaining({}),
            emptyComments,
            'props.leftOfTheCake[0]',
            'not-optionally-chained',
          ),
          'hat',
          '',
          expect.objectContaining({}),
          emptyComments,
          'props.leftOfTheCake[0].hat',
          'not-optionally-chained',
        ),
        right: jsExpressionValue(20, emptyComments),
        top: jsExpressionValue(-20, emptyComments),
      })
      const firstCake = jsxElement('cake', 'aab', firstCakeAttributes, [])
      const secondCakeAttributes: JSXAttributes = jsxAttributesFromMap({
        'data-uid': jsExpressionValue('111', emptyComments),
        'data-label': jsExpressionValue('Second cake', emptyComments),
        style: jsExpressionValue({ backgroundColor: 'blue' }, emptyComments),
        left: jsPropertyAccess(
          jsElementAccess(
            jsPropertyAccess(
              jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
              'rightOfTheCake',
              '',
              expect.objectContaining({}),
              emptyComments,
              'props.rightOfTheCake',
              'not-optionally-chained',
            ),
            jsExpressionValue(0, emptyComments, ''),
            '',
            expect.objectContaining({}),
            emptyComments,
            'props.rightOfTheCake[0]',
            'not-optionally-chained',
          ),
          'hat',
          '',
          expect.objectContaining({}),
          emptyComments,
          'props.rightOfTheCake[0].hat',
          'not-optionally-chained',
        ),
        right: jsExpressionValue(10, emptyComments),
        top: jsExpressionValue(-10, emptyComments),
      })
      const secondCake = jsxElement('cake', '111', secondCakeAttributes, [])
      const view = jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
      const { imports } = addImport(
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
        [exportFunction('whatever')],
        expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aab', emptyComments),
      style: jsExpressionValue(
        {
          backgroundColor: 'red',
        },
        emptyComments,
      ),
      left: jsPropertyAccess(
        jsElementAccess(
          jsPropertyAccess(
            jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
            'leftOfTheCake',
            '',
            expect.objectContaining({}),
            emptyComments,
            'props.leftOfTheCake',
            'not-optionally-chained',
          ),
          jsExpressionValue(0, emptyComments, ''),
          '',
          expect.objectContaining({}),
          emptyComments,
          'props.leftOfTheCake[0]',
          'not-optionally-chained',
        ),
        'hat',
        '',
        expect.objectContaining({}),
        emptyComments,
        'props.leftOfTheCake[0].hat',
        'not-optionally-chained',
      ),
      right: jsExpressionValue(20, emptyComments),
      top: jsExpressionValue(-20, emptyComments),
      nullProp: jsExpressionValue(null, emptyComments),
      undefinedProp: jsExpressionValue(undefined, emptyComments),
      trueProp: jsExpressionValue(true, emptyComments),
      falseProp: jsExpressionValue(false, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
    const { imports } = addImport(
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
  <cake data-uid='aab' style={{backgroundColor: 'red'}} left={getSizing(spacing)} right={20} top={-20} onClick={function click(){console.log('click')}} />
</View>
`
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aab', emptyComments),
      style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
      left: jsExpressionOtherJavaScript(
        [],
        'getSizing(spacing)',
        'getSizing(spacing);',
        'return getSizing(spacing);',
        ['getSizing', 'spacing'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
        emptyComments,
      ),
      right: jsExpressionValue(20, emptyComments),
      top: jsExpressionValue(-20, emptyComments),
      onClick: jsExpressionOtherJavaScript(
        [],
        `function click(){console.log('click')}`,
        `(function click() {console.log('click');});`,
        `return (function click() {
  console.log('click');
});`,
        ['console'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
        emptyComments,
      ),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
    const jsCode1 = `
function getSizing(n) {
  return 100 + n
}`
    const transpiledJsCode1 = `return (() => {
  function getSizing(n) {
    return 100 + n;
  }

  return utopiaCanvasBlockRanToEnd({
    getSizing: getSizing
  });
})();`
    const arbitraryBlock1 = arbitraryJSBlock(
      [],
      jsCode1,
      transpiledJsCode1,
      ['getSizing'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [jsOpaqueArbitraryStatement(jsCode1.trim(), ['getSizing'], [], '')],
    )
    const jsCode2 = `var spacing = 20`
    const transpiledJsCode2 = `return (() => {
  var spacing = 20;
  return utopiaCanvasBlockRanToEnd({
    spacing: spacing
  });
})();`
    const arbitraryBlock2 = arbitraryJSBlock(
      [],
      jsCode2,
      transpiledJsCode2,
      ['spacing'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [simpleJSAssignmentStatement('var', 'spacing', 20)],
    )
    const combinedJsCode = `function getSizing(n) {
  return 100 + n
}
var spacing = 20`
    const transpiledCombinedJsCode = `return (() => {
  function getSizing(n) {
    return 100 + n;
  }

  var spacing = 20;
  return utopiaCanvasBlockRanToEnd({
    getSizing: getSizing,
    spacing: spacing
  });
})();`
    const combinedArbitraryBlock = arbitraryJSBlock(
      [],
      combinedJsCode,
      transpiledCombinedJsCode,
      ['getSizing', 'spacing'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [
        jsOpaqueArbitraryStatement(jsCode1, ['getSizing'], [], ''),
        simpleJSAssignmentStatement('var', 'spacing', 20),
      ],
    )
    const topLevelElements = [arbitraryBlock1, arbitraryBlock2, exported].map(
      clearTopLevelElementUniqueIDsAndEmptyBlocks,
    )
    const { imports } = addImport(
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aab', emptyComments),
      style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
      left: jsPropertyAccess(
        jsElementAccess(
          jsPropertyAccess(
            jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
            'leftOfTheCake',
            '',
            expect.objectContaining({}),
            emptyComments,
            'props.leftOfTheCake',
            'not-optionally-chained',
          ),
          jsExpressionValue(0, emptyComments, ''),
          '',
          expect.objectContaining({}),
          emptyComments,
          'props.leftOfTheCake[0]',
          'not-optionally-chained',
        ),
        'hat',
        '',
        expect.objectContaining({}),
        emptyComments,
        'props.leftOfTheCake[0].hat',
        'not-optionally-chained',
      ),
      right: jsExpressionValue(20, emptyComments),
      top: jsExpressionValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
    const jsCode = `
export default function getSizing(n) {
  return 100 + n
}`
    const transpiledJsCode = `return (() => {
  function getSizing(n) {
    return 100 + n;
  }

  return utopiaCanvasBlockRanToEnd({
    getSizing: getSizing
  });
})();`
    const arbitraryBlockCombinedBlock = arbitraryJSBlock(
      [],
      jsCode.trim(),
      transpiledJsCode,
      ['getSizing'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [jsOpaqueArbitraryStatement(jsCode, ['getSizing'], [], '')],
    )
    const arbitraryBlockTopLevelElement = arbitraryJSBlock(
      [],
      jsCode.trim(),
      transpiledJsCode,
      ['getSizing'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [jsOpaqueArbitraryStatement(jsCode.trim(), ['getSizing'], [], '')],
    )
    const topLevelElements = [arbitraryBlockTopLevelElement, exported].map(
      clearTopLevelElementUniqueIDsAndEmptyBlocks,
    )
    const { imports } = addImport(
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
      clearArbitraryJSBlockUniqueIDs(arbitraryBlockCombinedBlock),
      [exportDefaultFunctionOrClass('getSizing'), exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aab', emptyComments),
      style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
      left: jsPropertyAccess(
        jsElementAccess(
          jsPropertyAccess(
            jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
            'leftOfTheCake',
            '',
            expect.objectContaining({}),
            emptyComments,
            'props.leftOfTheCake',
            'not-optionally-chained',
          ),
          jsExpressionValue(0, emptyComments, ''),
          '',
          expect.objectContaining({}),
          emptyComments,
          'props.leftOfTheCake[0]',
          'not-optionally-chained',
        ),
        'hat',
        '',
        expect.objectContaining({}),
        emptyComments,
        'props.leftOfTheCake[0].hat',
        'not-optionally-chained',
      ),
      right: jsExpressionValue(20, emptyComments),
      top: jsExpressionValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
    const jsCode = `
export function getSizing(n) {
  switch (n) {
    case 100:
      return 1
    default:
      return 100 + n
  }
}`
    const transpiledJsCode = `return (() => {
  function getSizing(n) {
    switch (n) {
      case 100:
        return 1;

      default:
        return 100 + n;
    }
  }

  return utopiaCanvasBlockRanToEnd({
    getSizing: getSizing
  });
})();`
    const arbitraryBlockTopLevelElement = arbitraryJSBlock(
      [],
      jsCode.trim(),
      transpiledJsCode,
      ['getSizing'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [jsOpaqueArbitraryStatement(jsCode.trim(), ['getSizing'], [], '')],
    )
    const arbitraryBlockCombinedBlock = arbitraryJSBlock(
      [],
      jsCode.trim(),
      transpiledJsCode,
      ['getSizing'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [jsOpaqueArbitraryStatement(jsCode, ['getSizing'], [], '')],
    )
    const topLevelElements = [arbitraryBlockTopLevelElement, exported].map(
      clearTopLevelElementUniqueIDsAndEmptyBlocks,
    )
    const { imports } = addImport(
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
      clearArbitraryJSBlockUniqueIDs(arbitraryBlockCombinedBlock),
      [exportFunction('getSizing'), exportFunction('whatever')],
      expect.objectContaining({}),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  // Temporarily disabled, as it appears these weren't being handled correctly currently.
  // Appears they'll need capturing in a slightly different way to the existing values
  // because of their lack of identifying name, which is what the logic currently relies on.
  xit('parses the code when it has an export default anonymous function', () => {
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aab', emptyComments),
      style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
      left: jsPropertyAccess(
        jsElementAccess(
          jsPropertyAccess(
            jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
            'leftOfTheCake',
            '',
            expect.objectContaining({}),
            emptyComments,
            'props.leftOfTheCake',
            'not-optionally-chained',
          ),
          jsExpressionValue(0, emptyComments, ''),
          '',
          expect.objectContaining({}),
          emptyComments,
          'props.leftOfTheCake[0]',
          'not-optionally-chained',
        ),
        'hat',
        '',
        expect.objectContaining({}),
        emptyComments,
        'props.leftOfTheCake[0].hat',
        'not-optionally-chained',
      ),
      right: jsExpressionValue(20, emptyComments),
      top: jsExpressionValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
    const transpiledJsCode = `n => {
  return 100 + n;
};
return {  };`
    const arbitraryBlock = arbitraryJSBlock(
      [],
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
      [],
    )
    const topLevelElements = [arbitraryBlock, exported].map(
      clearTopLevelElementUniqueIDsAndEmptyBlocks,
    )
    const { imports } = addImport(
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
      [exportDefaultFunctionOrClass(null), exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aab', emptyComments),
      style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
      left: jsIdentifier('spacing', '', expect.objectContaining({}), emptyComments),
      right: jsExpressionValue(20, emptyComments),
      top: jsExpressionValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
    const transpiledJSCode = `return (() => {
  var spacing = 20;
  return utopiaCanvasBlockRanToEnd({
    spacing: spacing
  });
})();`
    const jsVariable = arbitraryJSBlock(
      [],
      'var spacing = 20',
      transpiledJSCode,
      ['spacing'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [simpleJSAssignmentStatement('var', 'spacing', 20)],
    )
    const topLevelElements = [jsVariable, exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)
    const { imports } = addImport(
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const viewAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aaa', emptyComments),
      style: jsxAttributeNestedObjectSimple(
        jsxAttributesFromMap({
          backgroundColor: jsElementAccess(
            jsIdentifier('bgs', '', expect.objectContaining({}), emptyComments),
            jsExpressionValue(0, emptyComments, ''),
            '',
            expect.objectContaining({}),
            emptyComments,
            'bgs[0]',
            'not-optionally-chained',
          ),
        }),
        emptyComments,
      ),
    })
    const view = jsxElement('View', 'aaa', viewAttributes, [])
    const jsCode = `const bgs = ['black', 'grey']
  const bg = bgs[0]`
    const transpiledJsCode = `return (() => {
  const bgs = ['black', 'grey'];
  const bg = bgs[0];
  return utopiaCanvasBlockRanToEnd({
    bgs: bgs,
    bg: bg
  });
})();`
    const arbitraryBlock = arbitraryJSBlock(
      [],
      jsCode,
      transpiledJsCode,
      ['bgs', 'bg'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [
        jsAssignmentStatement(
          'const',
          [
            jsAssignment(
              regularParam(
                'bgs',
                jsExpressionNestedArray(
                  [
                    jsxArrayValue(jsExpressionValue('black', emptyComments, ''), emptyComments),
                    jsxArrayValue(jsExpressionValue('grey', emptyComments, ''), emptyComments),
                  ],
                  emptyComments,
                  '',
                ),
              ),
              jsExpressionNestedArray(
                [
                  jsxArrayValue(jsExpressionValue('black', emptyComments, ''), emptyComments),
                  jsxArrayValue(jsExpressionValue('grey', emptyComments, ''), emptyComments),
                ],
                emptyComments,
                '',
              ),
            ),
          ],
          expect.objectContaining({}),
        ),
        jsAssignmentStatement(
          'const',
          [
            jsAssignment(
              regularParam(
                'bg',
                jsElementAccess(
                  jsIdentifier('bgs', '', expect.objectContaining({}), emptyComments),
                  jsExpressionValue(0, emptyComments, ''),
                  '',
                  expect.objectContaining({}),
                  emptyComments,
                  'bgs[0]',
                  'not-optionally-chained',
                ),
              ),
              jsElementAccess(
                jsIdentifier('bgs', '', expect.objectContaining({}), emptyComments),
                jsExpressionValue(0, emptyComments, ''),
                '',
                expect.objectContaining({}),
                emptyComments,
                'bgs[0]',
                'not-optionally-chained',
              ),
            ),
          ],
          expect.objectContaining({}),
        ),
      ],
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const viewAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aaa', emptyComments),
      colors: jsExpressionNestedArray(
        [
          jsxArrayValue(jsExpressionValue('black', emptyComments), emptyComments),
          jsxArraySpread(
            jsIdentifier('greys', '', expect.objectContaining({}), emptyComments),
            emptyComments,
          ),
        ],
        emptyComments,
      ),
    })
    const view = jsxElement('View', 'aaa', viewAttributes, [])
    const jsCode = `const greys = ['lightGrey', 'grey']`
    const transpiledJsCode = `return (() => {
  const greys = ['lightGrey', 'grey'];
  return utopiaCanvasBlockRanToEnd({
    greys: greys
  });
})();`
    const arbitraryBlock = arbitraryJSBlock(
      [],
      jsCode,
      transpiledJsCode,
      ['greys'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [
        jsAssignmentStatement(
          'const',
          [
            jsAssignment(
              regularParam(
                'greys',
                jsExpressionNestedArray(
                  [
                    jsxArrayValue(jsExpressionValue('lightGrey', emptyComments, ''), emptyComments),
                    jsxArrayValue(jsExpressionValue('grey', emptyComments, ''), emptyComments),
                  ],
                  emptyComments,
                  '',
                ),
              ),
              jsExpressionNestedArray(
                [
                  jsxArrayValue(jsExpressionValue('lightGrey', emptyComments, ''), emptyComments),
                  jsxArrayValue(jsExpressionValue('grey', emptyComments, ''), emptyComments),
                ],
                emptyComments,
                '',
              ),
            ),
          ],
          expect.objectContaining({}),
        ),
      ],
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const viewAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aaa', emptyComments),
      left: jsExpressionOtherJavaScript(
        [],
        'a + b',
        'a + b;',
        'return a + b;',
        ['a', 'b'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
        emptyComments,
      ),
    })
    const view = jsxElement('View', 'aaa', viewAttributes, [])
    const jsCode = `const a = 10
  const b = 20`
    const transpiledJsCode = `return (() => {
  const a = 10;
  const b = 20;
  return utopiaCanvasBlockRanToEnd({
    a: a,
    b: b
  });
})();`
    const arbitraryBlock = arbitraryJSBlock(
      [],
      jsCode,
      transpiledJsCode,
      ['a', 'b'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [
        simpleJSAssignmentStatement('const', 'a', 10),
        simpleJSAssignmentStatement('const', 'b', 20),
      ],
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const viewAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aaa', emptyComments),
      left: jsExpressionOtherJavaScript(
        [],
        'a ? b : c',
        'a ? b : c;',
        'return a ? b : c;',
        ['a', 'b', 'c'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
        emptyComments,
      ),
    })
    const view = jsxElement('View', 'aaa', viewAttributes, [])
    const jsCode = `const a = true
  const b = 10
  const c = 20`
    const transpiledJsCode = `return (() => {
  const a = true;
  const b = 10;
  const c = 20;
  return utopiaCanvasBlockRanToEnd({
    a: a,
    b: b,
    c: c
  });
})();`
    const arbitraryBlock = arbitraryJSBlock(
      [],
      jsCode,
      transpiledJsCode,
      ['a', 'b', 'c'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [
        simpleJSAssignmentStatement('const', 'a', true),
        simpleJSAssignmentStatement('const', 'b', 10),
        simpleJSAssignmentStatement('const', 'c', 20),
      ],
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const viewAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aaa', emptyComments),
      left: jsExpressionOtherJavaScript(
        [],
        'a++',
        'a++;',
        'return a++;',
        ['a'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
        emptyComments,
      ),
      right: jsExpressionOtherJavaScript(
        [],
        '++a',
        '++a;',
        'return ++a;',
        ['a'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
        emptyComments,
      ),
    })
    const view = jsxElement('View', 'aaa', viewAttributes, [])
    const jsCode = `let a = 10`
    const transpiledJsCode = `return (() => {
  let a = 10;
  return utopiaCanvasBlockRanToEnd({
    a: a
  });
})();`
    const arbitraryBlock = arbitraryJSBlock(
      [],
      jsCode,
      transpiledJsCode,
      ['a'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [simpleJSAssignmentStatement('let', 'a', 10)],
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const viewAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aaa', emptyComments),
      left: jsPropertyAccess(
        jsIdentifier('b', '', expect.objectContaining({}), emptyComments),
        'a',
        '',
        expect.objectContaining({}),
        emptyComments,
        'b.a',
        'not-optionally-chained',
      ),
    })
    const view = jsxElement('View', 'aaa', viewAttributes, [])
    const jsCode = `const a = 10
  const b = { a: a }`
    const transpiledJsCode = `return (() => {
  const a = 10;
  const b = {
    a: a
  };
  return utopiaCanvasBlockRanToEnd({
    a: a,
    b: b
  });
})();`
    const arbitraryBlock = arbitraryJSBlock(
      [],
      jsCode,
      transpiledJsCode,
      ['a', 'b'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [
        simpleJSAssignmentStatement('const', 'a', 10),
        jsAssignmentStatement(
          'const',
          [
            jsAssignment(
              regularParam(
                'b',
                jsExpressionNestedObject(
                  [
                    jsxPropertyAssignment(
                      'a',
                      jsIdentifier('a', '', expect.objectContaining({}), emptyComments),
                      emptyComments,
                      emptyComments,
                    ),
                  ],
                  emptyComments,
                  '',
                ),
              ),
              jsExpressionNestedObject(
                [
                  jsxPropertyAssignment(
                    'a',
                    jsIdentifier('a', '', expect.objectContaining({}), emptyComments),
                    emptyComments,
                    emptyComments,
                  ),
                ],
                emptyComments,
                '',
              ),
            ),
          ],
          '',
        ),
      ],
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses object literal initializers', () => {
    const code = `import * as React from 'react'
    import { Scene, Storyboard } from 'utopia-api'
    
    function showMeWhatYouGot(obj) {
      return obj.t
    }
    
    const App = (props) => {
      const text = 'I got this!'
      const t = text
      const obj = { t: text }
    
      return (
        <div style={props.style}>
          {showMeWhatYouGot(obj)}
          <br />
          {showMeWhatYouGot({ t })}
          <br />
          {showMeWhatYouGot({ t: text })}
        </div>
      )
    }
    
    export var storyboard = (
      <Storyboard data-uid='0cd'>
        <App
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 178,
            top: 131,
            width: 443,
            height: 451,
          }}
        />
      </Storyboard>
    )
    `

    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    expect(actualResult.type).toEqual('PARSE_SUCCESS')
    const jsxComponent = (actualResult as ParseSuccess).topLevelElements.find(
      (e): e is UtopiaJSXComponent => e.type === 'UTOPIA_JSX_COMPONENT',
    )!
    const definedElseWhere = (jsxComponent.rootElement as JSXElement).children.flatMap((c) =>
      c.type === 'ATTRIBUTE_OTHER_JAVASCRIPT' ? c.definedElsewhere : [],
    )
    expect(definedElseWhere).toEqual([
      'showMeWhatYouGot',
      'obj',
      'showMeWhatYouGot',
      't',
      'showMeWhatYouGot',
      'text',
    ])
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const viewAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aaa', emptyComments),
      style: jsExpressionNestedObject(
        [
          jsxSpreadAssignment(
            jsIdentifier('bg', '', expect.objectContaining({}), emptyComments),
            emptyComments,
          ),
        ],
        emptyComments,
      ),
    })
    const view = jsxElement('View', 'aaa', viewAttributes, [])
    const jsCode = `const bg = { backgroundColor: 'grey' }`
    const transpiledJsCode = `return (() => {
  const bg = {
    backgroundColor: 'grey'
  };
  return utopiaCanvasBlockRanToEnd({
    bg: bg
  });
})();`
    const arbitraryBlock = arbitraryJSBlock(
      [],
      jsCode,
      transpiledJsCode,
      ['bg'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [
        jsAssignmentStatement(
          'const',
          [
            jsAssignment(
              regularParam(
                'bg',
                jsExpressionNestedObject(
                  [
                    jsxPropertyAssignment(
                      'backgroundColor',
                      jsExpressionValue('grey', emptyComments, ''),
                      emptyComments,
                      emptyComments,
                    ),
                  ],
                  emptyComments,
                  '',
                ),
              ),
              jsExpressionNestedObject(
                [
                  jsxPropertyAssignment(
                    'backgroundColor',
                    jsExpressionValue('grey', emptyComments, ''),
                    emptyComments,
                    emptyComments,
                  ),
                ],
                emptyComments,
                '',
              ),
            ),
          ],
          '',
        ),
      ],
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aab', emptyComments),
      style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
      text: jsExpressionOtherJavaScript(
        [],
        '`Count ${count}`',
        '`Count ${count}`;',
        'return `Count ${count}`;',
        ['count'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
        emptyComments,
      ),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
    const transpiledJSCode = `return (() => {
  var count = 10;
  return utopiaCanvasBlockRanToEnd({
    count: count
  });
})();`
    const jsVariable = arbitraryJSBlock(
      [],
      jsCode,
      transpiledJSCode,
      ['count'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [simpleJSAssignmentStatement('var', 'count', 10)],
    )
    const topLevelElements = [jsVariable, exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)
    const { imports } = addImport(
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aab', emptyComments),
      style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
      left: jsExpressionOtherJavaScript(
        [],
        'use20 ? 20 : 10',
        'use20 ? 20 : 10;',
        'return use20 ? 20 : 10;',
        ['use20'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
        emptyComments,
      ),
      right: jsExpressionValue(20, emptyComments),
      top: jsExpressionValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
    const transpiledJSCode = `return (() => {
  var use20 = true;
  return utopiaCanvasBlockRanToEnd({
    use20: use20
  });
})();`
    const jsVariable = arbitraryJSBlock(
      [],
      'var use20 = true',
      transpiledJSCode,
      ['use20'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [simpleJSAssignmentStatement('var', 'use20', true)],
    )
    const topLevelElements = [jsVariable, exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)
    const { imports } = addImport(
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
    const transpiledJSCode = `return (() => {
  var mySet = new Set();
  return utopiaCanvasBlockRanToEnd({
    mySet: mySet
  });
})();`
    const jsVariable = arbitraryJSBlock(
      [],
      'var mySet = new Set()',
      transpiledJSCode,
      ['mySet'],
      [
        'Set',
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [
        jsAssignmentStatement(
          'var',
          [
            jsAssignment(
              regularParam(
                'mySet',
                jsExpressionOtherJavaScript(
                  [],
                  expect.stringContaining(''),
                  expect.stringContaining(''),
                  expect.stringContaining(''),
                  ['Set'],
                  expect.objectContaining({}),
                  {},
                  emptyComments,
                ),
              ),
              jsExpressionOtherJavaScript(
                [],
                expect.stringContaining(''),
                expect.stringContaining(''),
                expect.stringContaining(''),
                ['Set'],
                expect.objectContaining({}),
                {},
                emptyComments,
              ),
            ),
          ],
          '',
        ),
      ],
    )
    const topLevelElements = [jsVariable, exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)
    const expectedResult = parseSuccess(
      sampleImportsForTests,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(jsVariable),
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aab', emptyComments),
      style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
      left: jsExpressionOtherJavaScript(
        [],
        'props.left + spacing',
        'props.left + spacing;',
        'return props.left + spacing;',
        ['spacing', 'props'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
        emptyComments,
      ),
      right: jsExpressionValue(20, emptyComments),
      top: jsExpressionValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'aab', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
    const transpiledJSCode = `return (() => {
  var spacing = 20;
  return utopiaCanvasBlockRanToEnd({
    spacing: spacing
  });
})();`
    const jsVariable = arbitraryJSBlock(
      [],
      'var spacing = 20',
      transpiledJSCode,
      ['spacing'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [
        jsAssignmentStatement(
          'var',
          [
            jsAssignment(
              regularParam('spacing', jsExpressionValue(20, emptyComments)),
              jsExpressionValue(20, emptyComments),
            ),
          ],
          '',
        ),
      ],
    )
    const topLevelElements = [jsVariable, exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)
    const { imports } = addImport(
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
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
    const transpiledJsCode = `return (() => {
  var MyComp = props => {
    return React.createElement("div", {
      style: {
        position: "absolute",
        left: props.layout.left,
        backgroundColor: "hotpink"
      }
    }, "hello");
  };

  return utopiaCanvasBlockRanToEnd({
    MyComp: MyComp
  });
})();`
    const definedElseWhere = [
      'React',
      JSX_CANVAS_LOOKUP_FUNCTION_NAME,
      BLOCK_RAN_TO_END_FUNCTION_NAME,
      EARLY_RETURN_RESULT_FUNCTION_NAME,
      EARLY_RETURN_VOID_FUNCTION_NAME,
    ]
    const myCompContent = jsExpressionOtherJavaScript(
      [functionParam(false, regularParam('props', null))],
      expect.stringContaining(''),
      expect.stringContaining(''),
      expect.stringContaining(''),
      ['React'],
      expect.objectContaining({}),
      {},
      emptyComments,
      '',
    )

    const MyComp = arbitraryJSBlock(
      [],
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
      [
        jsAssignmentStatement(
          'var',
          [jsAssignment(regularParam('MyComp', myCompContent), myCompContent)],
          '',
        ),
      ],
    )
    const myCompAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aab', emptyComments),
      layout: jsExpressionValue({ left: 100 }, emptyComments),
    })
    const myCompElement = jsxElement('MyComp', 'aab', myCompAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )

    const rootDivAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('abc', emptyComments),
      style: jsExpressionNestedObject(
        [
          jsxPropertyAssignment(
            'position',
            jsExpressionValue('absolute', emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'left',
            jsPropertyAccess(
              jsPropertyAccess(
                jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
                'layout',
                '',
                expect.objectContaining({}),
                emptyComments,
                'props.layout',
                'not-optionally-chained',
              ),
              'left',
              '',
              expect.objectContaining({}),
              emptyComments,
              'props.layout.left',
              'not-optionally-chained',
            ),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'top',
            jsPropertyAccess(
              jsPropertyAccess(
                jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
                'layout',
                '',
                expect.objectContaining({}),
                emptyComments,
                'props.layout',
                'not-optionally-chained',
              ),
              'top',
              '',
              expect.objectContaining({}),
              emptyComments,
              'props.layout.top',
              'not-optionally-chained',
            ),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'width',
            jsExpressionValue(100, emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'height',
            jsExpressionValue(100, emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'backgroundColor',
            jsExpressionValue('hotpink', emptyComments),
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
      'data-uid': jsExpressionValue('aab', emptyComments),
      layout: jsExpressionValue({ left: 100 }, emptyComments),
    })
    const myCompElement = jsxElement('MyComp', 'aab', myCompAttributes, [])
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = parseCode('code.tsx', code, null, emptySet(), 'do-not-apply-steganography')
    if (isParseSuccess(actualResult)) {
      expect(actualResult.topLevelElements.filter(isArbitraryJSBlock).length).toEqual(1)
      expect(actualResult.topLevelElements.filter(isUtopiaJSXComponent).length).toEqual(0)
    } else {
      throw new Error('Parse result is not a success.')
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
            throw new Error(`First child is not an element ${child}`)
          }
        } else {
          throw new Error(
            `Unexpected number of children returned: ${result.rootElement.children.length}`,
          )
        }
      } else {
        throw new Error(`Root element is not an element ${result.rootElement}`)
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const cake = clearJSXElementChildUniqueIDs(
      jsxElement(
        'cake',
        'aab',
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue('aab', emptyComments),
          style: jsxAttributeNestedObjectSimple(
            jsxAttributesFromMap({
              backgroundColor: jsExpressionValue('red', emptyComments),
              color: jsxAttributeNestedArraySimple([
                jsPropertyAccess(
                  jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
                  'color',
                  '',
                  expect.objectContaining({}),
                  emptyComments,
                  'props.color',
                  'not-optionally-chained',
                ),
                jsExpressionValue(-200, emptyComments),
              ]),
            }),
            emptyComments,
          ),
        }),
        [],
      ),
    )
    const view = clearJSXElementChildUniqueIDs(
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
        [cake],
      ),
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
    const { imports } = addImport(
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it is a var 2', () => {
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const cake = clearJSXElementChildUniqueIDs(
      jsxElement(
        'cake',
        'aab',
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue('aab', emptyComments),
          style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
        }),
        [],
      ),
    )
    const view = clearJSXElementChildUniqueIDs(
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
        [cake],
      ),
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
    const { imports } = addImport(
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const emptyBrackets = {
      ...jsExpressionOtherJavaScript([], '', '', 'return undefined', [], null, {}, emptyComments),
      uid: expect.any(String),
    }
    const view = clearJSXElementChildUniqueIDs(
      jsxElement(
        'View',
        'bbb',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('bbb', emptyComments) }),
        [emptyBrackets],
      ),
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
      [exportFunction('App')],
      expect.objectContaining({}),
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
    expect(
      simplifyParsedTextFileAttributes(clearParseResultUniqueIDsAndEmptyBlocks(actualResult)),
    ).toMatchSnapshot()
  })

  it('parses back and forth as a var', () => {
    const cake = clearJSXElementChildUniqueIDs(
      jsxElement(
        'cake',
        'aab',
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue('aab', emptyComments),
          style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
          left: jsExpressionValue(10, emptyComments),
          name: jsExpressionValue('test', emptyComments),
        }),
        [],
      ),
    )
    const view = clearJSXElementChildUniqueIDs(
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
        [cake],
      ),
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
    const { imports } = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const detailOfExports = [exportFunction('whatever')]
    const printedCode = printCode(
      '/index.js',
      printCodeOptions(false, true, true),
      imports,
      [exported],
      null,
      detailOfExports,
    )
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(printedCode)),
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported]),
      expect.objectContaining({}),
      null,
      null,
      detailOfExports,
      expect.objectContaining({}),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses back and forth as a var, with some arbitrary javascript', () => {
    const cakeAttributes: JSXAttributes = jsxAttributesFromMap({
      'data-uid': jsExpressionValue('cake', emptyComments),
      style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
      left: jsExpressionOtherJavaScript(
        [],
        'getSizing(spacing)',
        'getSizing(spacing);',
        'return getSizing(spacing);',
        ['getSizing', 'spacing'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {},
        emptyComments,
      ),
      right: jsExpressionValue(20, emptyComments),
      top: jsExpressionValue(-20, emptyComments),
    })
    const cake = jsxElement('cake', 'cake', cakeAttributes, [])
    const view = jsxElement(
      'View',
      'view',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('view', emptyComments) }),
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
    const jsCode1 = `
function getSizing(n) {
  return 100 + n
}`
    const transpiledJSCode1 = `return (() => {
  function getSizing(n) {
    return 100 + n;
  }

  return utopiaCanvasBlockRanToEnd({
    getSizing: getSizing
  });
})();`
    const arbitraryBlock1 = arbitraryJSBlock(
      [],
      jsCode1,
      transpiledJSCode1,
      ['getSizing'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [jsOpaqueArbitraryStatement(jsCode1.trim(), ['getSizing'], [], '')],
    )
    const jsCode2 = `var spacing = 20`
    const transpiledJSCode2 = `return (() => {
  var spacing = 20;
  return utopiaCanvasBlockRanToEnd({
    spacing: spacing
  });
})();`
    const arbitraryBlock2 = arbitraryJSBlock(
      [],
      jsCode2,
      transpiledJSCode2,
      ['spacing'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [
        jsAssignmentStatement(
          'var',
          [
            jsAssignment(
              regularParam('spacing', jsExpressionValue(20, emptyComments)),
              jsExpressionValue(20, emptyComments),
            ),
          ],
          '',
        ),
      ],
    )
    const combinedJSCode = `function getSizing(n) {
  return 100 + n
}
var spacing = 20`
    const transpiledCombinedJSCode = `return (() => {
  function getSizing(n) {
    return 100 + n;
  }

  var spacing = 20;
  return utopiaCanvasBlockRanToEnd({
    getSizing: getSizing,
    spacing: spacing
  });
})();`
    const combinedArbitraryBlock = arbitraryJSBlock(
      [],
      combinedJSCode,
      transpiledCombinedJSCode,
      ['getSizing', 'spacing'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [
        jsOpaqueArbitraryStatement(jsCode1, ['getSizing'], [], ''),
        jsAssignmentStatement(
          'var',
          [
            jsAssignment(
              regularParam('spacing', jsExpressionValue(20, emptyComments)),
              jsExpressionValue(20, emptyComments),
            ),
          ],
          '',
        ),
      ],
    )
    const topLevelElements = [arbitraryBlock1, arbitraryBlock2, exported].map(
      clearTopLevelElementUniqueIDsAndEmptyBlocks,
    )
    const { imports } = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const detailOfExports = [exportFunction('whatever')]
    const printedCode = printCode(
      '/index.js',
      printCodeOptions(false, true, true, false, true),
      imports,
      [...topLevelElements],
      null,
      detailOfExports,
    )
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(printedCode)),
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(combinedArbitraryBlock),
      detailOfExports,
      expect.objectContaining({}),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses back and forth as a var and includes canvas metadata', () => {
    const cake = clearJSXElementChildUniqueIDs(
      jsxElement(
        'cake',
        'aab',
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue('aab', emptyComments),
          style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
        }),
        [],
      ),
    )
    const view = clearJSXElementChildUniqueIDs(
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
        [cake],
      ),
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
    const { imports } = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const detailOfExports = [exportFunction('whatever')]
    const printedCode = printCode(
      '/index.js',
      printCodeOptions(false, true, true),
      imports,
      [exported],
      null,
      detailOfExports,
    )
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(printedCode)),
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported]),
      expect.objectContaining({}),
      null,
      null,
      detailOfExports,
      expect.objectContaining({}),
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
    expect(
      simplifyParsedTextFileAttributes(clearParseResultUniqueIDsAndEmptyBlocks(parsedCode)),
    ).toMatchSnapshot()
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
        '/index.js',
        printCodeOptions(false, true, true),
        sampleImportsForTests,
        parsedCode.topLevelElements,
        null,
        [exportFunction('whatever')],
      )
      expect(printedCode).toEqual(code)
    } else {
      throw new Error('Parse result is not a success.')
    }
  })
  it('parses back and forth as a function', () => {
    const cake = clearJSXElementChildUniqueIDs(
      jsxElement(
        'cake',
        'aaa',
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue('aaa', emptyComments),
          style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
        }),
        [],
      ),
    )
    const view = clearJSXElementChildUniqueIDs(
      jsxElement(
        'View',
        'aab',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aab', emptyComments) }),
        [cake],
      ),
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
    const { imports } = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const detailOfExports = [exportFunction('whatever')]
    const printedCode = printCode(
      '/index.js',
      printCodeOptions(false, true, true),
      imports,
      [exported],
      null,
      detailOfExports,
    )
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(printedCode)),
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported]),
      expect.objectContaining({}),
      null,
      null,
      detailOfExports,
      expect.objectContaining({}),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses back and forth as a function, with null and undefined values for props', () => {
    const cake = clearJSXElementChildUniqueIDs(
      jsxElement(
        'cake',
        'aaa',
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue('aaa', emptyComments),
          style: jsExpressionValue({ backgroundColor: 'red' }, emptyComments),
          trueProp: jsExpressionValue(true, emptyComments),
          falseProp: jsExpressionValue(false, emptyComments),
          nullProp: jsExpressionValue(null, emptyComments),
          undefinedProp: jsExpressionValue(undefined, emptyComments),
        }),
        [],
      ),
    )
    const view = clearJSXElementChildUniqueIDs(
      jsxElement(
        'View',
        'aab',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aab', emptyComments) }),
        [cake],
      ),
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
    const { imports } = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const detailOfExports = [exportFunction('whatever')]
    const printedCode = printCode(
      '/index.js',
      printCodeOptions(false, true, true),
      imports,
      [exported],
      null,
      detailOfExports,
    )
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(printedCode)),
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported]),
      expect.objectContaining({}),
      null,
      null,
      detailOfExports,
      expect.objectContaining({}),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses back and forth as a function, with a nested object', () => {
    const cake = jsxElement(
      'cake',
      'aab',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aab', emptyComments),
        style: jsxAttributeNestedObjectSimple(
          jsxAttributesFromMap({
            backgroundColor: jsExpressionValue('red', emptyComments),
            color: jsxAttributeNestedArraySimple([
              jsPropertyAccess(
                jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
                'color',
                '',
                expect.objectContaining({}),
                emptyComments,
                'props.color',
                'not-optionally-chained',
              ),
              jsExpressionValue(-200, emptyComments),
            ]),
            boxShadow: jsExpressionFunctionCall('createShadow', [
              jsExpressionValue(15, emptyComments),
              jsPropertyAccess(
                jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
                'shadowValue',
                '',
                expect.objectContaining({}),
                emptyComments,
                'props.shadowValue',
                'not-optionally-chained',
              ),
              jsxAttributeNestedArraySimple([
                jsExpressionValue('hello', emptyComments),
                jsPropertyAccess(
                  jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
                  'there',
                  '',
                  expect.objectContaining({}),
                  emptyComments,
                  'props.there',
                  'not-optionally-chained',
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
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
    const { imports } = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const detailOfExports = [exportFunction('whatever')]
    const printedCode = printCode(
      '/index.js',
      printCodeOptions(false, true, true),
      imports,
      [exported],
      null,
      detailOfExports,
    )
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(printedCode)),
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining([exported].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      detailOfExports,
      expect.objectContaining({}),
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
        '/index.js',
        printCodeOptions(false, true, true),
        emptyImports(),
        parsedCode.topLevelElements,
        null,
        [exportFunction('whatever')],
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
    const jsCode = `
  function test(n) {
    return n * 2
  }`
    const transpiledJSCode = `return (() => {
  function test(n) {
    return n * 2;
  }

  return utopiaCanvasBlockRanToEnd({
    test: test
  });
})();`
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const { imports } = addImport(
      '/code.js',
      'cake',
      null,
      [importAlias('cake')],
      null,
      sampleImportsForTests,
    )
    const arbitraryBlock = arbitraryJSBlock(
      [],
      jsCode,
      transpiledJSCode,
      ['test'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [jsOpaqueArbitraryStatement(jsCode, ['test'], [], '')],
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
                'data-uid': jsExpressionValue('aaa', emptyComments),
              }),
              [
                jsxElement(
                  'cake',
                  'aab',
                  jsxAttributesFromMap({
                    'data-uid': jsExpressionValue('aab', emptyComments),
                    left: jsExpressionOtherJavaScript(
                      [],
                      'test(100)',
                      'test(100);',
                      'return test(100);',
                      ['test'],
                      expect.objectContaining({
                        sources: ['code.tsx'],
                        version: 3,
                        file: 'code.tsx',
                      }),
                      {},
                      emptyComments,
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
    )

    expect(actualResult).toEqual(expectedResult)
  })
  it('parses arbitrary code in a component back and forth', () => {
    const jsCode = `
  function test(n) {
    return n * 2
  }`
    const transpiledJSCode = `return (() => {
  function test(n) {
    return n * 2;
  }

  return utopiaCanvasBlockRanToEnd({
    test: test
  });
})();`
    const { imports } = addImport(
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
              'data-uid': jsExpressionValue('aaa', emptyComments),
            }),
            [
              jsxElement(
                'cake',
                'aab',
                jsxAttributesFromMap({
                  'data-uid': jsExpressionValue('aab', emptyComments),
                  left: jsExpressionOtherJavaScript(
                    [],
                    'test(100)',
                    'test(100);',
                    'return test(100);',
                    ['test'],
                    expect.objectContaining({
                      sources: ['code.tsx'],
                      version: 3,
                      file: 'code.tsx',
                    }),
                    {},
                    emptyComments,
                  ),
                }),
                [],
              ),
            ],
          ),
          arbitraryJSBlock(
            [],
            jsCode,
            transpiledJSCode,
            ['test'],
            [
              JSX_CANVAS_LOOKUP_FUNCTION_NAME,
              BLOCK_RAN_TO_END_FUNCTION_NAME,
              EARLY_RETURN_RESULT_FUNCTION_NAME,
              EARLY_RETURN_VOID_FUNCTION_NAME,
            ],
            expect.objectContaining({
              sources: ['code.tsx'],
              version: 3,
              file: 'code.tsx',
            }),
            {},
            [jsOpaqueArbitraryStatement(jsCode, ['test'], [], '')],
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
              'data-uid': jsExpressionValue(BakedInStoryboardUID, emptyComments),
            }),
            [],
          ),
          null,
          false,
          emptyComments,
        ),
      ),
    ]
    const detailOfExports = [exportFunction('whatever'), exportFunction('storyboard')]
    const printedCode = printCode(
      '/index.js',
      printCodeOptions(false, true, true),
      imports,
      components,
      null,
      detailOfExports,
    )
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(printedCode)),
    )
    const expectedResult = parseSuccess(
      imports,
      expect.arrayContaining(components.map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      detailOfExports,
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
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
          'data-uid': jsExpressionValue('aaa', emptyComments),
        }),
        [
          jsxMapExpression(
            jsxAttributeNestedArraySimple([
              jsExpressionValue(1, emptyComments),
              jsExpressionValue(2, emptyComments),
              jsExpressionValue(3, emptyComments),
            ]),
            jsExpressionOtherJavaScript(
              [functionParam(false, regularParam('x', null))],
              `x=> <View data-uid='abc' />`,
              `(x) => <View data-uid='abc' />;`,
              `return x => utopiaCanvasJSXLookup("abc", {
  x: x,
  callerThis: this
});`,
              ['React', 'View', JSX_CANVAS_LOOKUP_FUNCTION_NAME],
              expect.objectContaining({
                sources: ['code.tsx'],
                version: 3,
                file: 'code.tsx',
              }),
              {
                abc: jsxElement(
                  'View',
                  'abc',
                  jsxAttributesFromMap({ 'data-uid': jsExpressionValue('abc', emptyComments) }),
                  [],
                ),
              },
              emptyComments,
              '',
            ),
            emptyComments,
            ['x'],
            '',
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
      [exportFunction('App')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
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
          'data-uid': jsExpressionValue('aaa', emptyComments),
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
      [exportFunction('App')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
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
          'data-uid': jsExpressionValue('aaa', emptyComments),
        }),
        [
          jsxMapExpression(
            jsxAttributeNestedArraySimple([
              jsExpressionValue(1, emptyComments),
              jsExpressionValue(2, emptyComments),
              jsExpressionValue(3, emptyComments),
            ]),
            jsExpressionOtherJavaScript(
              [functionParam(false, regularParam('n', null))],
              `n => (
        <div data-uid="abc" />
      )`,
              `(n) =>
<div data-uid="abc" />;`,
              `return n => utopiaCanvasJSXLookup("abc", {
  n: n,
  callerThis: this
});`,
              ['React', JSX_CANVAS_LOOKUP_FUNCTION_NAME],
              expect.objectContaining({
                sources: ['code.tsx'],
                version: 3,
                file: 'code.tsx',
              }),
              {
                abc: jsxElement(
                  'div',
                  'abc',
                  jsxAttributesFromMap({ 'data-uid': jsExpressionValue('abc', emptyComments) }),
                  [],
                ),
              },
              emptyComments,
              '',
            ),
            emptyComments,
            ['n'],
            '',
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
      [exportFunction('App')],
      expect.objectContaining({}),
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
          'data-uid': jsExpressionValue('aaa', emptyComments),
        }),
        [
          jsxMapExpression(
            jsxAttributeNestedArraySimple([
              jsExpressionValue(1, emptyComments),
              jsExpressionValue(2, emptyComments),
              jsExpressionValue(3, emptyComments),
            ]),
            jsExpressionOtherJavaScript(
              [functionParam(false, regularParam('x', null))],
              `(x) => (
      <View data-uid='abc' />
    )`,
              `(x) =>
<View data-uid='abc' />;`,
              `return x => utopiaCanvasJSXLookup("abc", {
  x: x,
  callerThis: this
});`,
              ['React', 'View', JSX_CANVAS_LOOKUP_FUNCTION_NAME],
              expect.objectContaining({
                sources: ['code.tsx'],
                version: 3,
                file: 'code.tsx',
              }),
              {
                abc: jsxElement(
                  'View',
                  'abc',
                  jsxAttributesFromMap({ 'data-uid': jsExpressionValue('abc', emptyComments) }),
                  [],
                ),
              },
              emptyComments,
              '',
            ),
            emptyComments,
            ['x'],
            '',
          ),
        ],
      ),
      null,
      false,
      emptyComments,
    )
    const detailOfExports = [exportFunction('App')]
    const printedCode = printCode(
      '/index.js',
      printCodeOptions(false, true, true),
      sampleImportsForTests,
      [component],
      null,
      detailOfExports,
    )
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(printedCode)),
    )
    const expectedResult = parseSuccess(
      sampleImportsForTests,
      expect.arrayContaining([component].map(clearTopLevelElementUniqueIDsAndEmptyBlocks)),
      expect.objectContaining({}),
      null,
      null,
      detailOfExports,
      expect.objectContaining({}),
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
  const MyCustomComponent = props => <View data-uid="abc">{props.children}</View>;

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
      <MyCustomComponent data-uid="ddd">
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
      </MyCustomComponent>
      <View
        style={{ backgroundColor: "blue", position: "absolute" }}
        layout={{ height: 200, left: 80, width: 100, top: 145 }}
        data-uid="ggg"
      />
    </View>
  );
};`
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const ellipse = clearJSXElementChildUniqueIDs(
      jsxElement(
        'Ellipse',
        'bbb',
        jsxAttributesFromMap({
          style: jsExpressionValue({ backgroundColor: 'lightgreen' }, emptyComments),
          layout: jsExpressionValue(
            { height: 100, left: 150, width: 100, top: 540 },
            emptyComments,
          ),
          'data-uid': jsExpressionValue('bbb', emptyComments),
        }),
        [],
      ),
    )
    const rectangle = clearJSXElementChildUniqueIDs(
      jsxElement(
        'Rectangle',
        'ccc',
        jsxAttributesFromMap({
          style: jsExpressionValue({ backgroundColor: 'orange' }, emptyComments),
          layout: jsExpressionValue(
            { height: 100, left: 150, width: 100, top: 540 },
            emptyComments,
          ),
          'data-uid': jsExpressionValue('ccc', emptyComments),
        }),
        [],
      ),
    )
    const myCustomComponent = clearJSXElementChildUniqueIDs(
      jsxElement(
        'MyCustomComponent',
        'ddd',
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue('ddd', emptyComments),
        }),
        [ellipse, rectangle],
      ),
    )
    const view = clearJSXElementChildUniqueIDs(
      jsxElement(
        'View',
        'ggg',
        jsxAttributesFromMap({
          style: jsExpressionValue(
            { backgroundColor: 'blue', position: 'absolute' },
            emptyComments,
          ),
          layout: jsExpressionValue({ height: 200, left: 80, width: 100, top: 145 }, emptyComments),
          'data-uid': jsExpressionValue('ggg', emptyComments),
        }),
        [],
      ),
    )
    const assignmentComponent = jsExpressionOtherJavaScript(
      [functionParam(false, regularParam('props', null))],
      'props => <View data-uid="abc">{props.children}</View>',
      '(props) => <View data-uid="abc">{props.children}</View>;',
      `return props => React.createElement(View, {\n  \"data-uid\": \"abc\"\n}, props.children);`,
      ['React', 'View', JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({}),
      {
        cb4: jsxElement(
          'View',
          '',
          jsxAttributesFromMap({
            'data-uid': jsExpressionValue('cb4', emptyComments, ''),
          }),
          [
            jsPropertyAccess(
              jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
              'children',
              '',
              expect.objectContaining({}),
              emptyComments,
              'props.children',
              'not-optionally-chained',
            ),
          ],
        ),
      },
      emptyComments,
      '',
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
          style: jsExpressionValue(
            { backgroundColor: 'lightgrey', position: 'absolute' },
            emptyComments,
          ),
          layout: jsxAttributeNestedObjectSimple(
            jsxAttributesFromMap({
              height: jsPropertyAccess(
                jsPropertyAccess(
                  jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
                  'layout',
                  '',
                  expect.objectContaining({}),
                  emptyComments,
                  'props.layout',
                  'not-optionally-chained',
                ),
                'height',
                '',
                expect.objectContaining({}),
                emptyComments,
                'props.layout.height',
                'not-optionally-chained',
              ),
              left: jsPropertyAccess(
                jsPropertyAccess(
                  jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
                  'layout',
                  '',
                  expect.objectContaining({}),
                  emptyComments,
                  'props.layout',
                  'not-optionally-chained',
                ),
                'left',
                '',
                expect.objectContaining({}),
                emptyComments,
                'props.layout.left',
                'not-optionally-chained',
              ),
              width: jsPropertyAccess(
                jsPropertyAccess(
                  jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
                  'layout',
                  '',
                  expect.objectContaining({}),
                  emptyComments,
                  'props.layout',
                  'not-optionally-chained',
                ),
                'width',
                '',
                expect.objectContaining({}),
                emptyComments,
                'props.layout.width',
                'not-optionally-chained',
              ),
              top: jsPropertyAccess(
                jsPropertyAccess(
                  jsIdentifier('props', '', expect.objectContaining({}), emptyComments),
                  'layout',
                  '',
                  expect.objectContaining({}),
                  emptyComments,
                  'props.layout',
                  'not-optionally-chained',
                ),
                'top',
                '',
                expect.objectContaining({}),
                emptyComments,
                'props.layout.top',
                'not-optionally-chained',
              ),
            }),
            emptyComments,
          ),
          'data-uid': jsExpressionValue('aaa', emptyComments),
        }),
        [myCustomComponent, view],
      ),
      arbitraryJSBlock(
        [],
        `const a = 20;
  const b = 40;
  const MyCustomComponent = props => <View data-uid="abc">{props.children}</View>;`,
        `return (() => {
  const a = 20;
  const b = 40;

  const MyCustomComponent = props => ${JSX_CANVAS_LOOKUP_FUNCTION_NAME}("abc", {
    props: props,
    callerThis: this
  });

  return utopiaCanvasBlockRanToEnd({
    a: a,
    b: b,
    MyCustomComponent: MyCustomComponent
  });
})();`,
        ['a', 'b', 'MyCustomComponent'],
        [
          'React',
          'View',
          JSX_CANVAS_LOOKUP_FUNCTION_NAME,
          BLOCK_RAN_TO_END_FUNCTION_NAME,
          EARLY_RETURN_RESULT_FUNCTION_NAME,
          EARLY_RETURN_VOID_FUNCTION_NAME,
        ],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {
          abc: expect.objectContaining({}),
        },
        [
          jsAssignmentStatement(
            'const',
            [
              jsAssignment(
                regularParam('a', jsExpressionValue(20, emptyComments, '')),
                jsExpressionValue(20, emptyComments, ''),
              ),
            ],
            '',
          ),
          jsAssignmentStatement(
            'const',
            [
              jsAssignment(
                regularParam('b', jsExpressionValue(40, emptyComments, '')),
                jsExpressionValue(40, emptyComments, ''),
              ),
            ],
            '',
          ),
          jsAssignmentStatement(
            'const',
            [
              jsAssignment(
                regularParam('MyCustomComponent', assignmentComponent),
                assignmentComponent,
              ),
            ],
            '',
          ),
        ],
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
      [exportFunction('App')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
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
          'data-uid': jsExpressionValue('aaa', emptyComments),
          booleanProperty: jsExpressionValue(true, emptyComments),
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
      [exportFunction('App')],
      expect.objectContaining({}),
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
        'data-uid': jsExpressionValue('aaa', emptyComments),
        booleanProperty: jsExpressionValue(true, emptyComments),
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
      '/index.js',
      printCodeOptions(false, true, true),
      sampleImportsForTests,
      [exported],
      null,
      [exportFunction('whatever')],
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
        'data-uid': jsExpressionValue('aaa', emptyComments),
        booleanProperty: jsExpressionValue(false, emptyComments),
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
      '/index.js',
      printCodeOptions(false, true, true),
      sampleImportsForTests,
      [exported],
      null,
      [exportFunction('whatever')],
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const arbitraryBlockCode = `for (var n = 0; n != -1; n++) {
    const n2 = n * 2
  }
  while (true) {
    const a = 1
  }`
    const arbitraryBlockTranspiledCode = `return (() => {
  var _loopIt = 0,
      _loopIt2 = 0;

  for (var n = 0; n != -1; n++) {
    if (_loopIt++ > ${InfiniteLoopMaxIterations}) {
      throw new RangeError('${InfiniteLoopError}');
    }

    const n2 = n * 2;
  }

  while (true) {
    if (_loopIt2++ > ${InfiniteLoopMaxIterations}) {
      throw new RangeError('${InfiniteLoopError}');
    }

    const a = 1;
  }

  return utopiaCanvasBlockRanToEnd({});
})();`
    const arbitraryBlock = arbitraryJSBlock(
      [],
      arbitraryBlockCode,
      arbitraryBlockTranspiledCode,
      [],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [
        jsOpaqueArbitraryStatement(
          `
  for (var n = 0; n != -1; n++) {
    const n2 = n * 2
  }`,
          [],
          [],
          '',
        ),
        jsOpaqueArbitraryStatement(
          `
  while (true) {
    const a = 1
  }`,
          [],
          [],
          '',
        ),
      ],
    )
    const view = jsxElement(
      'div',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const arbitraryBlockCode = `let result = []
  for (var n = 0; n < 5; n++) {
    const n2 = n * 2
    result.push(<div style={{ left: n, top: n2 }} data-uid='bbb' />)
  }`
    const arbitraryBlockTranspiledCode = `return (() => {
  var _loopIt = 0;
  let result = [];

  for (var n = 0; n < 5; n++) {
    if (_loopIt++ > ${InfiniteLoopMaxIterations}) {
      throw new RangeError('${InfiniteLoopError}');
    }

    const n2 = n * 2;
    result.push(${JSX_CANVAS_LOOKUP_FUNCTION_NAME}("bbb", {
      n: n,
      n2: n2,
      callerThis: this
    }));
  }

  return utopiaCanvasBlockRanToEnd({
    result: result
  });
})();`
    const arbitraryBlock = arbitraryJSBlock(
      [],
      arbitraryBlockCode,
      arbitraryBlockTranspiledCode,
      ['result'],
      [
        'React',
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
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
      [
        jsAssignmentStatement(
          'let',
          [
            jsAssignment(
              regularParam('result', jsxAttributeNestedArraySimple([])),
              jsExpressionNestedArray([], emptyComments, ''),
            ),
          ],
          '',
        ),
        jsOpaqueArbitraryStatement(
          `
  for (var n = 0; n < 5; n++) {
    const n2 = n * 2
    result.push(<div style={{ left: n, top: n2 }} data-uid='bbb' />)
  }`,
          [],
          ['result', 'React'],
          '',
        ),
      ],
    )
    const innerBlock = jsIdentifier('result', '', expect.objectContaining({}), emptyComments)
    const view = jsxElement(
      'div',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const arbitraryBlockOriginalCode = `n => {
      return <div style={{left: n * 30, top: n * 30}} data-uid='bbb' />
    }`
    const arbitraryBlockCode = `(n) => {
  return <div style={{ left: n * 30, top: n * 30 }} data-uid='bbb' />;
};`
    const arbitraryBlockTranspiledCode = `return n => {
  return utopiaCanvasJSXLookup("bbb", {
    n: n,
    callerThis: this
  });
};`
    const innerElement = clearJSXElementUniqueIDs(
      jsxElement(
        'div',
        'bbb',
        jsxAttributesFromMap({
          style: jsExpressionNestedObject(
            [
              jsxPropertyAssignment(
                'left',
                jsExpressionOtherJavaScript(
                  [],
                  `n * 30`,
                  `n * 30;`,
                  `return n * 30;`,
                  ['n'],
                  expect.objectContaining({}),
                  {},
                  emptyComments,
                ),
                emptyComments,
                emptyComments,
              ),
              jsxPropertyAssignment(
                'top',
                jsExpressionOtherJavaScript(
                  [],
                  `n * 30`,
                  `n * 30;`,
                  `return n * 30;`,
                  ['n'],
                  expect.objectContaining({}),
                  {},
                  emptyComments,
                ),
                emptyComments,
                emptyComments,
              ),
            ],
            emptyComments,
          ),
          ['data-uid']: jsExpressionValue('bbb', emptyComments),
        }),
        [],
      ),
    )

    const arbitraryBlock = jsxMapExpression(
      jsxAttributeNestedArraySimple([
        jsExpressionValue(1, emptyComments),
        jsExpressionValue(2, emptyComments),
        jsExpressionValue(3, emptyComments),
      ]),
      jsExpressionOtherJavaScript(
        [functionParam(false, regularParam('n', null))],
        arbitraryBlockOriginalCode,
        arbitraryBlockCode,
        arbitraryBlockTranspiledCode,
        ['React', JSX_CANVAS_LOOKUP_FUNCTION_NAME],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {
          bbb: innerElement,
        },
        emptyComments,
        '',
      ),
      emptyComments,
      ['n'],
      '',
    )
    const view = clearJSXElementChildUniqueIDs(
      jsxElement(
        'div',
        'aaa',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
        [arbitraryBlock],
      ),
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const arbitraryBlockOriginalCode = `n => {
      return <div style={{left: n * a, top: n * a}} data-uid='bbb' />
    }`
    const arbitraryBlockCode = `(n) => {
  return <div style={{ left: n * a, top: n * a }} data-uid='bbb' />;
};`
    const arbitraryBlockTranspiledCode = `return n => {
  return utopiaCanvasJSXLookup("bbb", {
    n: n,
    a: a,
    callerThis: this
  });
};`
    const innerElement = clearJSXElementUniqueIDs(
      jsxElement(
        'div',
        'bbb',
        jsxAttributesFromMap({
          style: jsExpressionNestedObject(
            [
              jsxPropertyAssignment(
                'left',
                jsExpressionOtherJavaScript(
                  [],
                  `n * a`,
                  `n * a;`,
                  `return n * a;`,
                  ['n', 'a'],
                  expect.objectContaining({}),
                  {},
                  emptyComments,
                ),
                emptyComments,
                emptyComments,
              ),
              jsxPropertyAssignment(
                'top',
                jsExpressionOtherJavaScript(
                  [],
                  `n * a`,
                  `n * a;`,
                  `return n * a;`,
                  ['n', 'a'],
                  expect.objectContaining({}),
                  {},
                  emptyComments,
                ),
                emptyComments,
                emptyComments,
              ),
            ],
            emptyComments,
          ),
          ['data-uid']: jsExpressionValue('bbb', emptyComments),
        }),
        [],
      ),
    )
    const arbitraryBlock = jsxMapExpression(
      jsxAttributeNestedArraySimple([
        jsExpressionValue(1, emptyComments),
        jsExpressionValue(2, emptyComments),
        jsExpressionValue(3, emptyComments),
      ]),
      jsExpressionOtherJavaScript(
        [functionParam(false, regularParam('n', null))],
        arbitraryBlockOriginalCode,
        arbitraryBlockCode,
        arbitraryBlockTranspiledCode,
        ['a', 'React', 'utopiaCanvasJSXLookup'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
        {
          bbb: innerElement,
        },
        emptyComments,
        '',
      ),
      emptyComments,
      ['n'],
      '',
    )

    const topLevelArbitraryBlock = arbitraryJSBlock(
      [],
      `const a = 30`,
      `return (() => {
  const a = 30;
  return utopiaCanvasBlockRanToEnd({
    a: a
  });
})();`,
      ['a'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [
        jsAssignmentStatement(
          'const',
          [
            jsAssignment(
              regularParam('a', jsExpressionValue(30, emptyComments, '')),
              jsExpressionValue(30, emptyComments, ''),
            ),
          ],
          '',
        ),
      ],
    )

    const view = clearJSXElementChildUniqueIDs(
      jsxElement(
        'div',
        'aaa',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
        [arbitraryBlock],
      ),
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('svg elements are accepted', () => {
    const code = `import * as React from "react"
export var whatever = props => {
  return <svg data-uid='abc'/>
}`
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const view = jsxElement(
      'svg',
      'abc',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('abc', emptyComments) }),
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
      [exportFunction('whatever')],
      expect.objectContaining({}),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses fine with a circular dependency.', () => {
    const code = `import * as React from "react";
import {
  View
} from "utopia-api";
const a = (n) => n > 0 ? <div>n</div> : b(10)
export var whatever = (props) => <View data-uid='aaa' />
const b = (n) => n > 0 ? <div>n</div> : a(10)
`
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
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
      (_) => {
        throw new Error('Unable to parse code.')
      },
      (success) => {
        let uids: Array<string> = []
        for (const topLevelElement of success.topLevelElements) {
          if (isUtopiaJSXComponent(topLevelElement)) {
            ensureElementsHaveUID(topLevelElement.rootElement, uids)
          }
        }
      },
      (_) => {
        throw new Error('Unable to parse code.')
      },
      actualResult,
    )
  })
  it('inserts data-uid into elements as part of the parse', () => {
    function checkDataUIDsPopulated({ code: printedCode }: ArbitraryProject): boolean {
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
    const printedArbitrary = printedProjectContentArbitrary(false)
    const dataUIDProperty = FastCheck.property(printedArbitrary, checkDataUIDsPopulated)
    FastCheck.assert(dataUIDProperty, { verbose: true })
  })
  describe('check that the UIDs of everything in a file also align with the highlight bounds for that file', () => {
    function checkElementUIDs(stripUIDs: boolean): void {
      function checkElementUIDSMatchHighlightBounds(
        printedArbitraryProjects: [ArbitraryProject, ArbitraryProject],
      ): boolean {
        const [firstPrintedProjectContent, secondPrintedProjectContent] = printedArbitraryProjects
        const alreadyExistingUIDs: Set<string> = emptySet()

        let fileCounter: number = 100
        let projectContents: ProjectContents = {}

        for (const { code: printedCode } of [
          firstPrintedProjectContent,
          secondPrintedProjectContent,
        ]) {
          const parseResult = testParseCode(printedCode, alreadyExistingUIDs)
          foldParsedTextFile(
            (failure) => {
              throw new Error(`${JSON.stringify(failure)}`)
            },
            (success) => {
              let uids: Array<string> = []
              for (const topLevelElement of success.topLevelElements) {
                switch (topLevelElement.type) {
                  case 'UTOPIA_JSX_COMPONENT':
                    ensureElementsHaveUID(
                      topLevelElement.rootElement,
                      uids,
                      isWantedElement,
                      'walk-attributes',
                    )
                    if (topLevelElement.arbitraryJSBlock != null) {
                      ensureArbitraryBlocksHaveUID(
                        topLevelElement.arbitraryJSBlock,
                        uids,
                        isWantedElement,
                        'walk-attributes',
                      )
                    }
                    break
                  case 'ARBITRARY_JS_BLOCK':
                    ensureArbitraryBlocksHaveUID(
                      topLevelElement,
                      uids,
                      isWantedElement,
                      'walk-attributes',
                    )
                    break
                  case 'IMPORT_STATEMENT':
                  case 'UNPARSED_CODE':
                    break
                  default:
                    assertNever(topLevelElement)
                }
              }

              // Check the UIDs for the elements, which excludes attributes.
              const elementUIDS = new Set(uids)
              const highlightBoundsUIDs = new Set(Object.keys(success.highlightBounds))
              if (!setsEqual(elementUIDS, highlightBoundsUIDs)) {
                throw new Error(
                  `Element UIDs [${Array.from(
                    elementUIDS,
                  ).sort()}] do not match the highlight bounds UIDs: [${Array.from(
                    highlightBoundsUIDs,
                  ).sort()}]`,
                )
              }

              const fileValue = textFile(
                textFileContents(printedCode, success, RevisionsState.ParsedAhead),
                null,
                null,
                0,
              )
              const singleFileUniqueIDsResult = getAllUniqueUids(
                contentsToTree({ '/index.js': fileValue }),
              )

              // Check the UIDs for anything and everything, including attributes.
              const fullHighlightBoundsUIDs = new Set(Object.keys(success.fullHighlightBounds))
              const allUIDsAreEqual = setsEqual(
                fullHighlightBoundsUIDs,
                new Set(singleFileUniqueIDsResult.uniqueIDs),
              )
              if (!allUIDsAreEqual) {
                throw new Error(
                  `All UIDs [${Array.from(
                    singleFileUniqueIDsResult.uniqueIDs,
                  ).sort()}] do not match the full highlight bounds UIDs: [${Array.from(
                    fullHighlightBoundsUIDs,
                  ).sort()}]`,
                )
              }

              projectContents[`/index${fileCounter++}.js`] = fileValue
            },
            (unparsed) => {
              throw new Error(`${unparsed}`)
            },
            parseResult,
          )
        }

        // Check that this parse has not surfaced any duplicates within itself.
        const uniqueIDsResult = getAllUniqueUids(contentsToTree(projectContents))
        const anyDuplicates = Object.keys(uniqueIDsResult.duplicateIDs).length > 0
        if (anyDuplicates) {
          throw new Error(`Found duplicate UIDs: ${uniqueIDsResult.duplicateIDs}`)
        }

        return true
      }
      const printedArbitrary = printedProjectContentArbitrary(stripUIDs)
      const dataUIDProperty = FastCheck.property(
        FastCheck.tuple(printedArbitrary, printedArbitrary),
        checkElementUIDSMatchHighlightBounds,
      )
      FastCheck.assert(dataUIDProperty, { verbose: false, numRuns: 100 })
    }
    it('with UIDs left in', () => {
      checkElementUIDs(false)
    })
    it('with UIDs stripped', () => {
      checkElementUIDs(true)
    })
  })
  it('when react is not imported treat components as arbitrary blocks', () => {
    const code = `
export var whatever = (props) => <View data-uid='aaa'>
  <View data-uid='aaa' />
</View>
`
    const actualResult = testParseCode(code)
    foldParsedTextFile(
      (_) => {
        throw new Error('Unable to parse code.')
      },
      (success) => {
        expect(elementsStructure(success.topLevelElements)).toMatchInlineSnapshot(`
          "UNPARSED_CODE
          ARBITRARY_JS_BLOCK
          UNPARSED_CODE"
        `)
      },
      (_) => {
        throw new Error('Unable to parse code.')
      },
      actualResult,
    )
  })
  it('parse jsx literal in attribute as jsx element', () => {
    const code = `import * as React from "react";
import { View } from "utopia-api";
var MyComp = (props) => {
  return <View data-uid='aaa'>{props.title}</View>
}
export var App = props => {
  return (
    <MyComp title={<h1>Hello</h1>} />
  )
}`
    const actualResult = testParseCode(code)
    foldParsedTextFile(
      (_) => {
        throw new Error('Unable to parse code.')
      },
      (success) => {
        expect(elementsStructure(success.topLevelElements)).toMatchInlineSnapshot(`
          "IMPORT_STATEMENT
          UNPARSED_CODE
          IMPORT_STATEMENT
          UNPARSED_CODE
          UTOPIA_JSX_COMPONENT - MyComp
            JSX_ELEMENT - View - aaa
              JS_PROPERTY_ACCESS - 807
                JS_IDENTIFIER - 09c
          UNPARSED_CODE
          UTOPIA_JSX_COMPONENT - App
            JSX_ELEMENT - MyComp - bb3
              JSX_ELEMENT - h1 - cc5
                JSX_TEXT_BLOCK - 9e1"
        `)

        const appComponent = success.topLevelElements.find(
          (e) => isUtopiaJSXComponent(e) && e.name === 'App',
        )!
        if (!isUtopiaJSXComponent(appComponent) || appComponent.name !== `App`) {
          throw new Error('expected to have a topLevelElement which is the App component')
        }
        if (!isJSXElement(appComponent.rootElement)) {
          throw new Error(`expected the App component's root element to be a JSXElement`)
        }

        appComponent.rootElement.children

        const arbitraryProp = optionalMap(
          (p) => p.value,
          appComponent.rootElement.props
            .filter(isJSXAttributesEntry)
            .find((p) => p.key === 'title'),
        )

        if (arbitraryProp == null || !modifiableAttributeIsJsxElement(arbitraryProp)) {
          throw new Error(`expected to have an jsx element prop called props.title`)
        }
      },
      (_) => {
        throw new Error('Unable to parse code.')
      },
      actualResult,
    )
  })

  it('parses self-closing jsx literal in attribute as jsx element', () => {
    const code = `import * as React from "react";
import { View } from "utopia-api";
var MyComp = (props) => {
  return <View data-uid='aaa'>{props.title}</View>
}
export var App = props => {
  return (
    <MyComp title={<div />} />
  )
}`
    const actualResult = testParseCode(code)
    foldParsedTextFile(
      (_) => {
        throw new Error('Unable to parse code.')
      },
      (success) => {
        expect(elementsStructure(success.topLevelElements)).toMatchInlineSnapshot(`
          "IMPORT_STATEMENT
          UNPARSED_CODE
          IMPORT_STATEMENT
          UNPARSED_CODE
          UTOPIA_JSX_COMPONENT - MyComp
            JSX_ELEMENT - View - aaa
              JS_PROPERTY_ACCESS - 807
                JS_IDENTIFIER - 09c
          UNPARSED_CODE
          UTOPIA_JSX_COMPONENT - App
            JSX_ELEMENT - MyComp - 0b3
              JSX_ELEMENT - div - b4c"
        `)

        const appComponent = success.topLevelElements.find(
          (e) => isUtopiaJSXComponent(e) && e.name === 'App',
        )!
        if (!isUtopiaJSXComponent(appComponent) || appComponent.name !== `App`) {
          throw new Error('expected to have a topLevelElement which is the App component')
        }
        if (!isJSXElement(appComponent.rootElement)) {
          throw new Error(`expected the App component's root element to be a JSXElement`)
        }

        appComponent.rootElement.children

        const arbitraryProp = optionalMap(
          (p) => p.value,
          appComponent.rootElement.props
            .filter(isJSXAttributesEntry)
            .find((p) => p.key === 'title'),
        )

        if (arbitraryProp == null || !modifiableAttributeIsJsxElement(arbitraryProp)) {
          throw new Error(`expected to have an jsx element prop called props.title`)
        }
      },
      (_) => {
        throw new Error('Unable to parse code.')
      },
      actualResult,
    )
  })
  it('parses destructure assignments', () => {
    const code = `import * as React from "react";
import {
  UtopiaUtils,
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  Scene
} from "utopia-api";
import { cake } from 'cake'
export var whatever = (props) => {
  const propsNewName = props
  const { a, b } = props
  const [ c, d ] = props
  const { e: f } = props
  return <div data-uid='root' />
}
`
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultSourceMapsUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    const toAssignmentsOptic = fromTypeGuard(isParseSuccess)
      .compose(fromField('topLevelElements'))
      .compose(traverseArray())
      .compose(filtered(isUtopiaJSXComponent))
      .compose(fromTypeGuard(isUtopiaJSXComponent))
      .compose(fromField('arbitraryJSBlock'))
      .compose(notNull())
      .compose(fromField('statements'))
    const possibleAssignments = toFirst(toAssignmentsOptic, actualResult)
    expect(possibleAssignments).toMatchInlineSnapshot(`
      Object {
        "type": "RIGHT",
        "value": Array [
          Object {
            "assignments": Array [
              Object {
                "leftHandSide": Object {
                  "defaultExpression": Object {
                    "comments": Object {
                      "leadingComments": Array [],
                      "trailingComments": Array [],
                    },
                    "name": "props",
                    "sourceMap": null,
                    "type": "JS_IDENTIFIER",
                    "uid": "",
                  },
                  "paramName": "propsNewName",
                  "type": "REGULAR_PARAM",
                },
                "rightHandSide": Object {
                  "comments": Object {
                    "leadingComments": Array [],
                    "trailingComments": Array [],
                  },
                  "name": "props",
                  "sourceMap": null,
                  "type": "JS_IDENTIFIER",
                  "uid": "",
                },
                "type": "JS_ASSIGNMENT",
              },
            ],
            "declarationKeyword": "const",
            "type": "JS_ASSIGNMENT_STATEMENT",
            "uid": "",
          },
          Object {
            "assignments": Array [
              Object {
                "leftHandSide": Object {
                  "parts": Array [
                    Object {
                      "defaultExpression": null,
                      "param": Object {
                        "boundParam": Object {
                          "defaultExpression": null,
                          "paramName": "a",
                          "type": "REGULAR_PARAM",
                        },
                        "dotDotDotToken": false,
                        "type": "PARAM",
                      },
                      "propertyName": undefined,
                    },
                    Object {
                      "defaultExpression": null,
                      "param": Object {
                        "boundParam": Object {
                          "defaultExpression": null,
                          "paramName": "b",
                          "type": "REGULAR_PARAM",
                        },
                        "dotDotDotToken": false,
                        "type": "PARAM",
                      },
                      "propertyName": undefined,
                    },
                  ],
                  "type": "DESTRUCTURED_OBJECT",
                },
                "rightHandSide": Object {
                  "comments": Object {
                    "leadingComments": Array [],
                    "trailingComments": Array [],
                  },
                  "name": "props",
                  "sourceMap": null,
                  "type": "JS_IDENTIFIER",
                  "uid": "",
                },
                "type": "JS_ASSIGNMENT",
              },
            ],
            "declarationKeyword": "const",
            "type": "JS_ASSIGNMENT_STATEMENT",
            "uid": "",
          },
          Object {
            "assignments": Array [
              Object {
                "leftHandSide": Object {
                  "parts": Array [
                    Object {
                      "boundParam": Object {
                        "defaultExpression": null,
                        "paramName": "c",
                        "type": "REGULAR_PARAM",
                      },
                      "dotDotDotToken": false,
                      "type": "PARAM",
                    },
                    Object {
                      "boundParam": Object {
                        "defaultExpression": null,
                        "paramName": "d",
                        "type": "REGULAR_PARAM",
                      },
                      "dotDotDotToken": false,
                      "type": "PARAM",
                    },
                  ],
                  "type": "DESTRUCTURED_ARRAY",
                },
                "rightHandSide": Object {
                  "comments": Object {
                    "leadingComments": Array [],
                    "trailingComments": Array [],
                  },
                  "name": "props",
                  "sourceMap": null,
                  "type": "JS_IDENTIFIER",
                  "uid": "",
                },
                "type": "JS_ASSIGNMENT",
              },
            ],
            "declarationKeyword": "const",
            "type": "JS_ASSIGNMENT_STATEMENT",
            "uid": "",
          },
          Object {
            "assignments": Array [
              Object {
                "leftHandSide": Object {
                  "parts": Array [
                    Object {
                      "defaultExpression": null,
                      "param": Object {
                        "boundParam": Object {
                          "defaultExpression": null,
                          "paramName": "f",
                          "type": "REGULAR_PARAM",
                        },
                        "dotDotDotToken": false,
                        "type": "PARAM",
                      },
                      "propertyName": "e",
                    },
                  ],
                  "type": "DESTRUCTURED_OBJECT",
                },
                "rightHandSide": Object {
                  "comments": Object {
                    "leadingComments": Array [],
                    "trailingComments": Array [],
                  },
                  "name": "props",
                  "sourceMap": null,
                  "type": "JS_IDENTIFIER",
                  "uid": "",
                },
                "type": "JS_ASSIGNMENT",
              },
            ],
            "declarationKeyword": "const",
            "type": "JS_ASSIGNMENT_STATEMENT",
            "uid": "",
          },
        ],
      }
    `)
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
      throw new Error('expected parseResult to be Right')
    }
    const consoleLogBlock = parseResult.topLevelElements.find(isArbitraryJSBlock)!

    if (
      !isArbitraryJSBlock(consoleLogBlock) ||
      consoleLogBlock.javascript !== `console.log('hello!')`
    ) {
      throw new Error('expected the first topLevelElement to be the console logline')
    }

    const transpiledCharacter = 1 + consoleLogBlock.transpiledJavascript.indexOf('log')
    const transpiledLine = 2

    const consumer = getSourceMapConsumer(consoleLogBlock.sourceMap)

    const position = consumer.getOriginalPosition(transpiledLine, transpiledCharacter)

    expect(position).toEqual(expect.objectContaining({ line: 13, column: 9 }))
  })

  it('maps an arbitraryJSBlock inside a utopiaJSXComponent', () => {
    const parseResult = testParseCode(code)
    if (!isParseSuccess(parseResult)) {
      throw new Error('expected parseResult to be Right')
    }
    const appComponent = parseResult.topLevelElements.find(isUtopiaJSXComponent)!
    if (!isUtopiaJSXComponent(appComponent) || appComponent.name !== `App`) {
      throw new Error('expected the second topLevelElement to be the App component')
    }

    const arbitraryBlock = appComponent.arbitraryJSBlock

    if (arbitraryBlock == null) {
      throw new Error(`expected the App component's arbitraryJSBlock to be defined`)
    }

    const transpiledCharacter = 1 + arbitraryBlock.transpiledJavascript.split('\n')[2].indexOf('b')
    const transpiledLine = 3

    const consumer = getSourceMapConsumer(arbitraryBlock.sourceMap)

    const position = consumer.getOriginalPosition(transpiledLine, transpiledCharacter)

    expect(position).toEqual(expect.objectContaining({ line: 17, column: 10 }))
  })

  it('maps a jsxAttributeOtherJavaScript correctly', () => {
    const parseResult = testParseCode(code)
    if (!isParseSuccess(parseResult)) {
      throw new Error('expected parseResult to be a success')
    }
    const appComponent = parseResult.topLevelElements.find(isUtopiaJSXComponent)!
    if (!isUtopiaJSXComponent(appComponent) || appComponent.name !== `App`) {
      throw new Error('expected the second topLevelElement to be the App component')
    }
    if (!isJSXElement(appComponent.rootElement)) {
      throw new Error(`expected the App component's root element to be a JSXElement`)
    }

    const arbitraryProp = optionalMap(
      (p) => p.value,
      appComponent.rootElement.props
        .filter(isJSXAttributesEntry)
        .find((p) => p.key === 'arbitrary'),
    )

    if (arbitraryProp == null || !modifiableAttributeIsAttributeOtherJavaScript(arbitraryProp)) {
      throw new Error(`expected <View /> to have an arbitrary js prop called props.arbitrary`)
    }

    const transpiledCharacter = 1 + arbitraryProp.transpiledJavascript.indexOf('log')

    const transpiledLine = 1

    const consumer = getSourceMapConsumer(arbitraryProp.sourceMap)

    const position = consumer.getOriginalPosition(transpiledLine, transpiledCharacter)

    expect(position).toEqual(expect.objectContaining({ line: 29, column: 18 }))
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
      'trim-bounds',
      'do-not-apply-steganography',
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
      transpileJavascriptFromCode(
        'test.js',
        file,
        code,
        sourceMap,
        [],
        'do-not-wrap',
        'do-not-apply-steganography',
      ),
    ).toMatchSnapshot()
  })
})
