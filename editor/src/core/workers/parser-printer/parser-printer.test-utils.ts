import * as Babel from '@babel/standalone'
import * as BabelTraverse from '@babel/traverse'
import * as BabelTypes from '@babel/types'
import ReactSyntaxPlugin from 'babel-plugin-syntax-jsx'
import * as FastCheck from 'fast-check'
import { Arbitrary } from 'fast-check'
import { MapLike } from 'typescript'
import * as PP from '../../shared/property-path'
import {
  ArbitraryJSBlock,
  arbitraryJSBlock,
  clearTopLevelElementUniqueIDs,
  isJSExpressionOtherJavaScript,
  isUtopiaJSXComponent,
  JSXArbitraryBlock,
  jsxArbitraryBlock,
  JSXArrayElement,
  jsxArraySpread,
  JSXArraySpread,
  jsxArrayValue,
  JSXArrayValue,
  JSExpression,
  JSExpressionFunctionCall,
  jsExpressionFunctionCall,
  JSExpressionNestedArray,
  jsExpressionNestedArray,
  JSExpressionNestedObject,
  jsExpressionNestedObject,
  JSExpressionOtherJavaScript,
  jsExpressionOtherJavaScript,
  JSXAttributes,
  JSExpressionValue,
  jsExpressionValue,
  JSXElement,
  jsxElement,
  JSXElementChild,
  JSXElementName,
  jsxElementName,
  JSXProperty,
  JSXPropertyAssignment,
  jsxPropertyAssignment,
  jsxSpreadAssignment,
  JSXSpreadAssignment,
  jsxTextBlock,
  JSXTextBlock,
  TopLevelElement,
  UtopiaJSXComponent,
  utopiaJSXComponent,
  defaultPropsParam,
  SingleLineComment,
  MultiLineComment,
  Comment,
  walkElement,
  getJSXElementNameAsString,
  isJSXElement,
  FunctionDeclarationSyntax,
  BlockOrExpression,
  jsxAttributesFromMap,
  ImportStatement,
  importStatement,
  emptyComments,
  ParsedComments,
  parsedComments,
  isJSXConditionalExpression,
  JSXConditionalExpression,
  jsxConditionalExpression,
  JSXFragment,
  jsxFragment,
  ElementsWithin,
  simplifyAttributeIfPossible,
  simplifyAttributesIfPossible,
} from '../../shared/element-template'
import { addImport } from '../common/project-file-utils'
import { ErrorMessage } from '../../shared/error-messages'
import {
  Imports,
  ParsedTextFile,
  ParseSuccess,
  PropertyPath,
  importAlias,
  foldParsedTextFile,
  mapParsedTextFile,
  forEachParseSuccess,
  ExportsDetail,
  ExportDetail,
  EmptyExportsDetail,
  StaticElementPathPart,
  isParseSuccess,
  isTextFile,
  ProjectFile,
  ExportVariables,
  exportVariable,
  exportVariables,
} from '../../shared/project-file-types'
import { lintAndParse, printCode, printCodeOptions } from './parser-printer'
import { getUtopiaID, getUtopiaIDFromJSXElement } from '../../shared/uid-utils'
import { assertNever, fastForEach } from '../../shared/utils'
import { addUniquely, flatMapArray } from '../../shared/array-utils'
import { optionalMap } from '../../shared/optional-utils'
import { emptySet } from '../../shared/set-utils'
import { UTOPIA_UID_KEY } from '../../../core/model/utopia-constants'
import { objectMap } from '../../shared/object-utils'

export const singleLineCommentArbitrary: Arbitrary<SingleLineComment> =
  lowercaseStringArbitrary().map((text) => {
    return {
      type: 'SINGLE_LINE_COMMENT',
      comment: text,
      rawText: text,
      trailingNewLine: false,
      pos: null,
    }
  })

export const multiLineCommentArbitrary: Arbitrary<MultiLineComment> =
  lowercaseStringArbitrary().map((text) => {
    return {
      type: 'MULTI_LINE_COMMENT',
      comment: text,
      rawText: text,
      trailingNewLine: false,
      pos: null,
    }
  })

export const commentArbitrary: Arbitrary<Comment> = FastCheck.oneof<Comment>(
  singleLineCommentArbitrary,
  multiLineCommentArbitrary,
)

const JavaScriptReservedKeywords: Array<string> = [
  'break',
  'case',
  'catch',
  'class',
  'const',
  'continue',
  'debugger',
  'default',
  'delete',
  'do',
  'else',
  'export',
  'extends',
  'finally',
  'for',
  'function',
  'if',
  'import',
  'in',
  'instanceof',
  'new',
  'return',
  'super',
  'switch',
  'this',
  'throw',
  'try',
  'typeof',
  'var',
  'void',
  'while',
  'with',
  'yield',
  'enum',
  'implements',
  'interface',
  'let',
  'package',
  'private',
  'protected',
  'public',
  'static',
  'await',
  'abstract',
  'boolean',
  'byte',
  'char',
  'double',
  'final',
  'float',
  'goto',
  'int',
  'long',
  'native',
  'short',
  'synchronized',
  'throws',
  'transient',
  'volatile',
  'null',
  'true',
  'false',
  'undefined',
]

export function testParseCode(contents: string): ParsedTextFile {
  const filename = 'code.tsx'
  const result = lintAndParse(filename, contents, null, emptySet(), 'do-not-trim-bounds')
  // Ensure that elements have valid unique IDs if the parse is successful.
  forEachParseSuccess((success) => {
    let uids: Array<string> = []
    fastForEach(success.topLevelElements, (topLevelElement) => {
      if (isUtopiaJSXComponent(topLevelElement)) {
        ensureElementsHaveUID(topLevelElement.rootElement, uids)
        ensureArbitraryJSXBlockCodeHasUIDs(topLevelElement.rootElement)
      }
    })
  }, result)
  return result
}

export function parseThenPrint(filename: string, originalCode: string): string {
  return parseModifyPrint(filename, originalCode, (ps) => ps)
}

export function testParseThenPrint(
  filename: string,
  originalCode: string,
  expectedFinalCode: string,
): void {
  return testParseModifyPrint(filename, originalCode, expectedFinalCode, (ps) => ps)
}

export function testParseThenPrintWithoutUids(
  filename: string,
  originalCode: string,
  expectedFinalCode: string,
): void {
  const printedCode = parseModifyPrint(filename, originalCode, (ps) => ps, true)
  expect(printedCode).toEqual(expectedFinalCode)
}

export function testParseModifyPrint(
  filename: string,
  originalCode: string,
  expectedFinalCode: string,
  transform: (parseSuccess: ParseSuccess) => ParseSuccess,
  stripUids?: boolean,
): void {
  const printedCode = parseModifyPrint(filename, originalCode, transform, stripUids)
  expect(printedCode).toEqual(expectedFinalCode)
}

function parseModifyPrint(
  filename: string,
  originalCode: string,
  transform: (parseSuccess: ParseSuccess) => ParseSuccess,
  stripUids?: boolean,
): string {
  const initialParseResult = testParseCode(originalCode)
  return foldParsedTextFile(
    (failure) => {
      throw new Error(JSON.stringify(failure))
    },
    (initialParseSuccess) => {
      const transformed = transform(initialParseSuccess)
      const printedCode = printCode(
        filename,
        printCodeOptions(false, true, true, stripUids),
        transformed.imports,
        transformed.topLevelElements,
        transformed.jsxFactoryFunction,
        transformed.exportsDetail,
      )
      return printedCode
    },
    (failure) => {
      throw new Error(JSON.stringify(failure))
    },
    initialParseResult,
  )
}

export function clearParseResultUniqueIDsAndEmptyBlocks(
  parseResult: ParsedTextFile,
): ParsedTextFile {
  return mapParsedTextFile((success) => {
    const updatedTopLevelElements = success.topLevelElements.map(
      clearTopLevelElementUniqueIDsAndEmptyBlocks,
    )
    const combinedTopLevelArbitraryBlock: ArbitraryJSBlock | null = optionalMap<
      ArbitraryJSBlock,
      ArbitraryJSBlock
    >(clearTopLevelElementUniqueIDsAndEmptyBlocks, success.combinedTopLevelArbitraryBlock)
    return {
      ...success,
      topLevelElements: updatedTopLevelElements,
      combinedTopLevelArbitraryBlock: combinedTopLevelArbitraryBlock,
    }
  }, parseResult)
}

export function simplifyJSXElementAttributes(element: JSXElement): JSXElement {
  const updatedAttributes = simplifyAttributesIfPossible(element.props)
  const updatedChildren = element.children.map(simplifyJSXElementChildAttributes)
  return {
    ...element,
    props: updatedAttributes,
    children: updatedChildren,
  }
}

export function simplifyJSXElementChildAttributes(element: JSXElementChild): JSXElementChild {
  switch (element.type) {
    case 'JSX_ELEMENT':
      return simplifyJSXElementAttributes(element)
    case 'ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return simplifyAttributeIfPossible(element)
    case 'JSX_FRAGMENT':
      const updatedChildren = element.children.map(simplifyJSXElementChildAttributes)
      return {
        ...element,
        children: updatedChildren,
      }
    case 'JSX_CONDITIONAL_EXPRESSION':
      const updatedCondition = simplifyAttributeIfPossible(element.condition)
      const updatedWhenTrue = simplifyJSXElementChildAttributes(element.whenTrue)
      const updatedWhenFalse = simplifyJSXElementChildAttributes(element.whenFalse)
      return {
        ...element,
        condition: updatedCondition,
        whenTrue: updatedWhenTrue,
        whenFalse: updatedWhenFalse,
      }
    case 'JSX_TEXT_BLOCK':
      return element
    default:
      assertNever(element)
  }
}

export function simplifyElementsWithinAttributes(elementsWithin: ElementsWithin): ElementsWithin {
  return objectMap((element) => simplifyJSXElementAttributes(element), elementsWithin)
}

export function simplifyArbitraryJSBlockAttributes(block: ArbitraryJSBlock): ArbitraryJSBlock {
  const updatedElementsWithin = simplifyElementsWithinAttributes(block.elementsWithin)
  return {
    ...block,
    elementsWithin: updatedElementsWithin,
  }
}

export function simplifyTopLevelElementAttributes(
  topLevelElement: TopLevelElement,
): TopLevelElement {
  switch (topLevelElement.type) {
    case 'UTOPIA_JSX_COMPONENT':
      const updatedRootElement = simplifyJSXElementChildAttributes(topLevelElement.rootElement)
      const updatedArbitraryBlock = optionalMap(
        simplifyArbitraryJSBlockAttributes,
        topLevelElement.arbitraryJSBlock,
      )
      return {
        ...topLevelElement,
        rootElement: updatedRootElement,
        arbitraryJSBlock: updatedArbitraryBlock,
      }
    case 'ARBITRARY_JS_BLOCK':
      return simplifyArbitraryJSBlockAttributes(topLevelElement)
    case 'IMPORT_STATEMENT':
    case 'UNPARSED_CODE':
      return topLevelElement
    default:
      assertNever(topLevelElement)
  }
}

export function simplifyParsedTextFileAttributes(parsedTextFile: ParsedTextFile): ParsedTextFile {
  return mapParsedTextFile((success) => {
    const updatedTopLevelElements = success.topLevelElements.map(simplifyTopLevelElementAttributes)
    const updatedCombinedTopLevelArbitraryBlock = optionalMap(
      simplifyArbitraryJSBlockAttributes,
      success.combinedTopLevelArbitraryBlock,
    )
    return {
      ...success,
      topLevelElements: updatedTopLevelElements,
      combinedTopLevelArbitraryBlock: updatedCombinedTopLevelArbitraryBlock,
    }
  }, parsedTextFile)
}

export const JustImportView: Imports = {
  'utopia-api': {
    importedAs: null,
    importedFromWithin: [importAlias('View')],
    importedWithName: null,
  },
}

export const JustImportViewAndReact: Imports = {
  'utopia-api': {
    importedAs: null,
    importedFromWithin: [importAlias('View')],
    importedWithName: null,
  },
  react: {
    importedAs: null,
    importedFromWithin: [],
    importedWithName: 'React',
  },
}

export const ImportViewImportStatement: ImportStatement = importStatement(
  `import { View } from 'utopia-api'`,
  false,
  false,
  ['View'],
  'utopia-api',
)

export const ImportReactImportStatement: ImportStatement = importStatement(
  `import * as React from 'react'`,
  true,
  false,
  [],
  'react',
)

export function clearErrorMessagePassTime(errorMessage: ErrorMessage): ErrorMessage {
  if (errorMessage.passTime == null) {
    return errorMessage
  } else {
    return {
      ...errorMessage,
      passTime: 12345678,
    }
  }
}

export function clearErrorMessagesPassTimes(
  errorMessages: Array<ErrorMessage>,
): Array<ErrorMessage> {
  return errorMessages.map(clearErrorMessagePassTime)
}

export function clearParseResultPassTimes(parseResult: ParsedTextFile): ParsedTextFile {
  return foldParsedTextFile<ParsedTextFile>(
    (failure) => {
      return {
        ...failure,
        errorMessages: clearErrorMessagesPassTimes(failure.errorMessages),
      }
    },
    (success) => success,
    (unparsedFile) => unparsedFile,
    parseResult,
  )
}

export function lowercaseStringArbitrary(): Arbitrary<string> {
  return FastCheck.array(
    FastCheck.constantFrom(
      ...[
        'a',
        'b',
        'c',
        'd',
        'e',
        'f',
        'g',
        'h',
        'i',
        'j',
        'k',
        'l',
        'm',
        'n',
        'o',
        'p',
        'q',
        'r',
        's',
        't',
        'u',
        'v',
        'w',
        'x',
        'y',
        'z',
      ],
    ),
    1,
    20,
  ).map((charArray) => {
    return charArray.join('')
  })
}

export function propertyPathPartsArbitrary(): Arbitrary<Array<string | number>> {
  return FastCheck.array(
    FastCheck.oneof<string | number>(lowercaseStringArbitrary(), FastCheck.nat()),
    3,
  )
}

export function propertyPathArbitrary(): Arbitrary<PropertyPath> {
  return propertyPathPartsArbitrary().map((pathParts) => {
    return PP.createFromArray(pathParts)
  })
}

export function jsxElementNameArbitrary(): Arbitrary<JSXElementName> {
  return FastCheck.tuple(
    lowercaseStringArbitrary().filter((str) => !JavaScriptReservedKeywords.includes(str)),
    FastCheck.array(lowercaseStringArbitrary(), 3),
    FastCheck.boolean(),
  ).map(([baseVariable, propertyPathParts, uppercaseFirstLetter]) => {
    let adjustedName: string
    if (uppercaseFirstLetter) {
      adjustedName = baseVariable.charAt(0).toUpperCase() + baseVariable.slice(1)
    } else {
      adjustedName = baseVariable
    }
    return jsxElementName(adjustedName, propertyPathParts)
  })
}

export function jsxTextBlockArbitrary(): Arbitrary<JSXTextBlock> {
  return FastCheck.base64String().map((text) => {
    return jsxTextBlock(text)
  })
}

export function jsxArbitraryBlockArbitrary(): Arbitrary<JSXArbitraryBlock> {
  return FastCheck.constant(jsxArbitraryBlock('1 + 2', '1 + 2;', 'return 1 + 2;', [], null, {}))
}

export function jsxAttributeValueArbitrary(): Arbitrary<JSExpressionValue<any>> {
  function validKey(key: string): boolean {
    // Exclude these characters from object keys.
    return !key.includes('"') && !key.includes("'")
  }
  function checkValue(value: unknown): boolean {
    if (typeof value === 'object' && value != null) {
      return Object.keys(value).every(validKey) && Object.values(value).every(checkValue)
    } else if (typeof value === 'string') {
      // Looks like these are being inserted directly and can end up escaping the closing mark for a string.
      return !value.includes('\\')
    } else {
      return true
    }
  }
  const valueArbitrary = FastCheck.jsonObject(2).filter(checkValue)
  return FastCheck.tuple(valueArbitrary, arbitraryMultiLineComments()).map(([value, comments]) =>
    jsExpressionValue(value, comments),
  )
}

export function jsxAttributeOtherJavaScriptArbitrary(): Arbitrary<JSExpressionOtherJavaScript> {
  return FastCheck.constant(jsExpressionOtherJavaScript('1 + 2', '1 + 2', [], null, {}))
}

export function jsxArrayValueArbitrary(depth: number): Arbitrary<JSXArrayValue> {
  return FastCheck.tuple(jsxAttributeArbitrary(depth), arbitraryMultiLineComments()).map(
    ([array, comments]) => jsxArrayValue(array, comments),
  )
}

export function jsxArraySpreadArbitrary(depth: number): Arbitrary<JSXArraySpread> {
  return FastCheck.tuple(jsxAttributeArbitrary(depth), arbitraryMultiLineComments()).map(
    ([array, comments]) => jsxArraySpread(array, comments),
  )
}

export function jsxArrayElementArbitrary(depth: number): Arbitrary<JSXArrayElement> {
  return FastCheck.oneof<JSXArrayElement>(
    jsxArrayValueArbitrary(depth),
    jsxArraySpreadArbitrary(depth),
  )
}

export function jsxAttributeNestedArrayArbitrary(
  depth: number,
): Arbitrary<JSExpressionNestedArray> {
  return FastCheck.tuple(
    FastCheck.array(jsxArrayElementArbitrary(depth - 1), 3),
    arbitraryMultiLineComments(),
  ).map(([array, comments]) => jsExpressionNestedArray(array, comments))
}

export function jsxPropertyAssignmentArbitrary(depth: number): Arbitrary<JSXPropertyAssignment> {
  return FastCheck.tuple(lowercaseStringArbitrary(), jsxAttributeArbitrary(depth)).map(
    ([key, attribute]) => {
      return jsxPropertyAssignment(key, attribute, emptyComments, emptyComments)
    },
  )
}

export function jsxSpreadAssignmentArbitrary(depth: number): Arbitrary<JSXSpreadAssignment> {
  return FastCheck.tuple(jsxAttributeArbitrary(depth), arbitraryMultiLineComments()).map(
    ([spread, comments]) => jsxSpreadAssignment(spread, comments),
  )
}

export function jsxPropertyArbitrary(depth: number): Arbitrary<JSXProperty> {
  return FastCheck.oneof<JSXProperty>(
    jsxPropertyAssignmentArbitrary(depth),
    jsxSpreadAssignmentArbitrary(depth),
  )
}

export function jsxAttributeNestedObjectArbitrary(
  depth: number,
): Arbitrary<JSExpressionNestedObject> {
  return FastCheck.tuple(
    FastCheck.array(jsxPropertyArbitrary(depth - 1), 3),
    arbitraryMultiLineComments(),
  ).map(([values, comments]) => jsExpressionNestedObject(values, comments))
}

export function jsxAttributeFunctionCallArbitrary(
  depth: number,
): Arbitrary<JSExpressionFunctionCall> {
  return FastCheck.tuple(
    lowercaseStringArbitrary(),
    FastCheck.array(jsxAttributeArbitrary(depth - 1), 3),
  ).map(([functionName, functionArguments]) => {
    return jsExpressionFunctionCall(functionName, functionArguments)
  })
}

export function jsxAttributeArbitrary(depth: number): Arbitrary<JSExpression> {
  if (depth <= 1) {
    return FastCheck.oneof<JSExpression>(
      jsxAttributeValueArbitrary(),
      jsxAttributeOtherJavaScriptArbitrary(),
    )
  } else {
    return FastCheck.oneof<JSExpression>(
      jsxAttributeValueArbitrary(),
      jsxAttributeOtherJavaScriptArbitrary(),
      jsxAttributeNestedArrayArbitrary(depth),
      jsxAttributeNestedObjectArbitrary(depth),
      jsxAttributeFunctionCallArbitrary(depth),
    )
  }
}

export function flatObjectArbitrary<V>(
  keys: Arbitrary<string>,
  values: Arbitrary<V>,
): Arbitrary<MapLike<V>> {
  // FastCheck.object is a load of rubbish.
  const pairsArbitrary = FastCheck.array(FastCheck.tuple(keys, values), 3)
  return pairsArbitrary.map((pairs) => {
    return pairs.reduce((working, [key, value]) => {
      return {
        ...working,
        [key]: value,
      }
    }, {})
  })
}

export function jsxAttributesArbitrary(): Arbitrary<JSXAttributes> {
  return flatObjectArbitrary(lowercaseStringArbitrary(), jsxAttributeArbitrary(3)).map(
    jsxAttributesFromMap,
  )
}

export function jsxElementArbitrary(depth: number): Arbitrary<JSXElement> {
  return FastCheck.tuple(
    jsxElementNameArbitrary(),
    lowercaseStringArbitrary().filter((str) => !JavaScriptReservedKeywords.includes(str)),
    jsxAttributesArbitrary(),
    FastCheck.array(jsxElementChildArbitrary(depth - 1), 3),
  ).map(([elementName, elementUID, elementAttributes, elementChildren]) => {
    return jsxElement(elementName, elementUID, elementAttributes, elementChildren)
  })
}

export function jsxFragmentArbitrary(depth: number): Arbitrary<JSXFragment> {
  return FastCheck.tuple(
    lowercaseStringArbitrary().filter((str) => !JavaScriptReservedKeywords.includes(str)),
    FastCheck.array(jsxElementChildArbitrary(depth - 1), 3),
    FastCheck.boolean(),
  ).map(([uid, children, longForm]) => {
    return jsxFragment(uid, children, longForm)
  })
}

export function jsxConditionalExpressionArbitrary(
  depth: number,
): Arbitrary<JSXConditionalExpression> {
  return FastCheck.tuple(
    lowercaseStringArbitrary().filter((str) => !JavaScriptReservedKeywords.includes(str)),
    FastCheck.oneof(FastCheck.constant('1 === 2'), FastCheck.constant('1 === 1')),
    jsxAttributeArbitrary(3),
    jsxElementChildArbitrary(depth - 1).filter((c) => c.type !== 'JSX_TEXT_BLOCK'),
    jsxElementChildArbitrary(depth - 1).filter((c) => c.type !== 'JSX_TEXT_BLOCK'),
    FastCheck.constant(emptyComments),
  ).map(([elementUID, originalConditionString, condition, whenTrue, whenFalse, comments]) => {
    return jsxConditionalExpression(
      elementUID,
      condition,
      originalConditionString,
      whenTrue,
      whenFalse,
      comments,
    )
  })
}

export function jsxElementChildArbitrary(depth: number): Arbitrary<JSXElementChild> {
  if (depth <= 1) {
    return FastCheck.oneof<JSXElementChild>(
      jsxArbitraryBlockArbitrary(),
      jsxTextBlockArbitrary(),
      jsxAttributeArbitrary(3),
    )
  } else {
    return FastCheck.oneof<JSXElementChild>(
      jsxElementArbitrary(depth),
      jsxArbitraryBlockArbitrary(),
      jsxTextBlockArbitrary(),
      jsxConditionalExpressionArbitrary(depth),
      jsxFragmentArbitrary(depth),
      jsxAttributeArbitrary(3),
    )
  }
}

export function arbitraryJSBlockArbitrary(): Arbitrary<ArbitraryJSBlock> {
  return FastCheck.constant(arbitraryJSBlock('1 + 2', '1 + 2', [], [], null, {}))
}

export function arbitraryComments(): Arbitrary<ParsedComments> {
  return FastCheck.tuple(
    FastCheck.array(commentArbitrary, 3),
    FastCheck.array(commentArbitrary, 3),
  ).map(([leadingComments, trailingComments]) => parsedComments(leadingComments, trailingComments))
}

export function arbitraryMultiLineComments(): Arbitrary<ParsedComments> {
  return FastCheck.tuple(
    FastCheck.array(multiLineCommentArbitrary, 3),
    FastCheck.array(multiLineCommentArbitrary, 3),
  ).map(([leadingComments, trailingComments]) => parsedComments(leadingComments, trailingComments))
}

export function arbitraryDeclarationSyntax(): Arbitrary<FunctionDeclarationSyntax> {
  return FastCheck.oneof<FunctionDeclarationSyntax>(
    FastCheck.constant('function'),
    FastCheck.constant('var'),
    FastCheck.constant('let'),
    FastCheck.constant('const'),
  )
}

export function arbitraryBlockOrExpression(): Arbitrary<BlockOrExpression> {
  return FastCheck.oneof<BlockOrExpression>(
    FastCheck.constant('block'),
    FastCheck.constant('expression'),
  )
}

export function utopiaJSXComponentArbitrary(): Arbitrary<UtopiaJSXComponent> {
  return FastCheck.tuple(
    lowercaseStringArbitrary().filter((str) => !JavaScriptReservedKeywords.includes(str)),
    FastCheck.boolean(),
    arbitraryDeclarationSyntax(),
    arbitraryBlockOrExpression(),
    jsxElementArbitrary(3),
    arbitraryJSBlockArbitrary(),
    arbitraryComments(),
  )
    .map(
      ([
        name,
        isFunction,
        declarationSyntax,
        blockOrExpression,
        rootElement,
        jsBlock,
        returnStatementComments,
      ]) => {
        return utopiaJSXComponent(
          name,
          isFunction,
          declarationSyntax,
          blockOrExpression,
          defaultPropsParam,
          [],
          rootElement,
          jsBlock,
          false,
          returnStatementComments,
        )
      },
    )
    .filter((component) => {
      // Prevent creating a component that depends on itself.
      let elementNames: Array<string> = []
      walkElements(component.rootElement, 'do-not-include-data-uid-attribute', (elem) => {
        if (isJSXElement(elem)) {
          elementNames.push(elem.name.baseVariable)
        }
      })
      return !elementNames.some((elementName) => elementName === component.name)
    })
}

export function topLevelElementArbitrary(): Arbitrary<TopLevelElement> {
  return FastCheck.oneof<TopLevelElement>(
    arbitraryJSBlockArbitrary(),
    utopiaJSXComponentArbitrary(),
  )
}

export function exportVariablesArbitrary(possibleNames: Array<string>): Arbitrary<ExportVariables> {
  return FastCheck.array(
    FastCheck.constantFrom(...possibleNames).map((name) => exportVariable(name, null)),
  ).map(exportVariables)
}

export function exportDetailArbitrary(possibleNames: Array<string>): Arbitrary<ExportDetail> {
  return FastCheck.oneof<ExportDetail>(exportVariablesArbitrary(possibleNames))
}

export function exportsDetailArbitrary(possibleNames: Array<string>): Arbitrary<ExportsDetail> {
  if (possibleNames.length === 0) {
    return FastCheck.constant(EmptyExportsDetail)
  } else {
    return FastCheck.array(exportDetailArbitrary(possibleNames))
  }
}

type IncludeDataUIDAttribute = 'include-data-uid-attribute' | 'do-not-include-data-uid-attribute'

function walkElementsWithin(
  elementsWithin: ElementsWithin,
  includeDataUIDAttribute: IncludeDataUIDAttribute,
  walkWith: (elem: JSXElementChild) => void,
): void {
  fastForEach(Object.keys(elementsWithin), (elementWithinKey) => {
    const innerElement = elementsWithin[elementWithinKey]
    walkElements(innerElement, includeDataUIDAttribute, walkWith)
  })
}

function walkElements(
  jsxElementChild: JSXElementChild,
  includeDataUIDAttribute: IncludeDataUIDAttribute,
  walkWith: (elem: JSXElementChild) => void,
): void {
  walkWith(jsxElementChild)
  switch (jsxElementChild.type) {
    case 'JSX_ELEMENT':
      walkJSXAttributes(jsxElementChild.props, includeDataUIDAttribute, walkWith)
      fastForEach(jsxElementChild.children, (child) => {
        walkElements(child, includeDataUIDAttribute, walkWith)
      })
      break
    case 'JSX_TEXT_BLOCK':
      break
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      walkElementsWithin(jsxElementChild.elementsWithin, includeDataUIDAttribute, walkWith)
      break
    case 'JSX_FRAGMENT':
      fastForEach(jsxElementChild.children, (child) => {
        walkElements(child, includeDataUIDAttribute, walkWith)
      })
      break
    case 'JSX_CONDITIONAL_EXPRESSION':
      walkElements(jsxElementChild.condition, includeDataUIDAttribute, walkWith)
      walkElements(jsxElementChild.whenTrue, includeDataUIDAttribute, walkWith)
      walkElements(jsxElementChild.whenFalse, includeDataUIDAttribute, walkWith)
      break
    case 'ATTRIBUTE_VALUE':
      break
    case 'ATTRIBUTE_NESTED_ARRAY':
      fastForEach(jsxElementChild.content, (contentElement) => {
        walkElements(contentElement.value, includeDataUIDAttribute, walkWith)
      })
      break
    case 'ATTRIBUTE_NESTED_OBJECT':
      fastForEach(jsxElementChild.content, (contentElement) => {
        walkElements(contentElement.value, includeDataUIDAttribute, walkWith)
      })
      break
    case 'ATTRIBUTE_FUNCTION_CALL':
      fastForEach(jsxElementChild.parameters, (param) => {
        walkElements(param, includeDataUIDAttribute, walkWith)
      })
      break
    default:
      const _exhaustiveCheck: never = jsxElementChild
      throw new Error(`Unhandled type ${JSON.stringify(jsxElementChild)}`)
  }
}

function walkJSXAttributes(
  jsxAttributes: JSXAttributes,
  includeDataUIDAttribute: IncludeDataUIDAttribute,
  walkWith: (elem: JSXElementChild) => void,
): void {
  for (const attributeEntry of jsxAttributes) {
    switch (attributeEntry.type) {
      case 'JSX_ATTRIBUTES_ENTRY':
        const isDataUID = attributeEntry.key === UTOPIA_UID_KEY
        if (!isDataUID || includeDataUIDAttribute === 'include-data-uid-attribute') {
          walkJSXElementChild(attributeEntry.value, includeDataUIDAttribute, walkWith)
        }
        break
      case 'JSX_ATTRIBUTES_SPREAD':
        walkJSXElementChild(attributeEntry.spreadValue, includeDataUIDAttribute, walkWith)
        break
      default:
        assertNever(attributeEntry)
    }
  }
}

function walkJSXElementChild(
  jsxElementChild: JSXElementChild,
  includeDataUIDAttribute: IncludeDataUIDAttribute,
  walkWith: (elem: JSXElementChild) => void,
): void {
  walkWith(jsxElementChild)
  switch (jsxElementChild.type) {
    case 'JSX_ELEMENT':
      fastForEach(jsxElementChild.children, (child) => {
        walkJSXElementChild(child, includeDataUIDAttribute, walkWith)
      })
      walkJSXAttributes(jsxElementChild.props, includeDataUIDAttribute, walkWith)
      break
    case 'JSX_TEXT_BLOCK':
      break
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      fastForEach(Object.keys(jsxElementChild.elementsWithin), (elementWithinKey) => {
        const innerElement = jsxElementChild.elementsWithin[elementWithinKey]
        walkJSXElementChild(innerElement, includeDataUIDAttribute, walkWith)
      })
      break
    case 'JSX_FRAGMENT':
      fastForEach(jsxElementChild.children, (child) => {
        walkJSXElementChild(child, includeDataUIDAttribute, walkWith)
      })
      break
    case 'JSX_CONDITIONAL_EXPRESSION':
      walkJSXElementChild(jsxElementChild.condition, includeDataUIDAttribute, walkWith)
      walkJSXElementChild(jsxElementChild.whenTrue, includeDataUIDAttribute, walkWith)
      walkJSXElementChild(jsxElementChild.whenFalse, includeDataUIDAttribute, walkWith)
      break
    case 'ATTRIBUTE_VALUE':
      break
    case 'ATTRIBUTE_NESTED_ARRAY':
      fastForEach(jsxElementChild.content, (contentElement) => {
        walkJSXElementChild(contentElement.value, includeDataUIDAttribute, walkWith)
      })
      break
    case 'ATTRIBUTE_NESTED_OBJECT':
      fastForEach(jsxElementChild.content, (contentElement) => {
        walkJSXElementChild(contentElement.value, includeDataUIDAttribute, walkWith)
      })
      break
    case 'ATTRIBUTE_FUNCTION_CALL':
      fastForEach(jsxElementChild.parameters, (param) => {
        walkJSXElementChild(param, includeDataUIDAttribute, walkWith)
      })
      break
    default:
      const _exhaustiveCheck: never = jsxElementChild
      throw new Error(`Unhandled type ${JSON.stringify(jsxElementChild)}`)
  }
}

function getAllBaseVariables(jsxElementChild: JSXElementChild): Array<string> {
  let result: Array<string> = []
  walkElements(jsxElementChild, 'do-not-include-data-uid-attribute', (element) => {
    if (isJSXElement(element)) {
      result = addUniquely(result, element.name.baseVariable)
    }
  })
  return result
}

function checkUID(
  uid: string,
  valueHoldingUID: JSXElementChild | ArbitraryJSBlock,
  uids: Array<string>,
): void {
  if (uid === '') {
    throw new Error(`Blank UID in in ${JSON.stringify(valueHoldingUID)}`)
  } else if (uids.includes(uid)) {
    throw new Error(`UID ${uid} is duplicated in ${JSON.stringify(valueHoldingUID)}`)
  } else {
    uids.push(uid)
  }
}

export function ensureArbitraryBlocksHaveUID(
  arbitraryBlock: ArbitraryJSBlock,
  uids: Array<string>,
): void {
  checkUID(arbitraryBlock.uid, arbitraryBlock, uids)
  walkElementsWithin(
    arbitraryBlock.elementsWithin,
    'do-not-include-data-uid-attribute',
    (element) => {
      const uid = getUtopiaIDFromJSXElement(element)
      checkUID(uid, element, uids)
    },
  )
}

export function ensureElementsHaveUID(jsxElementChild: JSXElementChild, uids: Array<string>): void {
  walkElements(jsxElementChild, 'do-not-include-data-uid-attribute', (element) => {
    // Relies on this function blowing out for anything that doesn't have a valid one.
    const uid = getUtopiaIDFromJSXElement(element)
    checkUID(uid, element, uids)
  })
}

function babelCheckForDataUID(): { visitor: BabelTraverse.Visitor } {
  return {
    visitor: {
      JSXElement(path) {
        const pathNode = path.node
        let hasValidUID: boolean = false
        for (const attribute of pathNode.openingElement.attributes) {
          if (BabelTypes.isJSXAttribute(attribute)) {
            if (BabelTypes.isJSXIdentifier(attribute.name)) {
              if (attribute.name.name === 'data-uid') {
                if (BabelTypes.isJSXExpressionContainer(attribute.value)) {
                  if (BabelTypes.isStringLiteral(attribute.value.expression)) {
                    hasValidUID = true
                  }
                } else {
                  if (BabelTypes.isStringLiteral(attribute.value)) {
                    hasValidUID = true
                  }
                }
              }
            }
          }
        }
        if (!hasValidUID) {
          throw new Error(
            `Element has no valid data-uid attribute: ${path.getSource().substring(0, 100)}`,
          )
        }
      },
    },
  }
}

export function ensureArbitraryJSXBlockCodeHasUIDs(jsxElementChild: JSXElementChild): void {
  walkJSXElementChild(jsxElementChild, 'do-not-include-data-uid-attribute', (element) => {
    if (isJSExpressionOtherJavaScript(element)) {
      const plugins: Array<any> = [ReactSyntaxPlugin, babelCheckForDataUID]

      Babel.transform(element.javascript, {
        presets: [],
        plugins: plugins,
        sourceType: 'script',
      })
    }
  })
}

export interface PrintableProjectContent {
  imports: Imports
  topLevelElements: Array<TopLevelElement>
  projectContainedOldSceneMetadata: boolean
  jsxFactoryFunction: string | null
  exportsDetail: ExportsDetail
}

function getTopLevelElementVariableNames(topLevelElement: TopLevelElement): Array<string> {
  switch (topLevelElement.type) {
    case 'UTOPIA_JSX_COMPONENT':
      return topLevelElement.name == null ? [] : [topLevelElement.name]
    case 'IMPORT_STATEMENT':
      return topLevelElement.imports
    case 'ARBITRARY_JS_BLOCK':
    case 'UNPARSED_CODE':
      return []
    default:
      const _exhaustiveCheck: never = topLevelElement
      throw new Error(`Unhandled type ${JSON.stringify(topLevelElement)}`)
  }
}

function areTopLevelElementsValid(topLevelElements: Array<TopLevelElement>): boolean {
  let variableNames: Array<string> = []
  for (const topLevelElement of topLevelElements) {
    variableNames.push(...getTopLevelElementVariableNames(topLevelElement))
  }
  return variableNames.length === new Set(variableNames).size
}

export function printableProjectContentArbitrary(): Arbitrary<PrintableProjectContent> {
  return FastCheck.tuple(
    FastCheck.array(topLevelElementArbitrary(), 3).filter(areTopLevelElementsValid),
    FastCheck.option(lowercaseStringArbitrary()),
    FastCheck.boolean(),
  ).chain(([topLevelElements, jsxFactoryFunction, projectContainedOldSceneMetadata]) => {
    const possibleNames = flatMapArray((topLevelElement) => {
      if (isUtopiaJSXComponent(topLevelElement)) {
        if (topLevelElement.isFunction) {
          return []
        } else {
          if (topLevelElement.name == null) {
            return []
          } else {
            return [topLevelElement.name]
          }
        }
      } else {
        return []
      }
    }, topLevelElements)
    return exportsDetailArbitrary(possibleNames).map((detailOfExports) => {
      const allBaseVariables = flatMapArray((topLevelElement) => {
        switch (topLevelElement.type) {
          case 'UTOPIA_JSX_COMPONENT':
            return getAllBaseVariables(topLevelElement.rootElement)
          case 'ARBITRARY_JS_BLOCK':
          case 'IMPORT_STATEMENT':
          case 'UNPARSED_CODE':
            return []
          default:
            const _exhaustiveCheck: never = topLevelElement
            throw new Error(`Unhandled type ${JSON.stringify(topLevelElement)}`)
        }
      }, topLevelElements)
      const imports: Imports = allBaseVariables.reduce((workingImports, baseVariable) => {
        const componentExistsInFile = topLevelElements.some((element) => {
          if (isUtopiaJSXComponent(element)) {
            return element.name === baseVariable
          } else {
            return false
          }
        })
        // Don't create an import for something where there's a component with the same name in this file.
        if (componentExistsInFile) {
          return workingImports
        } else {
          return addImport('code.jsx', 'testlib', baseVariable, [], null, workingImports)
        }
      }, JustImportViewAndReact)
      return {
        imports: imports,
        topLevelElements: topLevelElements,
        projectContainedOldSceneMetadata: projectContainedOldSceneMetadata,
        jsxFactoryFunction: jsxFactoryFunction,
        exportsDetail: detailOfExports,
      }
    })
  })
}

export function elementsStructure(topLevelElements: Array<TopLevelElement>): string {
  let structureResults: Array<string> = []
  for (const topLevelElement of topLevelElements) {
    let elementResult: string = topLevelElement.type
    if (isUtopiaJSXComponent(topLevelElement)) {
      elementResult += ` - ${topLevelElement.name}`
    }
    structureResults.push(elementResult)
    if (isUtopiaJSXComponent(topLevelElement)) {
      const emptyPath = [] as any as StaticElementPathPart
      walkElement(topLevelElement.rootElement, emptyPath, 1, (innerElement, path, depth) => {
        let innerElementResult: string = ''
        for (let index = 0; index < depth; index++) {
          innerElementResult += '  '
        }
        innerElementResult += innerElement.type
        if (isJSXElement(innerElement)) {
          innerElementResult += ` - ${getJSXElementNameAsString(innerElement.name)} - ${getUtopiaID(
            innerElement,
          )}`
        } else if (isJSXConditionalExpression(innerElement)) {
          innerElementResult += ` - ${getUtopiaID(innerElement)}`
        }
        structureResults.push(innerElementResult)
      })
    }
  }
  return structureResults.join('\n')
}

export function forceParseSuccessFromFileOrFail(
  file: ProjectFile | null | undefined,
): ParseSuccess {
  if (file != null && isTextFile(file)) {
    if (isParseSuccess(file.fileContents.parsed)) {
      return file.fileContents.parsed
    } else {
      throw new Error(`Not a parse success ${file.fileContents.parsed}`)
    }
  } else {
    throw new Error(`Not a text file ${file}`)
  }
}

export function clearTopLevelElementUniqueIDsAndEmptyBlocks(
  element: UtopiaJSXComponent,
): UtopiaJSXComponent
export function clearTopLevelElementUniqueIDsAndEmptyBlocks(
  element: ArbitraryJSBlock,
): ArbitraryJSBlock
export function clearTopLevelElementUniqueIDsAndEmptyBlocks(
  element: TopLevelElement,
): TopLevelElement
export function clearTopLevelElementUniqueIDsAndEmptyBlocks(
  element: TopLevelElement,
): TopLevelElement {
  const withoutUID = clearTopLevelElementUniqueIDs(element)
  switch (withoutUID.type) {
    case 'UTOPIA_JSX_COMPONENT': {
      const blockCode = (withoutUID.arbitraryJSBlock?.javascript ?? '').trim()
      const blockIsEmpty = blockCode.length === 0
      return {
        ...withoutUID,
        arbitraryJSBlock:
          blockIsEmpty || withoutUID.arbitraryJSBlock == null
            ? null
            : {
                ...withoutUID.arbitraryJSBlock,
                javascript: blockCode,
              },
      }
    }
    case 'ARBITRARY_JS_BLOCK': {
      const blockCode = (withoutUID.javascript ?? '').trim()
      return {
        ...withoutUID,
        javascript: blockCode,
      }
    }
    case 'IMPORT_STATEMENT':
    case 'UNPARSED_CODE':
      return withoutUID
    default:
      const _exhaustiveCheck: never = withoutUID
      throw new Error(`Unhandled element ${JSON.stringify(withoutUID)}`)
  }
}
