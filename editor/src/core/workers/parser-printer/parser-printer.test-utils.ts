import * as Babel from '@babel/standalone'
import type * as BabelTraverse from '@babel/traverse'
import * as BabelTypes from '@babel/types'
import ReactSyntaxPlugin from 'babel-plugin-syntax-jsx'
import * as FastCheck from 'fast-check'
import type { Arbitrary } from 'fast-check'
import type { MapLike } from 'typescript'
import * as PP from '../../shared/property-path'
import type {
  ArbitraryJSBlock,
  JSXArrayElement,
  JSXArraySpread,
  JSXArrayValue,
  JSExpression,
  JSExpressionFunctionCall,
  JSExpressionNestedArray,
  JSExpressionNestedObject,
  JSExpressionOtherJavaScript,
  JSXAttributes,
  JSExpressionValue,
  JSXElement,
  JSXElementChild,
  JSXElementName,
  JSXProperty,
  JSXPropertyAssignment,
  JSXSpreadAssignment,
  JSXTextBlock,
  TopLevelElement,
  UtopiaJSXComponent,
  SingleLineComment,
  MultiLineComment,
  Comment,
  FunctionDeclarationSyntax,
  BlockOrExpression,
  ImportStatement,
  ParsedComments,
  JSXConditionalExpression,
  JSXFragment,
  ElementsWithin,
  JSXElementLike,
} from '../../shared/element-template'
import {
  arbitraryJSBlock,
  clearTopLevelElementUniqueIDs,
  isJSExpressionMapOrOtherJavaScript,
  isUtopiaJSXComponent,
  jsxArraySpread,
  jsxArrayValue,
  jsExpressionFunctionCall,
  jsExpressionNestedArray,
  jsExpressionNestedObject,
  jsExpressionOtherJavaScript,
  jsExpressionValue,
  jsxElement,
  jsxElementName,
  jsxPropertyAssignment,
  jsxSpreadAssignment,
  jsxTextBlock,
  utopiaJSXComponent,
  defaultPropsParam,
  walkElement,
  getJSXElementNameAsString,
  isJSXElement,
  jsxAttributesFromMap,
  importStatement,
  emptyComments,
  parsedComments,
  isJSXConditionalExpression,
  jsxConditionalExpression,
  jsxFragment,
  simplifyAttributeIfPossible,
  simplifyAttributesIfPossible,
  isJSXFragment,
  jsxAttributesEntry,
  clearTopLevelElementSourceMaps,
  clearArbitraryJSBlockSourceMaps,
} from '../../shared/element-template'
import { addImport } from '../common/project-file-utils'
import type { ErrorMessage } from '../../shared/error-messages'
import type {
  Imports,
  ParsedTextFile,
  ParseSuccess,
  PropertyPath,
  ExportsDetail,
  ExportDetail,
  StaticElementPathPart,
  ProjectFile,
  ExportVariables,
} from '../../shared/project-file-types'
import {
  importAlias,
  foldParsedTextFile,
  mapParsedTextFile,
  forEachParseSuccess,
  EmptyExportsDetail,
  isParseSuccess,
  isTextFile,
  exportVariable,
  exportVariables,
} from '../../shared/project-file-types'
import { lintAndParse, printCode, printCodeOptions } from './parser-printer'
import { atoz, getUtopiaID, getUtopiaIDFromJSXElement } from '../../shared/uid-utils'
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

export function testParseCode(
  contents: string,
  alreadyExistingUIDs: Set<string> = emptySet(),
): ParsedTextFile {
  const filename = 'code.tsx'
  const result = lintAndParse(filename, [], contents, 'trim-bounds', 'do-not-apply-steganography')
  // Ensure that elements have valid unique IDs if the parse is successful.
  forEachParseSuccess((success) => {
    let uids: Array<string> = []
    fastForEach(success.topLevelElements, (topLevelElement) => {
      if (isUtopiaJSXComponent(topLevelElement)) {
        ensureElementsHaveUID(topLevelElement.rootElement, uids, () => true, 'walk-attributes')
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

export function clearParseResultSourceMapsUniqueIDsAndEmptyBlocks(
  parseResult: ParsedTextFile,
): ParsedTextFile {
  return mapParsedTextFile((success) => {
    const updatedTopLevelElements = success.topLevelElements.map(
      clearTopLevelElementSourceMapsUniqueIDsAndEmptyBlocks,
    )
    const combinedTopLevelArbitraryBlock: ArbitraryJSBlock | null = optionalMap<
      ArbitraryJSBlock,
      ArbitraryJSBlock
    >(clearTopLevelElementSourceMapsUniqueIDsAndEmptyBlocks, success.combinedTopLevelArbitraryBlock)
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

export function simplifyJSXFragmentAttributes(element: JSXFragment): JSXFragment {
  const updatedChildren = element.children.map(simplifyJSXElementChildAttributes)
  return {
    ...element,
    children: updatedChildren,
  }
}

export function simplifyJSXElementLikeAttributes(element: JSXElementLike): JSXElementLike {
  switch (element.type) {
    case 'JSX_ELEMENT':
      return simplifyJSXElementAttributes(element)
    case 'JSX_FRAGMENT':
      return simplifyJSXFragmentAttributes(element)
    default:
      assertNever(element)
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
    case 'JSX_MAP_EXPRESSION':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'JS_IDENTIFIER':
    case 'JS_ELEMENT_ACCESS':
    case 'JS_PROPERTY_ACCESS':
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
  return objectMap(simplifyJSXElementLikeAttributes, elementsWithin)
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

export function uidArbitrary(): Arbitrary<string> {
  return FastCheck.tuple(
    FastCheck.constantFrom(...atoz.slice(0, 10)),
    FastCheck.constantFrom(...atoz.slice(0, 10)),
    FastCheck.constantFrom(...atoz.slice(0, 10)),
  ).map(([first, second, third]) => {
    return `${first}${second}${third}`
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
  return FastCheck.tuple(FastCheck.base64String(), uidArbitrary()).map(([text, uid]) => {
    return jsxTextBlock(text, uid)
  })
}

export function jsxArbitraryBlockArbitrary(): Arbitrary<JSExpression> {
  return uidArbitrary().chain((uid) =>
    FastCheck.constant(
      jsExpressionOtherJavaScript(
        [],
        '1 + 2',
        '1 + 2;',
        'return 1 + 2;',
        [],
        null,
        {},
        emptyComments,
        uid,
      ),
    ),
  )
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
  return FastCheck.tuple(valueArbitrary, arbitraryMultiLineComments(), uidArbitrary()).map(
    ([value, comments, uid]) => jsExpressionValue(value, comments, uid),
  )
}

export function jsxAttributeOtherJavaScriptArbitrary(): Arbitrary<JSExpressionOtherJavaScript> {
  return uidArbitrary().chain((uid) =>
    FastCheck.constant(
      jsExpressionOtherJavaScript([], '1 + 2', '1 + 2', '1 + 2', [], null, {}, emptyComments, uid),
    ),
  )
}

export function jsxArrayValueArbitrary(depth: number): Arbitrary<JSXArrayValue> {
  return FastCheck.tuple(
    jsxAttributeArbitrary(depth),
    arbitraryMultiLineComments(),
    uidArbitrary(),
  ).map(([array, comments]) => jsxArrayValue(array, comments))
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
    uidArbitrary(),
  ).map(([array, comments, uid]) => jsExpressionNestedArray(array, comments, uid))
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
    uidArbitrary(),
  ).map(([values, comments, uid]) => jsExpressionNestedObject(values, comments, uid))
}

export function jsxAttributeFunctionCallArbitrary(
  depth: number,
): Arbitrary<JSExpressionFunctionCall> {
  return FastCheck.tuple(
    lowercaseStringArbitrary(),
    FastCheck.array(jsxAttributeArbitrary(depth - 1), 3),
    uidArbitrary(),
  ).map(([functionName, functionArguments, uid]) => {
    return jsExpressionFunctionCall(functionName, functionArguments, uid)
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
  return FastCheck.tuple(
    flatObjectArbitrary(lowercaseStringArbitrary(), jsxAttributeArbitrary(3)).map(
      jsxAttributesFromMap,
    ),
    uidArbitrary(),
    uidArbitrary(),
  ).map(([baseAttributes, dataUID, dataUIDValueUID]) => {
    return [
      ...baseAttributes,
      jsxAttributesEntry(
        UTOPIA_UID_KEY,
        jsExpressionValue(dataUID, emptyComments, dataUIDValueUID),
        emptyComments,
      ),
    ]
  })
}

export function jsxElementArbitrary(depth: number): Arbitrary<JSXElement> {
  return FastCheck.tuple(
    jsxElementNameArbitrary(),
    uidArbitrary(),
    jsxAttributesArbitrary(),
    FastCheck.array(jsxElementChildArbitrary(depth - 1), 3),
  ).map(([elementName, elementUID, elementAttributes, elementChildren]) => {
    return jsxElement(elementName, elementUID, elementAttributes, elementChildren)
  })
}

export function jsxFragmentArbitrary(depth: number): Arbitrary<JSXFragment> {
  return FastCheck.tuple(
    uidArbitrary(),
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
    uidArbitrary(),
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
  return FastCheck.oneof(
    FastCheck.constant(arbitraryJSBlock([], '1 + 2;', '1 + 2', [], [], null, {}, [])),
    FastCheck.constant(arbitraryJSBlock([], ' \n ', ' \n ', [], [], null, {}, [])),
  )
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
    FastCheck.oneof<ArbitraryJSBlock | null>(FastCheck.constant(null), arbitraryJSBlockArbitrary()),
    arbitraryMultiLineComments(),
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
          [],
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
      walkElements(
        component.rootElement,
        'do-not-include-data-uid-attribute',
        (elem) => {
          if (isJSXElement(elem)) {
            elementNames.push(elem.name.baseVariable)
          }
        },
        () => true,
        'walk-attributes',
      )
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
type ShouldWalkAttributes = 'walk-attributes' | 'do-not-walk-attributes'

function walkElementsWithin(
  elementsWithin: ElementsWithin,
  includeDataUIDAttribute: IncludeDataUIDAttribute,
  walkWith: (elem: JSXElementChild) => void,
  shouldWalkElement: (elem: JSXElementChild) => boolean,
  shouldWalkAttributes: ShouldWalkAttributes,
): void {
  fastForEach(Object.keys(elementsWithin), (elementWithinKey) => {
    const innerElement = elementsWithin[elementWithinKey]
    walkElements(
      innerElement,
      includeDataUIDAttribute,
      walkWith,
      shouldWalkElement,
      shouldWalkAttributes,
    )
  })
}

function walkElements(
  jsxElementChild: JSXElementChild,
  includeDataUIDAttribute: IncludeDataUIDAttribute,
  walkWith: (elem: JSXElementChild) => void,
  shouldWalkElement: (elem: JSXElementChild) => boolean,
  shouldWalkAttributes: ShouldWalkAttributes,
): void {
  if (!shouldWalkElement(jsxElementChild)) {
    return
  }

  walkWith(jsxElementChild)
  switch (jsxElementChild.type) {
    case 'JSX_ELEMENT':
      if (shouldWalkAttributes === 'walk-attributes') {
        walkJSXAttributes(
          jsxElementChild.props,
          includeDataUIDAttribute,
          walkWith,
          shouldWalkElement,
        )
      }
      fastForEach(jsxElementChild.children, (child) => {
        walkElements(
          child,
          includeDataUIDAttribute,
          walkWith,
          shouldWalkElement,
          shouldWalkAttributes,
        )
      })
      break
    case 'JSX_TEXT_BLOCK':
      break
    case 'JSX_MAP_EXPRESSION':
      walkElements(
        jsxElementChild.valueToMap,
        includeDataUIDAttribute,
        walkWith,
        shouldWalkElement,
        shouldWalkAttributes,
      )
      walkElements(
        jsxElementChild.mapFunction,
        includeDataUIDAttribute,
        walkWith,
        shouldWalkElement,
        shouldWalkAttributes,
      )
      break
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      walkElementsWithin(
        jsxElementChild.elementsWithin,
        includeDataUIDAttribute,
        walkWith,
        shouldWalkElement,
        shouldWalkAttributes,
      )
      break
    case 'JSX_FRAGMENT':
      fastForEach(jsxElementChild.children, (child) => {
        walkElements(
          child,
          includeDataUIDAttribute,
          walkWith,
          shouldWalkElement,
          shouldWalkAttributes,
        )
      })
      break
    case 'JSX_CONDITIONAL_EXPRESSION':
      walkElements(
        jsxElementChild.condition,
        includeDataUIDAttribute,
        walkWith,
        shouldWalkElement,
        shouldWalkAttributes,
      )
      walkElements(
        jsxElementChild.whenTrue,
        includeDataUIDAttribute,
        walkWith,
        shouldWalkElement,
        shouldWalkAttributes,
      )
      walkElements(
        jsxElementChild.whenFalse,
        includeDataUIDAttribute,
        walkWith,
        shouldWalkElement,
        shouldWalkAttributes,
      )
      break
    case 'ATTRIBUTE_VALUE':
    case 'JS_IDENTIFIER':
      break
    case 'JS_PROPERTY_ACCESS':
      walkElements(
        jsxElementChild.onValue,
        includeDataUIDAttribute,
        walkWith,
        shouldWalkElement,
        shouldWalkAttributes,
      )
      break
    case 'JS_ELEMENT_ACCESS':
      walkElements(
        jsxElementChild.onValue,
        includeDataUIDAttribute,
        walkWith,
        shouldWalkElement,
        shouldWalkAttributes,
      )
      walkElements(
        jsxElementChild.element,
        includeDataUIDAttribute,
        walkWith,
        shouldWalkElement,
        shouldWalkAttributes,
      )
      break
    case 'ATTRIBUTE_NESTED_ARRAY':
      fastForEach(jsxElementChild.content, (contentElement) => {
        walkElements(
          contentElement.value,
          includeDataUIDAttribute,
          walkWith,
          shouldWalkElement,
          shouldWalkAttributes,
        )
      })
      break
    case 'ATTRIBUTE_NESTED_OBJECT':
      fastForEach(jsxElementChild.content, (contentElement) => {
        walkElements(
          contentElement.value,
          includeDataUIDAttribute,
          walkWith,
          shouldWalkElement,
          shouldWalkAttributes,
        )
      })
      break
    case 'ATTRIBUTE_FUNCTION_CALL':
      fastForEach(jsxElementChild.parameters, (param) => {
        walkElements(
          param,
          includeDataUIDAttribute,
          walkWith,
          shouldWalkElement,
          shouldWalkAttributes,
        )
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
  shouldWalkElement: (elem: JSXElementChild) => boolean,
): void {
  for (const attributeEntry of jsxAttributes) {
    switch (attributeEntry.type) {
      case 'JSX_ATTRIBUTES_ENTRY':
        const isDataUID = attributeEntry.key === UTOPIA_UID_KEY
        if (!isDataUID || includeDataUIDAttribute === 'include-data-uid-attribute') {
          walkElements(
            attributeEntry.value,
            includeDataUIDAttribute,
            walkWith,
            shouldWalkElement,
            'walk-attributes',
          )
        }
        break
      case 'JSX_ATTRIBUTES_SPREAD':
        walkElements(
          attributeEntry.spreadValue,
          includeDataUIDAttribute,
          walkWith,
          shouldWalkElement,
          'walk-attributes',
        )
        break
      default:
        assertNever(attributeEntry)
    }
  }
}

function getAllBaseVariables(jsxElementChild: JSXElementChild): Array<string> {
  let result: Array<string> = []
  walkElements(
    jsxElementChild,
    'do-not-include-data-uid-attribute',
    (element) => {
      if (isJSXElement(element)) {
        result = addUniquely(result, element.name.baseVariable)
      }
    },
    () => true,
    'walk-attributes',
  )
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

function walkWantedElementsOnly(element: JSXElementChild, uids: Array<string>): void {
  if (
    isJSXElement(element) ||
    isJSXFragment(element) ||
    isJSXConditionalExpression(element) ||
    isJSExpressionMapOrOtherJavaScript(element)
  ) {
    // Relies on this function blowing out for anything that doesn't have a valid one.
    const uid = getUtopiaIDFromJSXElement(element)
    checkUID(uid, element, uids)
  }
}

export function isWantedElement(element: JSXElementChild): boolean {
  return (
    isJSXElement(element) ||
    isJSXFragment(element) ||
    isJSXConditionalExpression(element) ||
    isJSExpressionMapOrOtherJavaScript(element)
  )
}

export function ensureArbitraryBlocksHaveUID(
  arbitraryBlock: ArbitraryJSBlock,
  uids: Array<string>,
  shouldWalkElement: (element: JSXElementChild) => boolean,
  shouldWalkAttributes: ShouldWalkAttributes,
): void {
  walkElementsWithin(
    arbitraryBlock.elementsWithin,
    'do-not-include-data-uid-attribute',
    (element) => {
      walkWantedElementsOnly(element, uids)
    },
    shouldWalkElement,
    shouldWalkAttributes,
  )
}

export function ensureElementsHaveUID(
  jsxElementChild: JSXElementChild,
  uids: Array<string>,
  shouldWalkElement: (element: JSXElementChild) => boolean = () => true,
  shouldWalkAttributes: ShouldWalkAttributes = 'walk-attributes',
): void {
  walkElements(
    jsxElementChild,
    'do-not-include-data-uid-attribute',
    (element) => {
      walkWantedElementsOnly(element, uids)
    },
    shouldWalkElement,
    shouldWalkAttributes,
  )
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

export interface ArbitraryProject {
  code: string
  parsed: ParsedTextFile
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

export function printedProjectContentArbitrary(stripUIDs: boolean): Arbitrary<ArbitraryProject> {
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
          return addImport('code.jsx', [], 'testlib', baseVariable, [], null, workingImports)
            .imports
        }
      }, JustImportViewAndReact)

      const code = printCode(
        '/index.js',
        printCodeOptions(false, true, false, stripUIDs, true),
        imports,
        topLevelElements,
        jsxFactoryFunction,
        detailOfExports,
      )

      const parsed = testParseCode(code)

      return { code: code, parsed: parsed }
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
        } else {
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

export function clearTopLevelElementSourceMapsUniqueIDsAndEmptyBlocks(
  element: UtopiaJSXComponent,
): UtopiaJSXComponent
export function clearTopLevelElementSourceMapsUniqueIDsAndEmptyBlocks(
  element: ArbitraryJSBlock,
): ArbitraryJSBlock
export function clearTopLevelElementSourceMapsUniqueIDsAndEmptyBlocks(
  element: TopLevelElement,
): TopLevelElement
export function clearTopLevelElementSourceMapsUniqueIDsAndEmptyBlocks(
  element: TopLevelElement,
): TopLevelElement {
  const withoutUID = clearTopLevelElementSourceMaps(clearTopLevelElementUniqueIDs(element))
  switch (withoutUID.type) {
    case 'UTOPIA_JSX_COMPONENT': {
      const blockCode = (withoutUID.arbitraryJSBlock?.javascript ?? '').trim()
      const blockIsEmpty = blockCode.length === 0
      return {
        ...withoutUID,
        arbitraryJSBlock:
          blockIsEmpty || withoutUID.arbitraryJSBlock == null
            ? null
            : clearArbitraryJSBlockSourceMaps({
                ...withoutUID.arbitraryJSBlock,
                javascript: blockCode,
              }),
      }
    }
    case 'ARBITRARY_JS_BLOCK': {
      const blockCode = (withoutUID.javascript ?? '').trim()
      return clearArbitraryJSBlockSourceMaps({
        ...withoutUID,
        javascript: blockCode,
        sourceMap: null,
      })
    }
    case 'IMPORT_STATEMENT':
    case 'UNPARSED_CODE':
      return withoutUID
    default:
      const _exhaustiveCheck: never = withoutUID
      throw new Error(`Unhandled element ${JSON.stringify(withoutUID)}`)
  }
}
