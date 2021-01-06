import * as Babel from '@babel/standalone'
import * as BabelTraverse from '@babel/traverse'
import * as BabelTypes from '@babel/types'
import * as ReactSyntaxPlugin from 'babel-plugin-syntax-jsx'
import * as FastCheck from 'fast-check'
import { Arbitrary } from 'fast-check'
import { MapLike } from 'typescript'
import * as PP from '../../shared/property-path'
import {
  ArbitraryJSBlock,
  arbitraryJSBlock,
  clearTopLevelElementUniqueIDs,
  isJSXArbitraryBlock,
  isUtopiaJSXComponent,
  JSXArbitraryBlock,
  jsxArbitraryBlock,
  JSXArrayElement,
  jsxArraySpread,
  JSXArraySpread,
  jsxArrayValue,
  JSXArrayValue,
  JSXAttribute,
  JSXAttributeFunctionCall,
  jsxAttributeFunctionCall,
  JSXAttributeNestedArray,
  jsxAttributeNestedArray,
  JSXAttributeNestedObject,
  jsxAttributeNestedObject,
  JSXAttributeOtherJavaScript,
  jsxAttributeOtherJavaScript,
  JSXAttributes,
  JSXAttributeValue,
  jsxAttributeValue,
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
  clearArbitraryJSBlockUniqueIDs,
  SingleLineComment,
  MultiLineComment,
  Comment,
  walkElement,
  getJSXElementNameAsString,
  isJSXElement,
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
  exportsDetail,
  ExportDetailNamed,
  exportDetailNamed,
  ExportDetailModifier,
  exportDetailModifier,
  ExportDetail,
  EmptyExportsDetail,
  StaticElementPath,
  isParseSuccess,
  isTextFile,
  ProjectFile,
} from '../../shared/project-file-types'
import { lintAndParse, printCode, printCodeOptions } from './parser-printer'
import { getUtopiaIDFromJSXElement } from '../../shared/uid-utils'
import { fastForEach } from '../../shared/utils'
import { addUniquely, flatMapArray } from '../../shared/array-utils'
import { optionalMap } from '../../shared/optional-utils'
import { getUtopiaID } from '../../model/element-template-utils'
import { emptyComments, parsedComments, ParsedComments } from './parser-printer-comments'

export const singleLineCommentArbitrary: Arbitrary<SingleLineComment> = lowercaseStringArbitrary().map(
  (text) => {
    return {
      type: 'SINGLE_LINE_COMMENT',
      comment: text,
      rawText: text,
      trailingNewLine: false,
      pos: null,
    }
  },
)

export const multiLineCommentArbitrary: Arbitrary<MultiLineComment> = lowercaseStringArbitrary().map(
  (text) => {
    return {
      type: 'MULTI_LINE_COMMENT',
      comment: text,
      rawText: text,
      trailingNewLine: false,
      pos: null,
    }
  },
)

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
  const result = lintAndParse(filename, contents)
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

export function parseThenPrint(originalCode: string): string {
  return parseModifyPrint(originalCode, (ps) => ps)
}

export function testParseThenPrint(originalCode: string, expectedFinalCode: string): void {
  return testParseModifyPrint(originalCode, expectedFinalCode, (ps) => ps)
}

export function testParseModifyPrint(
  originalCode: string,
  expectedFinalCode: string,
  transform: (parseSuccess: ParseSuccess) => ParseSuccess,
): void {
  const printedCode = parseModifyPrint(originalCode, transform)
  expect(printedCode).toEqual(expectedFinalCode)
}

function parseModifyPrint(
  originalCode: string,
  transform: (parseSuccess: ParseSuccess) => ParseSuccess,
): string {
  const initialParseResult = testParseCode(originalCode)
  return foldParsedTextFile(
    (failure) => fail(failure),
    (initialParseSuccess) => {
      const transformed = transform(initialParseSuccess)
      const printedCode = printCode(
        printCodeOptions(false, true, true),
        transformed.imports,
        transformed.topLevelElements,
        transformed.jsxFactoryFunction,
        transformed.exportsDetail,
      )
      return printedCode
    },
    (failure) => fail(failure),
    initialParseResult,
  )
}

export function clearParseResultUniqueIDs(parseResult: ParsedTextFile): ParsedTextFile {
  return mapParsedTextFile((success) => {
    const updatedTopLevelElements = success.topLevelElements.map(clearTopLevelElementUniqueIDs)
    return {
      ...success,
      topLevelElements: updatedTopLevelElements,
      combinedTopLevelArbitraryBlock: optionalMap(
        clearArbitraryJSBlockUniqueIDs,
        success.combinedTopLevelArbitraryBlock,
      ),
    }
  }, parseResult)
}

export const JustImportView: Imports = {
  'utopia-api': {
    importedAs: null,
    importedFromWithin: [importAlias('View')],
    importedWithName: null,
    comments: emptyComments,
  },
}

export const JustImportViewAndReact: Imports = {
  'utopia-api': {
    importedAs: null,
    importedFromWithin: [importAlias('View')],
    importedWithName: null,
    comments: emptyComments,
  },
  react: {
    importedAs: null,
    importedFromWithin: [],
    importedWithName: 'React',
    comments: emptyComments,
  },
}

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
    return PP.create(pathParts)
  })
}

export function jsxElementNameArbitrary(): Arbitrary<JSXElementName> {
  return FastCheck.tuple(
    lowercaseStringArbitrary().filter((str) => !JavaScriptReservedKeywords.includes(str)),
    FastCheck.array(lowercaseStringArbitrary()),
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

export function jsxAttributeValueArbitrary(): Arbitrary<JSXAttributeValue<any>> {
  return FastCheck.tuple(
    FastCheck.jsonObject(),
    arbitraryMultiLineComments(),
  ).map(([value, comments]) => jsxAttributeValue(value, comments))
}

export function jsxAttributeOtherJavaScriptArbitrary(): Arbitrary<JSXAttributeOtherJavaScript> {
  return FastCheck.constant(jsxAttributeOtherJavaScript('1 + 2', '1 + 2', [], null))
}

export function jsxArrayValueArbitrary(depth: number): Arbitrary<JSXArrayValue> {
  return FastCheck.tuple(
    jsxAttributeArbitrary(depth),
    arbitraryMultiLineComments(),
  ).map(([array, comments]) => jsxArrayValue(array, comments))
}

export function jsxArraySpreadArbitrary(depth: number): Arbitrary<JSXArraySpread> {
  return FastCheck.tuple(
    jsxAttributeArbitrary(depth),
    arbitraryMultiLineComments(),
  ).map(([array, comments]) => jsxArraySpread(array, comments))
}

export function jsxArrayElementArbitrary(depth: number): Arbitrary<JSXArrayElement> {
  return FastCheck.oneof<JSXArrayElement>(
    jsxArrayValueArbitrary(depth),
    jsxArraySpreadArbitrary(depth),
  )
}

export function jsxAttributeNestedArrayArbitrary(
  depth: number,
): Arbitrary<JSXAttributeNestedArray> {
  return FastCheck.tuple(
    FastCheck.array(jsxArrayElementArbitrary(depth - 1), 3),
    arbitraryMultiLineComments(),
  ).map(([array, comments]) => jsxAttributeNestedArray(array, comments))
}

export function jsxPropertyAssignmentArbitrary(depth: number): Arbitrary<JSXPropertyAssignment> {
  return FastCheck.tuple(lowercaseStringArbitrary(), jsxAttributeArbitrary(depth)).map(
    ([key, attribute]) => {
      return jsxPropertyAssignment(key, attribute, emptyComments)
    },
  )
}

export function jsxSpreadAssignmentArbitrary(depth: number): Arbitrary<JSXSpreadAssignment> {
  return FastCheck.tuple(
    jsxAttributeArbitrary(depth),
    arbitraryMultiLineComments(),
  ).map(([spread, comments]) => jsxSpreadAssignment(spread, comments))
}

export function jsxPropertyArbitrary(depth: number): Arbitrary<JSXProperty> {
  return FastCheck.oneof<JSXProperty>(
    jsxPropertyAssignmentArbitrary(depth),
    jsxSpreadAssignmentArbitrary(depth),
  )
}

export function jsxAttributeNestedObjectArbitrary(
  depth: number,
): Arbitrary<JSXAttributeNestedObject> {
  return FastCheck.tuple(
    FastCheck.array(jsxPropertyArbitrary(depth - 1), 3),
    arbitraryMultiLineComments(),
  ).map(([values, comments]) => jsxAttributeNestedObject(values, comments))
}

export function jsxAttributeFunctionCallArbitrary(
  depth: number,
): Arbitrary<JSXAttributeFunctionCall> {
  return FastCheck.tuple(
    lowercaseStringArbitrary(),
    FastCheck.array(jsxAttributeArbitrary(depth - 1), 3),
  ).map(([functionName, functionArguments]) => {
    return jsxAttributeFunctionCall(functionName, functionArguments)
  })
}

export function jsxAttributeArbitrary(depth: number): Arbitrary<JSXAttribute> {
  if (depth <= 1) {
    return FastCheck.oneof<JSXAttribute>(
      jsxAttributeValueArbitrary(),
      jsxAttributeOtherJavaScriptArbitrary(),
    )
  } else {
    return FastCheck.oneof<JSXAttribute>(
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
  return flatObjectArbitrary(lowercaseStringArbitrary(), jsxAttributeArbitrary(3))
}

export function jsxElementArbitrary(depth: number): Arbitrary<JSXElement> {
  let childArbitrary: Arbitrary<JSXElementChild>
  if (depth <= 1) {
    childArbitrary = FastCheck.oneof<JSXElementChild>(
      jsxArbitraryBlockArbitrary(),
      jsxTextBlockArbitrary(),
    )
  } else {
    childArbitrary = FastCheck.oneof<JSXElementChild>(
      jsxElementArbitrary(depth - 1),
      jsxArbitraryBlockArbitrary(),
      jsxTextBlockArbitrary(),
    )
  }
  return FastCheck.tuple(
    jsxElementNameArbitrary(),
    jsxAttributesArbitrary(),
    FastCheck.array(childArbitrary, 3),
  ).map(([elementName, elementAttributes, elementChildren]) => {
    return jsxElement(elementName, elementAttributes, elementChildren)
  })
}

export function jsxElementChildArbitrary(): Arbitrary<JSXElementChild> {
  return FastCheck.oneof<JSXElementChild>(
    jsxElementArbitrary(3),
    jsxArbitraryBlockArbitrary(),
    jsxTextBlockArbitrary(),
  )
}

export function arbitraryJSBlockArbitrary(): Arbitrary<ArbitraryJSBlock> {
  return FastCheck.constant(arbitraryJSBlock('1 + 2', '1 + 2', [], [], null, emptyComments))
}

export function arbitraryComments(): Arbitrary<ParsedComments> {
  return FastCheck.tuple(
    FastCheck.array(commentArbitrary),
    FastCheck.array(commentArbitrary),
  ).map(([leadingComments, trailingComments]) => parsedComments(leadingComments, trailingComments))
}

export function arbitraryMultiLineComments(): Arbitrary<ParsedComments> {
  return FastCheck.tuple(
    FastCheck.array(multiLineCommentArbitrary),
    FastCheck.array(multiLineCommentArbitrary),
  ).map(([leadingComments, trailingComments]) => parsedComments(leadingComments, trailingComments))
}

export function utopiaJSXComponentArbitrary(): Arbitrary<UtopiaJSXComponent> {
  return FastCheck.tuple(
    lowercaseStringArbitrary().filter((str) => !JavaScriptReservedKeywords.includes(str)),
    FastCheck.boolean(),
    jsxElementArbitrary(3),
    arbitraryJSBlockArbitrary(),
    arbitraryComments(),
    arbitraryComments(),
  )
    .map(([name, isFunction, rootElement, jsBlock, comments, returnStatementComments]) => {
      return utopiaJSXComponent(
        name,
        isFunction,
        defaultPropsParam,
        [],
        rootElement,
        jsBlock,
        false,
        comments,
        returnStatementComments,
      )
    })
    .filter((component) => {
      // Prevent creating a component that depends on itself.
      let elementNames: Array<string> = []
      walkElements(component.rootElement, (elem) => {
        elementNames.push(elem.name.baseVariable)
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

export function exportDetailNamedArbitrary(
  possibleNames: Array<string>,
): Arbitrary<ExportDetailNamed> {
  return FastCheck.constantFrom(...possibleNames).map(exportDetailNamed)
}

export function exportDetailModifierArbitrary(): Arbitrary<ExportDetailModifier> {
  return lowercaseStringArbitrary().map(exportDetailModifier)
}

export function exportDetailArbitrary(possibleNames: Array<string>): Arbitrary<ExportDetail> {
  return FastCheck.oneof<ExportDetail>(
    exportDetailNamedArbitrary(possibleNames),
    exportDetailModifierArbitrary(),
  )
}

export function exportsDetailArbitrary(possibleNames: Array<string>): Arbitrary<ExportsDetail> {
  if (possibleNames.length === 0) {
    return FastCheck.constant(EmptyExportsDetail)
  } else {
    return FastCheck.tuple(
      FastCheck.constant(null), //FastCheck.option(exportDetailNamedArbitrary(possibleNames)),
      flatObjectArbitrary(lowercaseStringArbitrary(), exportDetailArbitrary(possibleNames)),
    ).map(([defaultExport, namedExports]) => {
      return exportsDetail(defaultExport, namedExports)
    })
  }
}

function walkElements(
  jsxElementChild: JSXElementChild,
  walkWith: (elem: JSXElement) => void,
): void {
  switch (jsxElementChild.type) {
    case 'JSX_ELEMENT':
      walkWith(jsxElementChild)
      fastForEach(jsxElementChild.children, (child) => {
        walkElements(child, walkWith)
      })
      break
    case 'JSX_TEXT_BLOCK':
      break
    case 'JSX_ARBITRARY_BLOCK':
      fastForEach(Object.keys(jsxElementChild.elementsWithin), (elementWithinKey) => {
        const innerElement = jsxElementChild.elementsWithin[elementWithinKey]
        walkElements(innerElement, walkWith)
      })
      break
    case 'JSX_FRAGMENT':
      fastForEach(jsxElementChild.children, (child) => {
        walkElements(child, walkWith)
      })
      break
    default:
      const _exhaustiveCheck: never = jsxElementChild
      throw new Error(`Unhandled type ${JSON.stringify(jsxElementChild)}`)
  }
}

function walkAllJSXElementChilds(
  jsxElementChild: JSXElementChild,
  walkWith: (elem: JSXElementChild) => void,
): void {
  walkWith(jsxElementChild)
  switch (jsxElementChild.type) {
    case 'JSX_ELEMENT':
      fastForEach(jsxElementChild.children, (child) => {
        walkAllJSXElementChilds(child, walkWith)
      })
      break
    case 'JSX_TEXT_BLOCK':
      break
    case 'JSX_ARBITRARY_BLOCK':
      fastForEach(Object.keys(jsxElementChild.elementsWithin), (elementWithinKey) => {
        const innerElement = jsxElementChild.elementsWithin[elementWithinKey]
        walkAllJSXElementChilds(innerElement, walkWith)
      })
      break
    case 'JSX_FRAGMENT':
      fastForEach(jsxElementChild.children, (child) => {
        walkAllJSXElementChilds(child, walkWith)
      })
      break
    default:
      const _exhaustiveCheck: never = jsxElementChild
      throw new Error(`Unhandled type ${JSON.stringify(jsxElementChild)}`)
  }
}

function getAllBaseVariables(jsxElementChild: JSXElementChild): Array<string> {
  let result: Array<string> = []
  walkElements(jsxElementChild, (element) => {
    result = addUniquely(result, element.name.baseVariable)
  })
  return result
}

export function ensureElementsHaveUID(jsxElementChild: JSXElementChild, uids: Array<string>): void {
  walkElements(jsxElementChild, (element) => {
    // Relies on this function blowing out for anything that doesn't have a valid one.
    const uid = getUtopiaIDFromJSXElement(element)
    if (uids.includes(uid)) {
      throw new Error(`UID ${uid} is duplicated in ${JSON.stringify(element)}`)
    } else {
      uids.push(uid)
    }
  })
}

function babelCheckForDataUID(): { visitor: BabelTraverse.Visitor } {
  return {
    visitor: {
      JSXElement(path) {
        const node = path.node
        let hasValidUID: boolean = false
        for (const attribute of node.openingElement.attributes) {
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
  walkAllJSXElementChilds(jsxElementChild, (element) => {
    if (isJSXArbitraryBlock(element)) {
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

function getTopLevelElementVariableName(topLevelElement: TopLevelElement): string | null {
  switch (topLevelElement.type) {
    case 'UTOPIA_JSX_COMPONENT':
      return topLevelElement.name
    case 'ARBITRARY_JS_BLOCK':
      return null
    default:
      const _exhaustiveCheck: never = topLevelElement
      throw new Error(`Unhandled type ${JSON.stringify(topLevelElement)}`)
  }
}

function areTopLevelElementsValid(topLevelElements: Array<TopLevelElement>): boolean {
  let componentNames: Array<string> = []
  for (const topLevelElement of topLevelElements) {
    const name = getTopLevelElementVariableName(topLevelElement)
    if (name != null) {
      if (componentNames.includes(name)) {
        return false
      } else {
        componentNames.push(name)
      }
    }
  }
  return true
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
          return [topLevelElement.name]
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
            return []
          default:
            const _exhaustiveCheck: never = topLevelElement
            throw new Error(`Unhandled type ${JSON.stringify(topLevelElement)}`)
        }
      }, topLevelElements)
      const imports: Imports = allBaseVariables.reduce((workingImports, baseVariable) => {
        return addImport('testlib', baseVariable, [], null, emptyComments, workingImports)
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
      const emptyPath = ([] as any) as StaticElementPath
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
      fail(`Not a parse success ${file.fileContents.parsed}`)
    }
  } else {
    fail(`Not a text file ${file}`)
  }
}
