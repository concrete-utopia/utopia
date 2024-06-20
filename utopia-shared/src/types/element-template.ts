import type { PropertyPath } from './project-file-types'
import { RawSourceMap } from './RawSourceMap'

export interface ParsedComments {
  leadingComments: Array<Comment>
  trailingComments: Array<Comment>
  questionTokenComments?: ParsedComments
}

export const emptyComments: ParsedComments = {
  leadingComments: [],
  trailingComments: [],
}

interface BaseComment {
  comment: string
  rawText: string
  trailingNewLine: boolean
  pos: number | null
}

export interface MultiLineComment extends BaseComment {
  type: 'MULTI_LINE_COMMENT'
}

export interface SingleLineComment extends BaseComment {
  type: 'SINGLE_LINE_COMMENT'
}

export type Comment = MultiLineComment | SingleLineComment

export interface WithComments {
  comments: ParsedComments
}

export interface JSExpressionValue<T> extends WithComments {
  type: 'ATTRIBUTE_VALUE'
  uid: string
  value: T
}

/**
 If the contents of an `ATTRIBUTE_VALUE` is an object (whose values are not `JSXAttribute`s), we might still want to
 dig directly into them. This is the return type for when we dig into the `JSXAttribute`s using a path, which
 results in digging directly into an `ATTRIBUTE_VALUE` somewhere along the way.
 
 For example: `<View style={backgroundColor: 'red'} />`
 Here `style` will be an `ATTRIBUTE_VALUE`, whose value is the object `{backgroundColor: 'red'}`
 Then, if we attempt to read `style.backgroundColor`, because we are digging into the contents of the `style` `ATTRIBUTE_VALUE`
 we will get a `PART_OF_ATTRIBUTE_VALUE`, whose value is `'red'`. If alternatively `style` were an `ATTRIBUTE_NESTED_OBJECT`, then
 each of its contents would all be a `JSXAttribute`, in which case `'red'` might be an `ATTRIBUTE_VALUE`
 */
export interface PartOfJSXAttributeValue {
  type: 'PART_OF_ATTRIBUTE_VALUE'
  value: any
}

export interface JSXAttributeNotFound {
  type: 'ATTRIBUTE_NOT_FOUND'
}

export interface JSOpaqueArbitraryStatement {
  type: 'JS_OPAQUE_ARBITRARY_STATEMENT'
  originalJavascript: string
  definedWithin: Array<string>
  definedElsewhere: Array<string>
  uid: string
}

export interface JSAssignment<R extends JSExpression = JSExpression> {
  type: 'JS_ASSIGNMENT'
  leftHandSide: BoundParam
  rightHandSide: R
}

export interface JSAssignmentStatement {
  type: 'JS_ASSIGNMENT_STATEMENT'
  declarationKeyword: 'let' | 'const' | 'var'
  assignments: Array<JSAssignment>
  uid: string
}

export type JSArbitraryStatement = JSOpaqueArbitraryStatement | JSAssignmentStatement

export interface JSExpressionOtherJavaScript extends WithComments, WithElementsWithin {
  type: 'ATTRIBUTE_OTHER_JAVASCRIPT'
  params: Array<Param>
  originalJavascript: string
  javascriptWithUIDs: string
  transpiledJavascript: string
  definedElsewhere: Array<string>
  sourceMap: RawSourceMap | null
  uid: string
}

export interface JSXMapExpression extends WithComments {
  type: 'JSX_MAP_EXPRESSION'
  valueToMap: JSExpression
  mapFunction: JSExpression
  valuesInScopeFromParameters: Array<string>
  uid: string
}

export interface JSXSpreadAssignment extends WithComments {
  type: 'SPREAD_ASSIGNMENT'
  value: JSExpression
}

export interface JSXPropertyAssignment extends WithComments {
  type: 'PROPERTY_ASSIGNMENT'
  key: string | number
  value: JSExpression
  keyComments: ParsedComments
}

export type JSXProperty = JSXPropertyAssignment | JSXSpreadAssignment

export interface JSExpressionNestedObject extends WithComments {
  type: 'ATTRIBUTE_NESTED_OBJECT'
  content: Array<JSXProperty>
  uid: string
}

export interface JSXArrayValue extends WithComments {
  type: 'ARRAY_VALUE'
  value: JSExpression
}

export interface JSXArraySpread extends WithComments {
  type: 'ARRAY_SPREAD'
  value: JSExpression
}

export type JSXArrayElement = JSXArrayValue | JSXArraySpread

export interface JSExpressionNestedArray extends WithComments {
  type: 'ATTRIBUTE_NESTED_ARRAY'
  content: Array<JSXArrayElement>
  uid: string
}

export interface JSExpressionFunctionCall {
  type: 'ATTRIBUTE_FUNCTION_CALL'
  functionName: string
  parameters: Array<JSExpression>
  uid: string
}

export interface JSIdentifier extends WithComments {
  type: 'JS_IDENTIFIER'
  name: string
  uid: string
  sourceMap: RawSourceMap | null
}

export type OptionallyChained = 'not-optionally-chained' | 'optionally-chained'

export interface JSPropertyAccess extends WithComments {
  type: 'JS_PROPERTY_ACCESS'
  onValue: JSExpression
  property: string
  uid: string
  sourceMap: RawSourceMap | null
  originalJavascript: string
  optionallyChained: OptionallyChained
}

export interface JSElementAccess extends WithComments {
  type: 'JS_ELEMENT_ACCESS'
  onValue: JSExpression
  element: JSExpression
  uid: string
  sourceMap: RawSourceMap | null
  originalJavascript: string
  optionallyChained: OptionallyChained
}

export type IdentifierOrAccess = JSIdentifier | JSPropertyAccess | JSElementAccess

export type JSExpression =
  | JSIdentifier
  | JSPropertyAccess
  | JSElementAccess
  | JSExpressionValue<any>
  | JSExpressionOtherJavaScript
  | JSExpressionNestedArray
  | JSExpressionNestedObject
  | JSExpressionFunctionCall
  | JSXMapExpression
  | JSXElement

export type JSExpressionMapOrOtherJavascript = JSExpressionOtherJavaScript | JSXMapExpression

export type PrintBehavior = 'skip-printing' | 'include-in-printing'

export interface JSXAttributesEntry extends WithComments {
  type: 'JSX_ATTRIBUTES_ENTRY'
  key: string | number
  value: JSExpression
  printBehavior: PrintBehavior
}

export interface JSXAttributesSpread extends WithComments {
  type: 'JSX_ATTRIBUTES_SPREAD'
  spreadValue: JSExpression
}

export type JSXAttributesPart = JSXAttributesEntry | JSXAttributesSpread

export type JSXAttributes = Array<JSXAttributesPart>

const AllowedExternalReferences = ['React', 'utopiaCanvasJSXLookup']

export interface JSXElementName {
  baseVariable: string
  propertyPath: PropertyPath
}

export interface JSXElement {
  type: 'JSX_ELEMENT'
  name: JSXElementName
  props: JSXAttributes
  children: JSXElementChildren
  uid: string
}

export type JSXElementWithoutUID = Omit<JSXElement, 'uid'>

export type JSXConditionalExpressionWithoutUID = Omit<JSXConditionalExpression, 'uid'>

export type JSXFragmentWithoutUID = Omit<JSXFragment, 'uid'>

export type JSXMapExpressionWithoutUID = Omit<JSXMapExpression, 'uid'>

export type ElementsWithin = { [uid: string]: JSXElement }

export interface WithElementsWithin {
  elementsWithin: ElementsWithin
}

export interface JSXTextBlock {
  type: 'JSX_TEXT_BLOCK'
  text: string
  uid: string
}

export interface JSXFragment {
  type: 'JSX_FRAGMENT'
  uid: string
  children: JSXElementChildren
  longForm: boolean
}

export type JSXElementLike = JSXElement | JSXFragment

export interface JSXConditionalExpression extends WithComments {
  type: 'JSX_CONDITIONAL_EXPRESSION'
  uid: string
  condition: JSExpression
  originalConditionString: string
  whenTrue: JSXElementChild
  whenFalse: JSXElementChild
}

export type JSXElementChild =
  | JSXElement
  | JSExpression
  | JSXTextBlock
  | JSXFragment
  | JSXConditionalExpression

export type JSXElementChildWithoutUID =
  | JSXElementWithoutUID
  | JSXConditionalExpressionWithoutUID
  | JSXFragmentWithoutUID
  | JSXMapExpressionWithoutUID

interface ElementWithUid {
  uid: string
}

export type JSXElementChildren = Array<JSXElementChild>

// FIXME we need to inject data-uids using insertDataUIDsIntoCode

export interface RegularParam {
  type: 'REGULAR_PARAM'
  paramName: string
  defaultExpression: JSExpression | null
}

export interface DestructuredParamPart {
  propertyName: string | undefined
  param: Param
  defaultExpression: JSExpression | null
}

export interface DestructuredObject {
  type: 'DESTRUCTURED_OBJECT'
  parts: Array<DestructuredParamPart>
}

interface OmittedParam {
  type: 'OMITTED_PARAM'
}

export type DestructuredArrayPart = Param | OmittedParam

export interface DestructuredArray {
  type: 'DESTRUCTURED_ARRAY'
  parts: Array<DestructuredArrayPart>
}

export type BoundParam = RegularParam | DestructuredObject | DestructuredArray

export type Param = {
  type: 'PARAM'
  dotDotDotToken: boolean
  boundParam: BoundParam
}

export type VarLetOrConst = 'var' | 'let' | 'const'
export type FunctionDeclarationSyntax = 'function' | VarLetOrConst
export type BlockOrExpression = 'block' | 'parenthesized-expression' | 'expression'

export interface SimpleFunctionWrap {
  type: 'SIMPLE_FUNCTION_WRAP'
  functionExpression: JSExpression
}

export function simpleFunctionWrap(functionExpression: JSExpression): SimpleFunctionWrap {
  return {
    type: 'SIMPLE_FUNCTION_WRAP',
    functionExpression: functionExpression,
  }
}

export type FunctionWrap = SimpleFunctionWrap

export function isSimpleFunctionWrap(wrap: FunctionWrap): wrap is SimpleFunctionWrap {
  return wrap.type === 'SIMPLE_FUNCTION_WRAP'
}

export interface UtopiaJSXComponent {
  type: 'UTOPIA_JSX_COMPONENT'
  name: string | null
  /**
   * isFunction is true if we are talking about a Function Component
   * isFunction false means that this is an exported Element with no props
   * (NOT a component, NOT a class component!)
   */
  isFunction: boolean
  declarationSyntax: FunctionDeclarationSyntax
  blockOrExpression: BlockOrExpression
  functionWrapping: Array<FunctionWrap>
  params: Array<Param> | null
  propsUsed: Array<string>
  rootElement: JSXElementChild
  arbitraryJSBlock: ArbitraryJSBlock | null
  usedInReactDOMRender: boolean
  returnStatementComments: ParsedComments
}

// FIXME we need to inject data-uids using insertDataUIDsIntoCode
export type ArbitraryJSBlock = {
  type: 'ARBITRARY_JS_BLOCK'
  params: Array<Param>
  javascript: string
  transpiledJavascript: string
  definedWithin: Array<string>
  definedElsewhere: Array<string>
  sourceMap: RawSourceMap | null
  statements: Array<JSArbitraryStatement>
  uid: string
} & WithElementsWithin

export interface ImportStatement {
  type: 'IMPORT_STATEMENT'
  rawCode: string
  importStarAs: boolean // Includes `import * as Name from`
  importWithName: boolean // Includes `import Name from`
  imports: Array<string> // All other imports inside braces i.e. `import { Name } from`
  module: string
}

export interface UnparsedCode {
  type: 'UNPARSED_CODE'
  rawCode: string
}

export type TopLevelElement = UtopiaJSXComponent | ArbitraryJSBlock | ImportStatement | UnparsedCode

// FIXME: Should only really be in test code.

export type ComputedStyle = { [key: string]: string }
export type StyleAttributeMetadataEntry = { fromStyleSheet: boolean } // TODO rename me to StyleAttributeMetadata, the other one to StyleAttributeMetadataMap
export type StyleAttributeMetadata = { [key: string]: StyleAttributeMetadataEntry | undefined }

export interface SameFileOrigin {
  type: 'SAME_FILE_ORIGIN'
  filePath: string
  variableName: string
}

export interface ImportedOrigin {
  type: 'IMPORTED_ORIGIN'
  filePath: string
  variableName: string
  exportedName: string | null
}

export type ImportInfo = SameFileOrigin | ImportedOrigin

export type ActiveAndDefaultConditionValues = { active: boolean; default: boolean }
export type ConditionValue = ActiveAndDefaultConditionValues | 'not-a-conditional'

export type DetectedLayoutSystem = 'flex' | 'grid' | 'flow' | 'none'
export type TextDirection = 'ltr' | 'rtl'
export type HugProperty = 'hug' | 'squeeze' | 'collapsed'
export type HugPropertyWidthHeight = {
  width: HugProperty | null
  height: HugProperty | null
}

export const emptyComputedStyle: ComputedStyle = {}
export const emptyAttributeMetadata: StyleAttributeMetadata = {}

export type ElementsByUID = { [uid: string]: JSXElement }
