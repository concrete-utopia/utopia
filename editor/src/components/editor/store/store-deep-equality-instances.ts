import { Sides } from 'utopia-api/core'
import {
  ElementInstanceMetadata,
  elementInstanceMetadata,
  ElementInstanceMetadataMap,
  ElementsWithin,
  isArraySpread,
  isArrayValue,
  isJSXArbitraryBlock,
  isJSXAttributeFunctionCall,
  isJSXAttributeNestedArray,
  isJSXAttributeNestedObject,
  isJSXAttributeOtherJavaScript,
  isJSXAttributeValue,
  isJSXElement,
  isJSXFragment,
  isJSXTextBlock,
  isMultiLineComment,
  isPropertyAssignment,
  isSingleLineComment,
  isSpreadAssignment,
  JSXArbitraryBlock,
  JSXArrayElement,
  jsxArraySpread,
  JSXArraySpread,
  JSXArrayValue,
  jsxArrayValue,
  JSXAttribute,
  jsxAttributeFunctionCall,
  JSXAttributeFunctionCall,
  jsxAttributeNestedArray,
  JSXAttributeNestedArray,
  jsxAttributeNestedObject,
  JSXAttributeNestedObject,
  JSXAttributeOtherJavaScript,
  JSXAttributes,
  jsxAttributeValue,
  JSXAttributeValue,
  jsxElement,
  JSXElement,
  JSXElementChild,
  JSXFragment,
  JSXProperty,
  jsxPropertyAssignment,
  JSXPropertyAssignment,
  jsxSpreadAssignment,
  JSXSpreadAssignment,
  JSXTextBlock,
  multiLineComment,
  MultiLineComment,
  SingleLineComment,
  singleLineComment,
  Comment,
  specialSizeMeasurements,
  SpecialSizeMeasurements,
  JSXAttributesEntry,
  jsxAttributesEntry,
  ImportInfo,
  createImportedFrom,
  FoundImportInfo,
  JSXAttributesSpread,
  jsxAttributesSpread,
  JSXAttributesPart,
  isJSXAttributesEntry,
  isJSXAttributesSpread,
  ParsedComments,
  parsedComments,
  UtopiaJSXComponent,
  utopiaJSXComponent,
  ArbitraryJSBlock,
  BoundParam,
  isRegularParam,
  isDestructuredObject,
  isDestructuredArray,
  regularParam,
  isOmittedParam,
  isParam,
  RegularParam,
  DestructuredObject,
  destructuredObject,
  DestructuredParamPart,
  destructuredParamPart,
  Param,
  destructuredArray,
  DestructuredArray,
  DestructuredArrayPart,
  functionParam,
} from '../../../core/shared/element-template'
import { CanvasRectangle, LocalPoint, LocalRectangle } from '../../../core/shared/math-utils'
import { RawSourceMap } from '../../../core/workers/ts/ts-typings/RawSourceMap'
import {
  KeepDeepEqualityResult,
  keepDeepEqualityResult,
  KeepDeepEqualityCall,
  combine3EqualityCalls,
  combine6EqualityCalls,
  nullableDeepEquality,
  createCallWithTripleEquals,
  objectDeepEquality,
  combine5EqualityCalls,
  arrayDeepEquality,
  combine2EqualityCalls,
  combine7EqualityCalls,
  combine8EqualityCalls,
  undefinableDeepEquality,
  combine4EqualityCalls,
  combine12EqualityCalls,
  combine11EqualityCalls,
  combine1EqualityCall,
  combine14EqualityCalls,
  createCallWithShallowEquals,
  combine10EqualityCalls,
} from '../../../utils/deep-equality'
import {
  ElementPathArrayKeepDeepEquality,
  HigherOrderControlArrayKeepDeepEquality,
  ElementPathKeepDeepEquality,
  EitherKeepDeepEquality,
  JSXElementNameKeepDeepEqualityCall,
} from '../../../utils/deep-equality-instances'
import { createCallFromIntrospectiveKeepDeep } from '../../../utils/react-performance'
import {
  TransientCanvasState,
  TransientFilesState,
  transientCanvasState,
  DerivedState,
  EditorStatePatch,
} from './editor-state'

export function TransientCanvasStateKeepDeepEquality(): KeepDeepEqualityCall<TransientCanvasState> {
  return combine4EqualityCalls(
    (state) => state.selectedViews,
    ElementPathArrayKeepDeepEquality,
    (state) => state.highlightedViews,
    ElementPathArrayKeepDeepEquality,
    (state) => state.filesState,
    createCallFromIntrospectiveKeepDeep<TransientFilesState | null>(),
    (state) => state.toastsToApply,
    createCallWithShallowEquals(),
    transientCanvasState,
  )
}

export function DerivedStateKeepDeepEquality(): KeepDeepEqualityCall<DerivedState> {
  return combine6EqualityCalls(
    (state) => state.navigatorTargets,
    ElementPathArrayKeepDeepEquality,
    (state) => state.visibleNavigatorTargets,
    ElementPathArrayKeepDeepEquality,
    (state) => state.canvas.descendantsOfHiddenInstances,
    ElementPathArrayKeepDeepEquality,
    (state) => state.canvas.controls,
    HigherOrderControlArrayKeepDeepEquality,
    (state) => state.canvas.transientState,
    TransientCanvasStateKeepDeepEquality(),
    (state) => state.elementWarnings,
    createCallFromIntrospectiveKeepDeep(),
    (
      navigatorTargets,
      visibleNavigatorTargets,
      descendantsOfHiddenInstances,
      controls,
      transientState,
      elementWarnings,
    ) => {
      return {
        navigatorTargets: navigatorTargets,
        visibleNavigatorTargets: visibleNavigatorTargets,
        canvas: {
          descendantsOfHiddenInstances: descendantsOfHiddenInstances,
          controls: controls,
          transientState: transientState,
        },
        elementWarnings: elementWarnings,
      }
    },
  )
}

export function MultiLineCommentKeepDeepEqualityCall(): KeepDeepEqualityCall<MultiLineComment> {
  return combine4EqualityCalls(
    (comment) => comment.comment,
    createCallWithTripleEquals(),
    (comment) => comment.rawText,
    createCallWithTripleEquals(),
    (comment) => comment.trailingNewLine,
    createCallWithTripleEquals(),
    (comment) => comment.pos,
    createCallWithTripleEquals(),
    multiLineComment,
  )
}

export function SingleLineCommentKeepDeepEqualityCall(): KeepDeepEqualityCall<SingleLineComment> {
  return combine4EqualityCalls(
    (comment) => comment.comment,
    createCallWithTripleEquals(),
    (comment) => comment.rawText,
    createCallWithTripleEquals(),
    (comment) => comment.trailingNewLine,
    createCallWithTripleEquals(),
    (comment) => comment.pos,
    createCallWithTripleEquals(),
    singleLineComment,
  )
}

export function CommentKeepDeepEqualityCall(): KeepDeepEqualityCall<Comment> {
  return (oldComment, newComment) => {
    if (isMultiLineComment(oldComment) && isMultiLineComment(newComment)) {
      return MultiLineCommentKeepDeepEqualityCall()(oldComment, newComment)
    } else if (isSingleLineComment(oldComment) && isSingleLineComment(newComment)) {
      return SingleLineCommentKeepDeepEqualityCall()(oldComment, newComment)
    } else {
      return keepDeepEqualityResult(newComment, false)
    }
  }
}

export function ParsedCommentsKeepDeepEqualityCall(): KeepDeepEqualityCall<ParsedComments> {
  return combine2EqualityCalls(
    (comments) => comments.leadingComments,
    arrayDeepEquality(CommentKeepDeepEqualityCall()),
    (comments) => comments.trailingComments,
    arrayDeepEquality(CommentKeepDeepEqualityCall()),
    parsedComments,
  )
}

export function JSXAttributeValueKeepDeepEqualityCall<T>(): KeepDeepEqualityCall<
  JSXAttributeValue<T>
> {
  return combine2EqualityCalls(
    (attribute) => attribute.value,
    createCallFromIntrospectiveKeepDeep<T>(),
    (attribute) => attribute.comments,
    ParsedCommentsKeepDeepEqualityCall(),
    jsxAttributeValue,
  )
}

export function JSXAttributeOtherJavaScriptKeepDeepEqualityCall(): KeepDeepEqualityCall<
  JSXAttributeOtherJavaScript
> {
  return combine6EqualityCalls(
    (attribute) => attribute.javascript,
    createCallWithTripleEquals(),
    (attribute) => attribute.transpiledJavascript,
    createCallWithTripleEquals(),
    (attribute) => attribute.definedElsewhere,
    arrayDeepEquality(createCallWithTripleEquals()),
    (attribute) => attribute.sourceMap,
    createCallFromIntrospectiveKeepDeep(),
    (attribute) => attribute.uniqueID,
    createCallWithTripleEquals(),
    (block) => block.elementsWithin,
    ElementsWithinKeepDeepEqualityCall(),
    (javascript, transpiledJavascript, definedElsewhere, sourceMap, uniqueID, elementsWithin) => {
      return {
        type: 'ATTRIBUTE_OTHER_JAVASCRIPT',
        javascript: javascript,
        transpiledJavascript: transpiledJavascript,
        definedElsewhere: definedElsewhere,
        sourceMap: sourceMap,
        uniqueID: uniqueID,
        elementsWithin: elementsWithin,
      }
    },
  )
}

export function JSXArrayValueKeepDeepEqualityCall(): KeepDeepEqualityCall<JSXArrayValue> {
  return combine2EqualityCalls(
    (value) => value.value,
    JSXAttributeKeepDeepEqualityCall(),
    (value) => value.comments,
    ParsedCommentsKeepDeepEqualityCall(),
    jsxArrayValue,
  )
}

export function JSXArraySpreadKeepDeepEqualityCall(): KeepDeepEqualityCall<JSXArraySpread> {
  return combine2EqualityCalls(
    (value) => value.value,
    JSXAttributeKeepDeepEqualityCall(),
    (value) => value.comments,
    ParsedCommentsKeepDeepEqualityCall(),
    jsxArraySpread,
  )
}

export function JSXArrayElementKeepDeepEqualityCall(): KeepDeepEqualityCall<JSXArrayElement> {
  return (oldElement, newElement) => {
    if (isArrayValue(oldElement) && isArrayValue(newElement)) {
      return JSXArrayValueKeepDeepEqualityCall()(oldElement, newElement)
    } else if (isArraySpread(oldElement) && isArraySpread(newElement)) {
      return JSXArraySpreadKeepDeepEqualityCall()(oldElement, newElement)
    } else {
      return keepDeepEqualityResult(newElement, false)
    }
  }
}

export function JSXAttributeNestedArrayKeepDeepEqualityCall(): KeepDeepEqualityCall<
  JSXAttributeNestedArray
> {
  return combine2EqualityCalls(
    (attribute) => attribute.content,
    arrayDeepEquality(JSXArrayElementKeepDeepEqualityCall()),
    (value) => value.comments,
    ParsedCommentsKeepDeepEqualityCall(),
    jsxAttributeNestedArray,
  )
}

export function JSXSpreadAssignmentKeepDeepEqualityCall(): KeepDeepEqualityCall<
  JSXSpreadAssignment
> {
  return combine2EqualityCalls(
    (value) => value.value,
    JSXAttributeKeepDeepEqualityCall(),
    (value) => value.comments,
    ParsedCommentsKeepDeepEqualityCall(),
    jsxSpreadAssignment,
  )
}

export function JSXPropertyAssignmentKeepDeepEqualityCall(): KeepDeepEqualityCall<
  JSXPropertyAssignment
> {
  return combine4EqualityCalls(
    (value) => value.key,
    createCallWithTripleEquals(),
    (value) => value.value,
    JSXAttributeKeepDeepEqualityCall(),
    (value) => value.comments,
    ParsedCommentsKeepDeepEqualityCall(),
    (value) => value.keyComments,
    ParsedCommentsKeepDeepEqualityCall(),
    jsxPropertyAssignment,
  )
}

export function JSXPropertyKeepDeepEqualityCall(): KeepDeepEqualityCall<JSXProperty> {
  return (oldProperty, newProperty) => {
    if (isSpreadAssignment(oldProperty) && isSpreadAssignment(newProperty)) {
      return JSXSpreadAssignmentKeepDeepEqualityCall()(oldProperty, newProperty)
    } else if (isPropertyAssignment(oldProperty) && isPropertyAssignment(newProperty)) {
      return JSXPropertyAssignmentKeepDeepEqualityCall()(oldProperty, newProperty)
    } else {
      return keepDeepEqualityResult(newProperty, false)
    }
  }
}

export function JSXAttributeNestedObjectKeepDeepEqualityCall(): KeepDeepEqualityCall<
  JSXAttributeNestedObject
> {
  return combine2EqualityCalls(
    (attribute) => attribute.content,
    arrayDeepEquality(JSXPropertyKeepDeepEqualityCall()),
    (value) => value.comments,
    ParsedCommentsKeepDeepEqualityCall(),
    jsxAttributeNestedObject,
  )
}

export function JSXAttributeFunctionCallKeepDeepEqualityCall(): KeepDeepEqualityCall<
  JSXAttributeFunctionCall
> {
  return combine2EqualityCalls(
    (value) => value.functionName,
    createCallWithTripleEquals(),
    (value) => value.parameters,
    arrayDeepEquality(JSXAttributeKeepDeepEqualityCall()),
    jsxAttributeFunctionCall,
  )
}

export function JSXAttributeKeepDeepEqualityCall(): KeepDeepEqualityCall<JSXAttribute> {
  return (oldAttribute, newAttribute) => {
    if (isJSXAttributeValue(oldAttribute) && isJSXAttributeValue(newAttribute)) {
      return JSXAttributeValueKeepDeepEqualityCall()(oldAttribute, newAttribute)
    } else if (
      isJSXAttributeOtherJavaScript(oldAttribute) &&
      isJSXAttributeOtherJavaScript(newAttribute)
    ) {
      return JSXAttributeOtherJavaScriptKeepDeepEqualityCall()(oldAttribute, newAttribute)
    } else if (isJSXAttributeNestedArray(oldAttribute) && isJSXAttributeNestedArray(newAttribute)) {
      return JSXAttributeNestedArrayKeepDeepEqualityCall()(oldAttribute, newAttribute)
    } else if (
      isJSXAttributeNestedObject(oldAttribute) &&
      isJSXAttributeNestedObject(newAttribute)
    ) {
      return JSXAttributeNestedObjectKeepDeepEqualityCall()(oldAttribute, newAttribute)
    } else if (
      isJSXAttributeFunctionCall(oldAttribute) &&
      isJSXAttributeFunctionCall(newAttribute)
    ) {
      return JSXAttributeFunctionCallKeepDeepEqualityCall()(oldAttribute, newAttribute)
    } else {
      return keepDeepEqualityResult(newAttribute, false)
    }
  }
}

export function JSXAttributesEntryDeepEqualityCall(): KeepDeepEqualityCall<JSXAttributesEntry> {
  return combine3EqualityCalls(
    (entry) => entry.key,
    createCallWithTripleEquals(),
    (entry) => entry.value,
    JSXAttributeKeepDeepEqualityCall(),
    (entry) => entry.comments,
    ParsedCommentsKeepDeepEqualityCall(),
    jsxAttributesEntry,
  )
}

export function JSXAttributesSpreadDeepEqualityCall(): KeepDeepEqualityCall<JSXAttributesSpread> {
  return combine2EqualityCalls(
    (entry) => entry.spreadValue,
    JSXAttributeKeepDeepEqualityCall(),
    (entry) => entry.comments,
    ParsedCommentsKeepDeepEqualityCall(),
    jsxAttributesSpread,
  )
}

export function JSXAttributesPartDeepEqualityCall(): KeepDeepEqualityCall<JSXAttributesPart> {
  return (oldPart, newPart) => {
    if (isJSXAttributesEntry(oldPart) && isJSXAttributesEntry(newPart)) {
      return JSXAttributesEntryDeepEqualityCall()(oldPart, newPart)
    } else if (isJSXAttributesSpread(oldPart) && isJSXAttributesSpread(newPart)) {
      return JSXAttributesSpreadDeepEqualityCall()(oldPart, newPart)
    } else {
      return keepDeepEqualityResult(newPart, false)
    }
  }
}

export function JSXAttributesKeepDeepEqualityCall(): KeepDeepEqualityCall<JSXAttributes> {
  return arrayDeepEquality(JSXAttributesPartDeepEqualityCall())
}

export function JSXElementKeepDeepEquality(): KeepDeepEqualityCall<JSXElement> {
  return combine4EqualityCalls(
    (element) => element.name,
    JSXElementNameKeepDeepEqualityCall(),
    (element) => element.uid,
    createCallWithTripleEquals(),
    (element) => element.props,
    JSXAttributesKeepDeepEqualityCall(),
    (element) => element.children,
    JSXElementChildArrayKeepDeepEquality(),
    jsxElement,
  )
}

export function ElementsWithinKeepDeepEqualityCall(): KeepDeepEqualityCall<ElementsWithin> {
  return objectDeepEquality(JSXElementKeepDeepEquality())
}

export function JSXArbitraryBlockKeepDeepEquality(): KeepDeepEqualityCall<JSXArbitraryBlock> {
  return combine7EqualityCalls(
    (block) => block.originalJavascript,
    createCallWithTripleEquals(),
    (block) => block.javascript,
    createCallWithTripleEquals(),
    (block) => block.transpiledJavascript,
    createCallWithTripleEquals(),
    (block) => block.definedElsewhere,
    arrayDeepEquality(createCallWithTripleEquals()),
    (block) => block.sourceMap,
    createCallFromIntrospectiveKeepDeep(),
    (block) => block.uniqueID,
    createCallWithTripleEquals(),
    (block) => block.elementsWithin,
    ElementsWithinKeepDeepEqualityCall(),
    (
      originalJavascript,
      javascript,
      transpiledJavascript,
      definedElsewhere,
      sourceMap,
      uniqueID,
      elementsWithin,
    ) => {
      return {
        type: 'JSX_ARBITRARY_BLOCK',
        originalJavascript: originalJavascript,
        javascript: javascript,
        transpiledJavascript: transpiledJavascript,
        definedElsewhere: definedElsewhere,
        sourceMap: sourceMap,
        uniqueID: uniqueID,
        elementsWithin: elementsWithin,
      }
    },
  )
}

export function ArbitraryJsBlockKeepDeepEquality(): KeepDeepEqualityCall<ArbitraryJSBlock> {
  return combine7EqualityCalls(
    (block) => block.javascript,
    createCallWithTripleEquals(),
    (block) => block.transpiledJavascript,
    createCallWithTripleEquals(),
    (block) => block.definedWithin,
    arrayDeepEquality(createCallWithTripleEquals()),
    (block) => block.definedElsewhere,
    arrayDeepEquality(createCallWithTripleEquals()),
    (block) => block.sourceMap,
    createCallFromIntrospectiveKeepDeep(),
    (block) => block.uniqueID,
    createCallWithTripleEquals(),
    (block) => block.elementsWithin,
    ElementsWithinKeepDeepEqualityCall(),
    (
      javascript: string,
      transpiledJavascript: string,
      definedWithin: Array<string>,
      definedElsewhere: Array<string>,
      sourceMap: RawSourceMap | null,
      uniqueID: string,
      elementsWithin: ElementsWithin,
    ) => {
      return {
        type: 'ARBITRARY_JS_BLOCK',
        javascript: javascript,
        transpiledJavascript: transpiledJavascript,
        definedWithin: definedWithin,
        definedElsewhere: definedElsewhere,
        sourceMap: sourceMap,
        uniqueID: uniqueID,
        elementsWithin: elementsWithin,
      }
    },
  )
}

export function JSXTextBlockKeepDeepEquality(): KeepDeepEqualityCall<JSXTextBlock> {
  return combine2EqualityCalls(
    (block) => block.text,
    createCallWithTripleEquals(),
    (block) => block.uniqueID,
    createCallWithTripleEquals(),
    (text, uniqueID) => {
      return {
        type: 'JSX_TEXT_BLOCK',
        text: text,
        uniqueID: uniqueID,
      }
    },
  )
}

export function JSXFragmentKeepDeepEquality(): KeepDeepEqualityCall<JSXFragment> {
  return combine3EqualityCalls(
    (fragment) => fragment.children,
    JSXElementChildArrayKeepDeepEquality(),
    (fragment) => fragment.uniqueID,
    createCallWithTripleEquals(),
    (fragment) => fragment.longForm,
    createCallWithTripleEquals(),
    (children, uniqueID, longForm) => {
      return {
        type: 'JSX_FRAGMENT',
        children: children,
        uniqueID: uniqueID,
        longForm: longForm,
      }
    },
  )
}

export function JSXElementChildKeepDeepEquality(): KeepDeepEqualityCall<JSXElementChild> {
  return (oldElement, newElement) => {
    if (isJSXElement(oldElement) && isJSXElement(newElement)) {
      return JSXElementKeepDeepEquality()(oldElement, newElement)
    } else if (isJSXArbitraryBlock(oldElement) && isJSXArbitraryBlock(newElement)) {
      return JSXArbitraryBlockKeepDeepEquality()(oldElement, newElement)
    } else if (isJSXTextBlock(oldElement) && isJSXTextBlock(newElement)) {
      return JSXTextBlockKeepDeepEquality()(oldElement, newElement)
    } else if (isJSXFragment(oldElement) && isJSXFragment(newElement)) {
      return JSXFragmentKeepDeepEquality()(oldElement, newElement)
    } else {
      return keepDeepEqualityResult(newElement, false)
    }
  }
}

export function JSXElementChildArrayKeepDeepEquality(): KeepDeepEqualityCall<
  Array<JSXElementChild>
> {
  return arrayDeepEquality(JSXElementChildKeepDeepEquality())
}

export const RegularParamKeepDeepEquality: KeepDeepEqualityCall<RegularParam> = combine2EqualityCalls(
  (param) => param.paramName,
  createCallWithTripleEquals(),
  (param) => param.defaultExpression,
  nullableDeepEquality(JSXAttributeOtherJavaScriptKeepDeepEqualityCall()),
  regularParam,
)

export const DestructuredParamPartKeepDeepEquality: KeepDeepEqualityCall<DestructuredParamPart> = combine3EqualityCalls(
  (paramPart) => paramPart.propertyName,
  createCallWithTripleEquals(),
  (paramPart) => paramPart.param,
  ParamKeepDeepEquality(),
  (paramPart) => paramPart.defaultExpression,
  nullableDeepEquality(JSXAttributeOtherJavaScriptKeepDeepEqualityCall()),
  destructuredParamPart,
)

export const DestructuredObjectParamKeepDeepEquality: KeepDeepEqualityCall<DestructuredObject> = combine1EqualityCall(
  (paramPart) => paramPart.parts,
  arrayDeepEquality(DestructuredParamPartKeepDeepEquality),
  destructuredObject,
)

export const DestructuredArrayPartKeepDeepEquality: KeepDeepEqualityCall<DestructuredArrayPart> = (
  oldValue,
  newValue,
) => {
  if (isOmittedParam(oldValue) && isOmittedParam(newValue)) {
    return keepDeepEqualityResult(oldValue, true)
  } else if (isParam(oldValue) && isParam(newValue)) {
    return ParamKeepDeepEquality()(oldValue, newValue)
  } else {
    return keepDeepEqualityResult(newValue, false)
  }
}

export const DestructuredArrayKeepDeepEquality: KeepDeepEqualityCall<DestructuredArray> = combine1EqualityCall(
  (param) => param.parts,
  arrayDeepEquality(DestructuredArrayPartKeepDeepEquality),
  destructuredArray,
)

export function BoundParamKeepDeepEquality(): KeepDeepEqualityCall<BoundParam> {
  return (oldValue, newValue) => {
    if (isRegularParam(oldValue) && isRegularParam(newValue)) {
      return RegularParamKeepDeepEquality(oldValue, newValue)
    } else if (isDestructuredObject(oldValue) && isDestructuredObject(newValue)) {
      return DestructuredObjectParamKeepDeepEquality(oldValue, newValue)
    } else if (isDestructuredArray(oldValue) && isDestructuredArray(newValue)) {
      return DestructuredArrayKeepDeepEquality(oldValue, newValue)
    } else {
      return keepDeepEqualityResult(newValue, false)
    }
  }
}

export function ParamKeepDeepEquality(): KeepDeepEqualityCall<Param> {
  return combine2EqualityCalls(
    (param) => param.dotDotDotToken,
    createCallWithTripleEquals(),
    (param) => param.boundParam,
    BoundParamKeepDeepEquality(),
    functionParam,
  )
}

export const UtopiaJSXComponentKeepDeepEquality: KeepDeepEqualityCall<UtopiaJSXComponent> = combine10EqualityCalls(
  (component) => component.name,
  createCallWithTripleEquals(),
  (component) => component.isFunction,
  createCallWithTripleEquals(),
  (component) => component.declarationSyntax,
  createCallWithTripleEquals(),
  (component) => component.blockOrExpression,
  createCallWithTripleEquals(),
  (component) => component.param,
  nullableDeepEquality(ParamKeepDeepEquality()),
  (component) => component.propsUsed,
  arrayDeepEquality(createCallWithTripleEquals()),
  (component) => component.rootElement,
  JSXElementChildKeepDeepEquality(),
  (component) => component.arbitraryJSBlock,
  nullableDeepEquality(ArbitraryJsBlockKeepDeepEquality()),
  (component) => component.usedInReactDOMRender,
  createCallWithTripleEquals(),
  (component) => component.returnStatementComments,
  ParsedCommentsKeepDeepEqualityCall(),
  utopiaJSXComponent,
)

export function CanvasRectangleKeepDeepEquality(
  oldRect: CanvasRectangle,
  newRect: CanvasRectangle,
): KeepDeepEqualityResult<CanvasRectangle> {
  if (
    oldRect.x === newRect.x &&
    oldRect.y === newRect.y &&
    oldRect.width === newRect.width &&
    oldRect.height === newRect.height
  ) {
    return keepDeepEqualityResult(oldRect, true)
  } else {
    return keepDeepEqualityResult(newRect, false)
  }
}

export function LocalRectangleKeepDeepEquality(
  oldRect: LocalRectangle,
  newRect: LocalRectangle,
): KeepDeepEqualityResult<LocalRectangle> {
  if (
    oldRect.x === newRect.x &&
    oldRect.y === newRect.y &&
    oldRect.width === newRect.width &&
    oldRect.height === newRect.height
  ) {
    return keepDeepEqualityResult(oldRect, true)
  } else {
    return keepDeepEqualityResult(newRect, false)
  }
}

export function LocalPointKeepDeepEquality(
  oldPoint: LocalPoint,
  newPoint: LocalPoint,
): KeepDeepEqualityResult<LocalPoint> {
  if (oldPoint.x === newPoint.x && oldPoint.y === newPoint.y) {
    return keepDeepEqualityResult(oldPoint, true)
  } else {
    return keepDeepEqualityResult(newPoint, false)
  }
}

export function SidesKeepDeepEquality(
  oldSides: Sides,
  newSides: Sides,
): KeepDeepEqualityResult<Sides> {
  if (
    oldSides.top === newSides.top &&
    oldSides.left === newSides.left &&
    oldSides.right === newSides.right &&
    oldSides.bottom === newSides.bottom
  ) {
    return keepDeepEqualityResult(oldSides, true)
  } else {
    return keepDeepEqualityResult(newSides, false)
  }
}

export const ImportInfoKeepDeepEquality: KeepDeepEqualityCall<ImportInfo> = EitherKeepDeepEquality<
  'NOT_IMPORTED',
  FoundImportInfo
>(
  createCallWithTripleEquals(),
  combine3EqualityCalls(
    (i) => i.variableName,
    createCallWithTripleEquals(),
    (info) => info.originalName,
    createCallWithTripleEquals(),
    (info) => info.path,
    createCallWithTripleEquals(),
    (variableName, originalName, path): FoundImportInfo => {
      return {
        variableName: variableName,
        originalName: originalName,
        path: path,
      }
    },
  ),
)

export function SpecialSizeMeasurementsKeepDeepEquality(): KeepDeepEqualityCall<
  SpecialSizeMeasurements
> {
  return (oldSize, newSize) => {
    const offsetResult = LocalPointKeepDeepEquality(oldSize.offset, newSize.offset)
    const coordinateSystemBoundsResult = nullableDeepEquality(CanvasRectangleKeepDeepEquality)(
      oldSize.coordinateSystemBounds,
      newSize.coordinateSystemBounds,
    )
    const immediateParentBoundsResult = nullableDeepEquality(CanvasRectangleKeepDeepEquality)(
      oldSize.immediateParentBounds,
      newSize.immediateParentBounds,
    )
    const immediateParentProvidesLayoutResult =
      oldSize.immediateParentProvidesLayout === newSize.immediateParentProvidesLayout
    const usesParentBoundsResult = oldSize.usesParentBounds === newSize.usesParentBounds
    const parentLayoutSystemResult = oldSize.parentLayoutSystem === newSize.parentLayoutSystem
    const layoutSystemForChildrenResult =
      oldSize.layoutSystemForChildren === newSize.layoutSystemForChildren
    const providesBoundsForChildrenResult =
      oldSize.providesBoundsForChildren === newSize.providesBoundsForChildren
    const positionResult = oldSize.position === newSize.position
    const marginResult = SidesKeepDeepEquality(oldSize.margin, newSize.margin)
    const paddingResult = SidesKeepDeepEquality(oldSize.padding, newSize.padding)
    const naturalWidthResult = oldSize.naturalWidth === newSize.naturalWidth
    const naturalHeightResult = oldSize.naturalHeight === newSize.naturalHeight
    const clientWidthResult = oldSize.clientWidth === newSize.clientWidth
    const clientHeightResult = oldSize.clientHeight === newSize.clientHeight
    const parentFlexDirectionResult = oldSize.parentFlexDirection === newSize.parentFlexDirection
    const flexDirectionResult = oldSize.flexDirection === newSize.flexDirection
    const displayEquals = oldSize.display === newSize.display
    const htmlElementNameEquals = oldSize.htmlElementName === newSize.htmlElementName
    const renderedChildrenCount = oldSize.renderedChildrenCount === newSize.renderedChildrenCount
    const areEqual =
      offsetResult.areEqual &&
      coordinateSystemBoundsResult.areEqual &&
      immediateParentBoundsResult.areEqual &&
      immediateParentProvidesLayoutResult &&
      usesParentBoundsResult &&
      parentLayoutSystemResult &&
      layoutSystemForChildrenResult &&
      providesBoundsForChildrenResult &&
      positionResult &&
      marginResult.areEqual &&
      paddingResult.areEqual &&
      naturalWidthResult &&
      naturalHeightResult &&
      clientWidthResult &&
      clientHeightResult &&
      parentFlexDirectionResult &&
      flexDirectionResult &&
      displayEquals &&
      htmlElementNameEquals &&
      renderedChildrenCount
    if (areEqual) {
      return keepDeepEqualityResult(oldSize, true)
    } else {
      const sizeMeasurements = specialSizeMeasurements(
        offsetResult.value,
        coordinateSystemBoundsResult.value,
        immediateParentBoundsResult.value,
        newSize.immediateParentProvidesLayout,
        newSize.usesParentBounds,
        newSize.parentLayoutSystem,
        newSize.layoutSystemForChildren,
        newSize.providesBoundsForChildren,
        newSize.display,
        newSize.position,
        marginResult.value,
        paddingResult.value,
        newSize.naturalWidth,
        newSize.naturalHeight,
        newSize.clientWidth,
        newSize.clientHeight,
        newSize.parentFlexDirection,
        newSize.flexDirection,
        newSize.htmlElementName,
        newSize.renderedChildrenCount,
      )
      return keepDeepEqualityResult(sizeMeasurements, false)
    }
  }
}

export function ElementInstanceMetadataKeepDeepEquality(): KeepDeepEqualityCall<
  ElementInstanceMetadata
> {
  return combine12EqualityCalls(
    (metadata) => metadata.elementPath,
    ElementPathKeepDeepEquality,
    (metadata) => metadata.element,
    EitherKeepDeepEquality(createCallWithTripleEquals(), JSXElementChildKeepDeepEquality()),
    (metadata) => metadata.props,
    createCallFromIntrospectiveKeepDeep(),
    (metadata) => metadata.globalFrame,
    nullableDeepEquality(CanvasRectangleKeepDeepEquality),
    (metadata) => metadata.localFrame,
    nullableDeepEquality(LocalRectangleKeepDeepEquality),
    (metadata) => metadata.componentInstance,
    createCallWithTripleEquals(),
    (metadata) => metadata.isEmotionOrStyledComponent,
    createCallWithTripleEquals(),
    (metadata) => metadata.specialSizeMeasurements,
    SpecialSizeMeasurementsKeepDeepEquality(),
    (metadata) => metadata.computedStyle,
    nullableDeepEquality(objectDeepEquality(createCallWithTripleEquals())),
    (metadata) => metadata.attributeMetadatada,
    createCallFromIntrospectiveKeepDeep(),
    (metadata) => metadata.label,
    nullableDeepEquality(createCallWithTripleEquals()),
    (metadata) => metadata.importInfo,
    nullableDeepEquality(ImportInfoKeepDeepEquality),
    elementInstanceMetadata,
  )
}

export function ElementInstanceMetadataMapKeepDeepEquality(): KeepDeepEqualityCall<
  ElementInstanceMetadataMap
> {
  return objectDeepEquality(ElementInstanceMetadataKeepDeepEquality())
}
