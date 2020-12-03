import { Sides } from 'utopia-api'
import {
  ComponentMetadata,
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
  isPropertyAssignment,
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
  jsxMetadata,
  JSXMetadata,
  JSXProperty,
  jsxPropertyAssignment,
  JSXPropertyAssignment,
  jsxSpreadAssignment,
  JSXSpreadAssignment,
  JSXTextBlock,
  specialSizeMeasurements,
  SpecialSizeMeasurements,
} from '../../../core/shared/element-template'
import { CanvasRectangle, LocalPoint, LocalRectangle } from '../../../core/shared/math-utils'
import {
  KeepDeepEqualityResult,
  keepDeepEqualityResult,
  KeepDeepEqualityCall,
  combine3EqualityCalls,
  combine6EqualityCalls,
  nullableDeepEquality,
  createCallWithTripleEquals,
  combine9EqualityCalls,
  objectDeepEquality,
  mapKeepDeepEqualityResult,
  combine5EqualityCalls,
  arrayDeepEquality,
  combine1EqualityCall,
  combine2EqualityCalls,
  combine7EqualityCalls,
  combine8EqualityCalls,
  undefinableDeepEquality,
  combine4EqualityCalls,
  combine10EqualityCalls,
} from '../../../utils/deep-equality'
import {
  TemplatePathArrayKeepDeepEquality,
  HigherOrderControlArrayKeepDeepEquality,
  TemplatePathKeepDeepEquality,
  EitherKeepDeepEquality,
  InstancePathKeepDeepEquality,
  InstancePathArrayKeepDeepEquality,
  JSXElementNameKeepDeepEqualityCall,
  ScenePathKeepDeepEquality,
} from '../../../utils/deep-equality-instances'
import { createCallFromIntrospectiveKeepDeep } from '../../../utils/react-performance'
import {
  TransientCanvasState,
  TransientFileState,
  transientCanvasState,
  DerivedState,
} from './editor-state'

export function TransientCanvasStateKeepDeepEquality(): KeepDeepEqualityCall<TransientCanvasState> {
  return combine4EqualityCalls(
    (state) => state.selectedViews,
    TemplatePathArrayKeepDeepEquality,
    (state) => state.highlightedViews,
    TemplatePathArrayKeepDeepEquality,
    (state) => state.fileState,
    createCallFromIntrospectiveKeepDeep<TransientFileState | null>(),
    (state) => state.reparentTargetPositions,
    createCallFromIntrospectiveKeepDeep(),
    transientCanvasState,
  )
}

export function DerivedStateKeepDeepEquality(): KeepDeepEqualityCall<DerivedState> {
  return combine6EqualityCalls(
    (state) => state.navigatorTargets,
    TemplatePathArrayKeepDeepEquality,
    (state) => state.visibleNavigatorTargets,
    TemplatePathArrayKeepDeepEquality,
    (state) => state.canvas.descendantsOfHiddenInstances,
    TemplatePathArrayKeepDeepEquality,
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

export function JSXAttributeValueKeepDeepEqualityCall<T>(): KeepDeepEqualityCall<
  JSXAttributeValue<T>
> {
  return combine1EqualityCall(
    (attribute) => attribute.value,
    createCallFromIntrospectiveKeepDeep<T>(),
    jsxAttributeValue,
  )
}

export function JSXAttributeOtherJavaScriptKeepDeepEqualityCall(): KeepDeepEqualityCall<
  JSXAttributeOtherJavaScript
> {
  return combine5EqualityCalls(
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
    (javascript, transpiledJavascript, definedElsewhere, sourceMap, uniqueID) => {
      return {
        type: 'ATTRIBUTE_OTHER_JAVASCRIPT',
        javascript: javascript,
        transpiledJavascript: transpiledJavascript,
        definedElsewhere: definedElsewhere,
        sourceMap: sourceMap,
        uniqueID: uniqueID,
      }
    },
  )
}

export function JSXArrayValueKeepDeepEqualityCall(): KeepDeepEqualityCall<JSXArrayValue> {
  return combine1EqualityCall(
    (value) => value.value,
    JSXAttributeKeepDeepEqualityCall(),
    jsxArrayValue,
  )
}

export function JSXArraySpreadKeepDeepEqualityCall(): KeepDeepEqualityCall<JSXArraySpread> {
  return combine1EqualityCall(
    (value) => value.value,
    JSXAttributeKeepDeepEqualityCall(),
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
  return combine1EqualityCall(
    (attribute) => attribute.content,
    arrayDeepEquality(JSXArrayElementKeepDeepEqualityCall()),
    jsxAttributeNestedArray,
  )
}

export function JSXSpreadAssignmentKeepDeepEqualityCall(): KeepDeepEqualityCall<
  JSXSpreadAssignment
> {
  return combine1EqualityCall(
    (value) => value.value,
    JSXAttributeKeepDeepEqualityCall(),
    jsxSpreadAssignment,
  )
}

export function JSXPropertyAssignmentKeepDeepEqualityCall(): KeepDeepEqualityCall<
  JSXPropertyAssignment
> {
  return combine2EqualityCalls(
    (value) => value.key,
    createCallWithTripleEquals(),
    (value) => value.value,
    JSXAttributeKeepDeepEqualityCall(),
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
  return combine1EqualityCall(
    (attribute) => attribute.content,
    arrayDeepEquality(JSXPropertyKeepDeepEqualityCall()),
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

export function JSXAttributesKeepDeepEqualityCall(): KeepDeepEqualityCall<JSXAttributes> {
  return objectDeepEquality(JSXAttributeKeepDeepEqualityCall())
}

export function JSXElementKeepDeepEquality(): KeepDeepEqualityCall<JSXElement> {
  return combine3EqualityCalls(
    (element) => element.name,
    JSXElementNameKeepDeepEqualityCall(),
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
      parentFlexDirectionResult
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
        newSize.position,
        marginResult.value,
        paddingResult.value,
        newSize.naturalWidth,
        newSize.naturalHeight,
        newSize.clientWidth,
        newSize.clientHeight,
        newSize.parentFlexDirection,
      )
      return keepDeepEqualityResult(sizeMeasurements, false)
    }
  }
}

export function ElementInstanceMetadataKeepDeepEquality(): KeepDeepEqualityCall<
  ElementInstanceMetadata
> {
  return combine10EqualityCalls(
    (metadata) => metadata.templatePath,
    InstancePathKeepDeepEquality,
    (metadata) => metadata.element,
    EitherKeepDeepEquality(createCallWithTripleEquals(), JSXElementChildKeepDeepEquality()),
    (metadata) => metadata.props,
    createCallFromIntrospectiveKeepDeep(),
    (metadata) => metadata.globalFrame,
    nullableDeepEquality(CanvasRectangleKeepDeepEquality),
    (metadata) => metadata.localFrame,
    nullableDeepEquality(LocalRectangleKeepDeepEquality),
    (metadata) => metadata.children,
    InstancePathArrayKeepDeepEquality,
    (metadata) => metadata.componentInstance,
    createCallWithTripleEquals(),
    (metadata) => metadata.componentInstance,
    createCallWithTripleEquals(),
    (metadata) => metadata.specialSizeMeasurements,
    SpecialSizeMeasurementsKeepDeepEquality(),
    (metadata) => metadata.computedStyle,
    nullableDeepEquality(objectDeepEquality(createCallWithTripleEquals())),
    elementInstanceMetadata,
  )
}

export function ElementInstanceMetadataMapKeepDeepEquality(): KeepDeepEqualityCall<
  ElementInstanceMetadataMap
> {
  return objectDeepEquality(ElementInstanceMetadataKeepDeepEquality())
}

export function ComponentMetadataKeepDeepEquality(): KeepDeepEqualityCall<ComponentMetadata> {
  return combine8EqualityCalls(
    (metadata) => metadata.scenePath,
    ScenePathKeepDeepEquality,
    (metadata) => metadata.templatePath,
    InstancePathKeepDeepEquality,
    (metadata) => metadata.rootElements,
    InstancePathArrayKeepDeepEquality,
    (metadata) => metadata.component,
    nullableDeepEquality(createCallWithTripleEquals()),
    (metadata) => metadata.globalFrame,
    nullableDeepEquality(CanvasRectangleKeepDeepEquality),
    (metadata) => metadata.sceneResizesContent,
    createCallWithTripleEquals(),
    (metadata) => metadata.label,
    undefinableDeepEquality(createCallWithTripleEquals()),
    (metadata) => metadata.style,
    createCallFromIntrospectiveKeepDeep(),
    (
      scenePath,
      templatePath,
      rootElements,
      component,
      globalFrame,
      sceneResizesContent,
      label,
      style,
    ) => {
      return {
        scenePath: scenePath,
        templatePath: templatePath,
        rootElements: rootElements,
        component: component,
        globalFrame: globalFrame,
        sceneResizesContent: sceneResizesContent,
        label: label,
        style: style,
      }
    },
  )
}

export function JSXMetadataKeepDeepEquality(): KeepDeepEqualityCall<JSXMetadata> {
  return combine2EqualityCalls(
    (metadata) => metadata.components,
    arrayDeepEquality(ComponentMetadataKeepDeepEquality()),
    (metadata) => metadata.elements,
    ElementInstanceMetadataMapKeepDeepEquality(),
    jsxMetadata,
  )
}
