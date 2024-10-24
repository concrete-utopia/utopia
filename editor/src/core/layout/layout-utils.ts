import { AllFramePoints, AllFramePointsExceptSize, LayoutSystem } from 'utopia-api/core'
import type { AllElementProps } from '../../components/editor/store/editor-state'
import {
  transformElementAtPath,
  transformJSXElementAtPath,
} from '../../components/editor/store/editor-state'
import * as EP from '../shared/element-path'
import {
  flatMapEither,
  forEachRight,
  isLeft,
  right,
  foldEither,
  defaultEither,
  mapEither,
  eitherToMaybe,
  left as leftEither,
} from '../shared/either'
import Utils from '../../utils/utils'
import type { CanvasRectangle } from '../shared/math-utils'
import {
  zeroCanvasRect,
  zeroLocalRect,
  parseNumberOrPercent,
  roundTo,
  printNumberOrPercent,
  numberOrPercent,
  isInfinityRectangle,
} from '../shared/math-utils'
import { findJSXElementAtPath, MetadataUtils } from '../model/element-metadata-utils'
import type {
  DetectedLayoutSystem,
  JSXElement,
  UtopiaJSXComponent,
  JSXAttributes,
  SettableLayoutSystem,
  ElementInstanceMetadataMap,
  JSXElementChild,
  ElementInstanceMetadata,
} from '../shared/element-template'
import { jsExpressionValue, isJSXElement, emptyComments } from '../shared/element-template'
import { findJSXElementAtStaticPath } from '../model/element-template-utils'
import type { ValueAtPath } from '../shared/jsx-attributes'
import {
  setJSXValuesAtPaths,
  unsetJSXValuesAtPaths,
  getAllPathsFromAttributes,
} from '../shared/jsx-attributes'
import { getJSXAttributesAtPath, setJSXValueAtPath } from '../shared/jsx-attribute-utils'
import type { PropertyPath, ElementPath } from '../shared/project-file-types'
import { FlexLayoutHelpers } from './layout-helpers'
import type { LayoutPinnedProp, StyleLayoutProp } from './layout-helpers-new'
import { LayoutPinnedProps } from './layout-helpers-new'
import type { CSSPosition } from '../../components/inspector/common/css-utils'
import type { Notice } from '../../components/common/notice'
import { createStylePostActionToast } from './layout-notice'
import { stylePropPathMappingFn } from '../../components/inspector/common/property-path-hooks'
import type { ElementPathTrees } from '../shared/element-path-tree'

interface LayoutPropChangeResult {
  components: UtopiaJSXComponent[]
  componentMetadata: ElementInstanceMetadataMap
  didSwitch: boolean
  toast: Array<Notice>
}

function getParentAxisFromElement(
  element: ElementInstanceMetadata | null,
  propertyTarget: ReadonlyArray<string>,
): 'horizontal' | 'vertical' | null {
  const jsxElement = element?.element ?? leftEither('no parent provided')
  return eitherToMaybe(
    flatMapEither<string, JSXElementChild, 'horizontal' | 'vertical'>((foundJsxElement) => {
      if (isJSXElement(foundJsxElement)) {
        return FlexLayoutHelpers.getMainAxis(propertyTarget, right(foundJsxElement.props))
      } else {
        return leftEither('parent is not JSXElement')
      }
    }, jsxElement),
  )
}

export const PinningAndFlexPoints: Array<LayoutPinnedProp | 'flexBasis'> = [
  ...LayoutPinnedProps,
  'flexBasis',
]

export const PinningAndFlexPointsExceptSize: Array<LayoutPinnedProp | 'flexBasis'> = [
  ...LayoutPinnedProps.filter((p) => p !== 'width' && p !== 'height'),
  'flexBasis',
]

function keepLayoutProps(
  target: ElementPath,
  newParentPath: ElementPath,
  targetOriginalContextMetadata: ElementInstanceMetadataMap,
  currentContextMetadata: ElementInstanceMetadataMap,
  components: UtopiaJSXComponent[],
): SwitchLayoutTypeResult {
  return {
    updatedComponents: components,
    updatedMetadata: currentContextMetadata,
  }
}

export function switchLayoutMetadata(
  metadata: ElementInstanceMetadataMap,
  target: ElementPath,
  parentLayoutSystem: DetectedLayoutSystem | undefined,
  layoutSystemForChildren: DetectedLayoutSystem | undefined,
  position: CSSPosition | undefined,
): ElementInstanceMetadataMap {
  const updatedElements = MetadataUtils.transformAtPathOptionally(
    metadata,
    target,
    (elementMetadata) => {
      return {
        ...elementMetadata,
        specialSizeMeasurements: {
          ...elementMetadata.specialSizeMeasurements,
          /**
           * TECH DEBT HACK: updateFramesOfScenesAndComponents will run after switchPinnedChildToFlex
           * and basically replicating the job done here by calling FlexLayoutHelpers.convertWidthHeightToFlex
           *
           * But that code works based on the metadata, so if we here don't update the metadata to what we assume
           * the measured value will be in the next update, updateFramesOfScenesAndComponents will just undo our
           * frame changes. :(
           */
          parentLayoutSystem:
            parentLayoutSystem ?? elementMetadata.specialSizeMeasurements.parentLayoutSystem,
          layoutSystemForChildren:
            layoutSystemForChildren ??
            elementMetadata.specialSizeMeasurements.layoutSystemForChildren,
          position: position ?? elementMetadata.specialSizeMeasurements.position,
        },
      }
    },
  )
  return updatedElements
}

export function switchPinnedChildToFlex(
  target: ElementPath,
  newParentPath: ElementPath,
  targetOriginalContextMetadata: ElementInstanceMetadataMap,
  currentContextMetadata: ElementInstanceMetadataMap,
  components: UtopiaJSXComponent[],
  newParentMainAxis: 'horizontal' | 'vertical' | null,
  propertyTarget: ReadonlyArray<string>,
): SwitchLayoutTypeResult {
  const currentFrame = MetadataUtils.getLocalFrame(target, targetOriginalContextMetadata, null)
  const newParent = findJSXElementAtPath(newParentPath, components)
  const element = findJSXElementAtPath(target, components)

  let propsToAdd: Array<ValueAtPath> = [
    {
      path: stylePropPathMappingFn('position', propertyTarget),
      value: jsExpressionValue('relative', emptyComments),
    },
  ]

  if (
    currentFrame != null &&
    !isInfinityRectangle(currentFrame) &&
    newParent != null &&
    element != null
  ) {
    // When moving pinned to flex, use width and height to set basis values
    const possibleFlexProps = FlexLayoutHelpers.convertWidthHeightToFlex(
      currentFrame.width,
      currentFrame.height,
      element.props,
      right(newParent.props),
      newParentMainAxis,
      null,
      propertyTarget,
    )

    forEachRight(possibleFlexProps, (flexProps) => {
      const { flexBasis, width, height } = flexProps
      if (flexBasis != null) {
        propsToAdd.push({
          path: stylePropPathMappingFn('flexBasis', propertyTarget),
          value: jsExpressionValue(flexBasis, emptyComments),
        })
      }
      if (width != null) {
        propsToAdd.push({
          path: stylePropPathMappingFn('width', propertyTarget),
          value: jsExpressionValue(width, emptyComments),
        })
      }
      if (height != null) {
        propsToAdd.push({
          path: stylePropPathMappingFn('height', propertyTarget),
          value: jsExpressionValue(height, emptyComments),
        })
      }
    })
  }

  const updatedComponents = transformJSXElementAtPath(components, target, (e: JSXElement) => {
    // Remove the pinning props first...
    const pinnedPropsRemoved = unsetJSXValuesAtPaths(e.props, [
      stylePropPathMappingFn('left', propertyTarget),
      stylePropPathMappingFn('top', propertyTarget),
      stylePropPathMappingFn('bottom', propertyTarget),
      stylePropPathMappingFn('right', propertyTarget),
      stylePropPathMappingFn('width', propertyTarget),
      stylePropPathMappingFn('height', propertyTarget),
      stylePropPathMappingFn('position', propertyTarget),
    ])
    // ...Add in the flex properties.
    const flexPropsAdded = flatMapEither(
      (props) => setJSXValuesAtPaths(props, propsToAdd),
      pinnedPropsRemoved,
    )
    if (isLeft(flexPropsAdded)) {
      return e
    } else {
      return {
        ...e,
        props: flexPropsAdded.value,
      }
    }
  })

  const updatedMetadata = switchLayoutMetadata(
    currentContextMetadata,
    target,
    'flex',
    undefined,
    'static',
  )

  return {
    updatedComponents: updatedComponents,
    updatedMetadata: updatedMetadata,
  }
}

export function switchFlexToFlexDifferentAxis(
  target: ElementPath,
  newParentPath: ElementPath,
  targetOriginalContextMetadata: ElementInstanceMetadataMap,
  currentContextMetadata: ElementInstanceMetadataMap,
  components: UtopiaJSXComponent[],
  newParentMainAxis: 'horizontal' | 'vertical' | null,
  propertyTarget: ReadonlyArray<string>,
): SwitchLayoutTypeResult {
  const element = findJSXElementAtPath(target, components)

  // If the element exists, has props, and the props DO NOT contain width, height or flexBasis, just skip conversion
  if (element != null) {
    const allAttributePaths = getAllPathsFromAttributes(element.props)
    if (
      !allAttributePaths.includes(stylePropPathMappingFn('width', propertyTarget)) &&
      !allAttributePaths.includes(stylePropPathMappingFn('height', propertyTarget)) &&
      !allAttributePaths.includes(stylePropPathMappingFn('flexBasis', propertyTarget))
    ) {
      // we leave the element alone
      return {
        updatedComponents: components,
        updatedMetadata: currentContextMetadata,
      }
    }
  }

  // otherwise, we run a regular switchPinnedChildToFlex
  return switchPinnedChildToFlex(
    target,
    newParentPath,
    targetOriginalContextMetadata,
    currentContextMetadata,
    components,
    newParentMainAxis,
    propertyTarget,
  )
}

interface SwitchLayoutTypeResult {
  updatedComponents: UtopiaJSXComponent[]
  updatedMetadata: ElementInstanceMetadataMap
}

export function switchFlexChildToPinned(
  target: ElementPath,
  newParentPath: ElementPath,
  targetOriginalContextMetadata: ElementInstanceMetadataMap,
  currentContextMetadata: ElementInstanceMetadataMap,
  components: UtopiaJSXComponent[],
  newParentMainAxis: 'horizontal' | 'vertical' | null,
  propertyTarget: ReadonlyArray<string>,
  allElementProps: AllElementProps,
): SwitchLayoutTypeResult {
  // TODO How should this behave if there is no rendered frame?
  const currentFrame = MetadataUtils.getFrameOrZeroRect(target, targetOriginalContextMetadata)
  const elementProps = allElementProps[EP.toString(target)]
  const newParentProps = allElementProps[EP.toString(newParentPath)]

  // When moving flex to pinned, use fixed values or basis values to set width and height
  // FIXME Right now this isn't taking into account groups
  const unstretched = FlexLayoutHelpers.getUnstretchedWidthHeight(
    elementProps ?? {},
    newParentProps ?? {},
  )
  const width = Utils.defaultIfNull(currentFrame.width, unstretched.width)
  const height = Utils.defaultIfNull(currentFrame.height, unstretched.height)
  const oldParentFrame = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
    EP.parentPath(target),
    targetOriginalContextMetadata,
  )

  const newParentFrame = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
    newParentPath,
    currentContextMetadata,
  )

  const newOffset = Utils.pointDifference(newParentFrame, oldParentFrame)

  const updatedComponents = removeFlexAndAddPinnedPropsToComponent(
    target,
    components,
    newOffset.y + currentFrame.y,
    newOffset.x + currentFrame.x,
    width,
    height,
    propertyTarget,
  )

  const updatedMetadata = switchLayoutMetadata(
    currentContextMetadata,
    target,
    'flow',
    undefined,
    'absolute',
  )

  return {
    updatedComponents: updatedComponents,
    updatedMetadata: updatedMetadata,
  }
}

export function switchFlexChildToGroup(
  target: ElementPath,
  newParentPath: ElementPath,
  targetOriginalContextMetadata: ElementInstanceMetadataMap,
  currentContextMetadata: ElementInstanceMetadataMap,
  components: UtopiaJSXComponent[],
  newParentMainAxis: 'horizontal' | 'vertical' | null,
  propertyTarget: Array<string>,
  allElementProps: AllElementProps,
): SwitchLayoutTypeResult {
  const currentFrame = MetadataUtils.getFrameOrZeroRect(target, targetOriginalContextMetadata)
  // TODO How should this behave if there is no rendered frame?
  const elementProps = allElementProps[EP.toString(target)]
  const newParentProps = allElementProps[EP.toString(newParentPath)]

  // When moving flex to pinned, use fixed values or basis values to set width and height
  const unstretched = FlexLayoutHelpers.getUnstretchedWidthHeight(
    elementProps ?? {},
    newParentProps ?? {},
  )
  const width = Utils.defaultIfNull(currentFrame.width, unstretched.width)
  const height = Utils.defaultIfNull(currentFrame.height, unstretched.height)
  const oldParentFrame = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
    EP.parentPath(target),
    targetOriginalContextMetadata,
  )
  const newParentFrame = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
    newParentPath,
    currentContextMetadata,
  )
  const newOffset = Utils.pointDifference(newParentFrame, oldParentFrame)

  const updatedComponents = removeFlexAndNonDefaultPinsAddPinnedPropsToComponent(
    target,
    components,
    newOffset.y + currentFrame.y,
    newOffset.x + currentFrame.x,
    width,
    height,
    propertyTarget,
  )

  const updatedMetadata = switchLayoutMetadata(
    currentContextMetadata,
    target,
    'flow',
    undefined,
    'absolute',
  )

  return {
    updatedComponents: updatedComponents,
    updatedMetadata: updatedMetadata,
  }
}

export function switchChildToGroupWithParentFrame(
  target: ElementPath,
  originalPath: ElementPath,
  componentMetadata: ElementInstanceMetadataMap,
  components: UtopiaJSXComponent[],
  parentFrame: CanvasRectangle,
  propertyTarget: ReadonlyArray<string>,
  allElementProps: AllElementProps,
): SwitchLayoutTypeResult {
  const isParentFlexContainer =
    MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      target,
      componentMetadata,
    )
  const currentFrame = MetadataUtils.getFrameOrZeroRect(originalPath, componentMetadata)
  // TODO How should this behave if there is no rendered frame?
  const elementProps = allElementProps[EP.toString(target)]
  const oldParentFrame = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
    EP.parentPath(originalPath),
    componentMetadata,
  )
  const newOffset = Utils.pointDifference(parentFrame, oldParentFrame)

  if (isParentFlexContainer) {
    // When moving flex to pinned, use fixed values or basis values to set width and height
    // TODO LAYOUT unstretched can be different from other switchTo cases, here we don't have access to props from metadata
    const unstretched = FlexLayoutHelpers.getUnstretchedWidthHeight(elementProps ?? {}, {})
    const width = Utils.defaultIfNull(currentFrame.width, unstretched.width)
    const height = Utils.defaultIfNull(currentFrame.height, unstretched.height)

    const updatedComponents = removeFlexAndNonDefaultPinsAddPinnedPropsToComponent(
      target,
      components,
      newOffset.y + currentFrame.y,
      newOffset.x + currentFrame.x,
      width,
      height,
      propertyTarget,
    )

    const updatedMetadata = switchLayoutMetadata(
      componentMetadata,
      target,
      'flow',
      undefined,
      'absolute',
    )

    return {
      updatedComponents: updatedComponents,
      updatedMetadata: updatedMetadata,
    }
  } else {
    const width = currentFrame.width
    const height = currentFrame.height

    const updatedComponents = changePinsToDefaultOnComponent(
      target,
      components,
      newOffset.y + currentFrame.y,
      newOffset.x + currentFrame.x,
      width,
      height,
      propertyTarget,
    )

    const updatedMetadata = switchLayoutMetadata(
      componentMetadata,
      target,
      'flow',
      undefined,
      'absolute',
    )

    return {
      updatedComponents: updatedComponents,
      updatedMetadata: updatedMetadata,
    }
  }
}

export function switchPinnedChildToGroup(
  target: ElementPath,
  newParentPath: ElementPath,
  targetOriginalContextMetadata: ElementInstanceMetadataMap,
  currentContextMetadata: ElementInstanceMetadataMap,
  components: UtopiaJSXComponent[],
  newParentMainAxis: 'horizontal' | 'vertical' | null,
  propertyTarget: Array<string>,
): SwitchLayoutTypeResult {
  const currentFrame = MetadataUtils.getFrameOrZeroRect(target, targetOriginalContextMetadata)
  // TODO How should this behave if there is no rendered frame?
  const oldParentFrame = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
    EP.parentPath(target),
    currentContextMetadata,
  )
  const newParentFrame = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
    newParentPath,
    currentContextMetadata,
  )
  const newOffset = Utils.pointDifference(newParentFrame, oldParentFrame)
  const width = currentFrame.width
  const height = currentFrame.height

  const updatedComponents = changePinsToDefaultOnComponent(
    target,
    components,
    newOffset.y + currentFrame.y,
    newOffset.x + currentFrame.x,
    width,
    height,
    propertyTarget,
  )

  const updatedMetadata = switchLayoutMetadata(
    currentContextMetadata,
    target,
    'flow',
    undefined,
    'absolute',
  )

  return {
    updatedComponents: updatedComponents,
    updatedMetadata: updatedMetadata,
  }
}

export function switchChildToPinnedWithParentFrame(
  target: ElementPath,
  originalPath: ElementPath,
  componentMetadata: ElementInstanceMetadataMap,
  components: UtopiaJSXComponent[],
  parentFrame: CanvasRectangle,
  propertyTarget: ReadonlyArray<string>,
  allElementProps: AllElementProps,
): SwitchLayoutTypeResult {
  const currentFrame = MetadataUtils.getFrameOrZeroRect(originalPath, componentMetadata)
  // TODO How should this behave if there is no rendered frame?
  const elementProps = allElementProps[EP.toString(originalPath)]
  const oldParentFrame = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
    EP.parentPath(originalPath),
    componentMetadata,
  )
  const newOffset = Utils.pointDifference(parentFrame, oldParentFrame)

  // When moving flex to pinned, use fixed values or basis values to set width and height
  // TODO LAYOUT unstretched can be different from other switchTo cases, here we don't have access to props from metadata
  const unstretched = FlexLayoutHelpers.getUnstretchedWidthHeight(elementProps ?? {}, {})
  const width = Utils.defaultIfNull(currentFrame.width, unstretched.width)
  const height = Utils.defaultIfNull(currentFrame.height, unstretched.height)

  const updatedComponents = removeFlexAndAddPinnedPropsToComponent(
    target,
    components,
    newOffset.y + currentFrame.y,
    newOffset.x + currentFrame.x,
    width,
    height,
    propertyTarget,
  )

  const updatedMetadata = switchLayoutMetadata(
    componentMetadata,
    target,
    'flow',
    undefined,
    'absolute',
  )

  return {
    updatedComponents: updatedComponents,
    updatedMetadata: updatedMetadata,
  }
}

function removeFlexAndNonDefaultPinsAddPinnedPropsToComponent(
  target: ElementPath,
  components: UtopiaJSXComponent[],
  top: number,
  left: number,
  width: string | number,
  height: string | number,
  propertyTarget: ReadonlyArray<string>,
) {
  const propsToAdd: Array<ValueAtPath> = [
    {
      path: stylePropPathMappingFn('left', propertyTarget),
      value: jsExpressionValue(left, emptyComments),
    },
    {
      path: stylePropPathMappingFn('top', propertyTarget),
      value: jsExpressionValue(top, emptyComments),
    },
    {
      path: stylePropPathMappingFn('width', propertyTarget),
      value: jsExpressionValue(width, emptyComments),
    },
    {
      path: stylePropPathMappingFn('height', propertyTarget),
      value: jsExpressionValue(height, emptyComments),
    },
  ]

  const propsToRemove: Array<StyleLayoutProp> = ['bottom', 'right', 'flexBasis']

  return transformJSXElementAtPath(components, target, (e: JSXElement) => {
    const flexPropsRemoved = unsetJSXValuesAtPaths(
      e.props,
      propsToRemove.map((p) => stylePropPathMappingFn(p, propertyTarget)),
    )
    const pinnedPropsAdded = flatMapEither(
      (props) => setJSXValuesAtPaths(props, propsToAdd),
      flexPropsRemoved,
    )
    if (isLeft(pinnedPropsAdded)) {
      return e
    } else {
      return {
        ...e,
        props: pinnedPropsAdded.value,
      }
    }
  })
}

function removeFlexAndAddPinnedPropsToComponent(
  target: ElementPath,
  components: UtopiaJSXComponent[],
  top: number,
  left: number,
  width: string | number,
  height: string | number,
  propertyTarget: ReadonlyArray<string>,
) {
  const propsToAdd: Array<ValueAtPath> = [
    {
      path: stylePropPathMappingFn('left', propertyTarget),
      value: jsExpressionValue(left, emptyComments),
    },
    {
      path: stylePropPathMappingFn('top', propertyTarget),
      value: jsExpressionValue(top, emptyComments),
    },
    {
      path: stylePropPathMappingFn('width', propertyTarget),
      value: jsExpressionValue(width, emptyComments),
    },
    {
      path: stylePropPathMappingFn('height', propertyTarget),
      value: jsExpressionValue(height, emptyComments),
    },
    {
      path: stylePropPathMappingFn('position', propertyTarget),
      value: jsExpressionValue('absolute', emptyComments),
    },
  ]
  const propsToRemove: Array<StyleLayoutProp> = ['flexBasis']

  return transformJSXElementAtPath(components, target, (e: JSXElement) => {
    const flexPropsRemoved = unsetJSXValuesAtPaths(
      e.props,
      propsToRemove.map((p) => stylePropPathMappingFn(p, propertyTarget)),
    )
    const pinnedPropsAdded = flatMapEither(
      (props) => setJSXValuesAtPaths(props, propsToAdd),
      flexPropsRemoved,
    )
    if (isLeft(pinnedPropsAdded)) {
      return e
    } else {
      return {
        ...e,
        props: pinnedPropsAdded.value,
      }
    }
  })
}

function changePinsToDefaultOnComponent(
  target: ElementPath,
  components: UtopiaJSXComponent[],
  top: number,
  left: number,
  width: string | number,
  height: string | number,
  propertyTarget: ReadonlyArray<string>,
) {
  const propsToAdd: Array<ValueAtPath> = [
    {
      path: stylePropPathMappingFn('left', propertyTarget),
      value: jsExpressionValue(left, emptyComments),
    },
    {
      path: stylePropPathMappingFn('top', propertyTarget),
      value: jsExpressionValue(top, emptyComments),
    },
    {
      path: stylePropPathMappingFn('width', propertyTarget),
      value: jsExpressionValue(width, emptyComments),
    },
    {
      path: stylePropPathMappingFn('height', propertyTarget),
      value: jsExpressionValue(height, emptyComments),
    },
  ]
  const propsToRemove: Array<StyleLayoutProp> = ['bottom', 'right']
  return transformJSXElementAtPath(components, target, (e: JSXElement) => {
    const pinPropsRemoved = unsetJSXValuesAtPaths(
      e.props,
      propsToRemove.map((p) => stylePropPathMappingFn(p, propertyTarget)),
    )
    const pinnedPropsAdded = flatMapEither(
      (props) => setJSXValuesAtPaths(props, propsToAdd),
      pinPropsRemoved,
    )
    if (isLeft(pinnedPropsAdded)) {
      return e
    } else {
      return {
        ...e,
        props: pinnedPropsAdded.value,
      }
    }
  })
}

function propertiesToRound(propertyTarget: Array<string>): Array<PropertyPath> {
  return [
    stylePropPathMappingFn('flexBasis', propertyTarget),
    stylePropPathMappingFn('left', propertyTarget),
    stylePropPathMappingFn('top', propertyTarget),
    stylePropPathMappingFn('width', propertyTarget),
    stylePropPathMappingFn('height', propertyTarget),
    stylePropPathMappingFn('right', propertyTarget),
    stylePropPathMappingFn('bottom', propertyTarget),
  ]
}

export function roundAttributeLayoutValues(
  propertyTarget: Array<string>,
  jsxAttributes: JSXAttributes,
): JSXAttributes {
  return propertiesToRound(propertyTarget).reduce((workingAttributes, propertyToRound) => {
    // Lookup the attribute given the property path.
    const attributeResult = getJSXAttributesAtPath(workingAttributes, propertyToRound)
    const value = attributeResult.attribute
    switch (value.type) {
      case 'ATTRIBUTE_VALUE':
      case 'PART_OF_ATTRIBUTE_VALUE':
        // Attempt to parse the value in the attribute.
        const valueToRound: unknown = value.value
        const parsedNumber = parseNumberOrPercent(valueToRound)
        // Handle the result of the parse.
        return foldEither(
          (_) => {
            // Can't parse the value, so fallback to not touching it.
            return workingAttributes
          },
          (numberToRound) => {
            // Round the underlying number.
            const roundedNumber = numberToRound.isPercent
              ? roundTo(numberToRound.value, 1)
              : Math.round(numberToRound.value)
            // Minimise changes to the model where possible.
            if (roundedNumber === numberToRound.value) {
              return workingAttributes
            } else {
              // Build the new representation of it.
              const rounded = printNumberOrPercent(
                numberOrPercent(roundedNumber, numberToRound.isPercent),
              )
              // Update the attribute itself.
              const withValueSet = setJSXValueAtPath(
                workingAttributes,
                propertyToRound,
                jsExpressionValue(rounded, emptyComments),
              )
              // Should we (unexpectedly) be unable to set the value, default the result.
              return defaultEither(workingAttributes, withValueSet)
            }
          },
          parsedNumber,
        )
      // These can't be rounded for various semi-obvious reasons.
      case 'ATTRIBUTE_NOT_FOUND':
      case 'ATTRIBUTE_NESTED_ARRAY':
      case 'ATTRIBUTE_NESTED_OBJECT':
      case 'ATTRIBUTE_FUNCTION_CALL':
      case 'JSX_MAP_EXPRESSION':
      case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      case 'JS_PROPERTY_ACCESS':
      case 'JS_ELEMENT_ACCESS':
      case 'JS_IDENTIFIER':
      case 'JSX_ELEMENT':
        return workingAttributes
      default:
        const _exhaustiveCheck: never = value
        throw new Error(`Unhandled type ${JSON.stringify(value)}`)
    }
  }, jsxAttributes)
}

export function roundJSXElementLayoutValues(
  propertyTarget: Array<string>,
  element: JSXElement,
): JSXElement {
  return {
    ...element,
    props: roundAttributeLayoutValues(propertyTarget, element.props),
  }
}
