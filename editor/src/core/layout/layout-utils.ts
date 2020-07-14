import { AllFramePoints, AllFramePointsExceptSize } from 'utopia-api'
import { transformElementAtPath } from '../../components/editor/store/editor-state'
import * as TP from '../shared/template-path'
import {
  flatMapEither,
  forEachRight,
  isLeft,
  right,
  foldEither,
  defaultEither,
} from '../shared/either'
import Utils from '../../utils/utils'
import {
  CanvasRectangle,
  zeroCanvasRect,
  zeroLocalRect,
  parseNumberOrPercent,
  roundTo,
  printNumberOrPercent,
  numberOrPercent,
} from '../shared/math-utils'
import { findJSXElementAtPath, MetadataUtils } from '../model/element-metadata-utils'
import {
  ComponentMetadata,
  DetectedLayoutSystem,
  isJSXElement,
  jsxAttributeValue,
  JSXElement,
  UtopiaJSXComponent,
  JSXAttributes,
  SettableLayoutSystem,
} from '../shared/element-template'
import { findJSXElementChildAtPath } from '../model/element-template-utils'
import {
  setJSXValuesAtPaths,
  unsetJSXValuesAtPaths,
  ValueAtPath,
  getJSXAttributeAtPath,
  ModifiableAttribute,
  setJSXValueAtPath,
} from '../shared/jsx-attributes'
import { InstancePath, PropertyPath } from '../shared/project-file-types'
import { FlexLayoutHelpers } from './layout-helpers'
import {
  createLayoutPropertyPath,
  LayoutProp,
  pinnedPropForFramePoint,
  StyleLayoutProp,
} from './layout-helpers-new'
import { getLayoutPropertyOr } from './getLayoutProperty'
import { CSSPosition, layoutEmptyValues } from '../../components/inspector/common/css-utils'

interface LayoutPropChangeResult {
  components: UtopiaJSXComponent[]
  componentMetadata: ComponentMetadata[]
  didSwitch: boolean
}

export function maybeSwitchChildrenLayoutProps(
  target: InstancePath,
  targetOriginalContextMetadata: ComponentMetadata[],
  currentContextMetadata: ComponentMetadata[],
  originalComponents: UtopiaJSXComponent[],
  components: UtopiaJSXComponent[],
): LayoutPropChangeResult {
  const children = MetadataUtils.getChildrenHandlingGroups(
    targetOriginalContextMetadata,
    target,
    true,
  )
  const result = children.reduce(
    (working, next) => {
      const { components: workingComponents, didSwitch: workingDidSwitch } = working
      const {
        components: nextComponents,
        componentMetadata: nextMetadata,
        didSwitch: nextDidSwitch,
      } = maybeSwitchLayoutProps(
        next.templatePath,
        next.templatePath,
        target,
        targetOriginalContextMetadata,
        currentContextMetadata,
        originalComponents,
        workingComponents,
        null,
        null,
      )
      return {
        components: nextComponents,
        componentMetadata: nextMetadata,
        didSwitch: workingDidSwitch || nextDidSwitch,
      }
    },
    { components: components, componentMetadata: currentContextMetadata, didSwitch: false },
  )
  return result
}

export function maybeSwitchLayoutProps(
  target: InstancePath,
  originalPath: InstancePath,
  newParentPath: InstancePath,
  targetOriginalContextMetadata: ComponentMetadata[],
  currentContextMetadata: ComponentMetadata[],
  originalComponents: UtopiaJSXComponent[],
  components: UtopiaJSXComponent[],
  parentFrame: CanvasRectangle | null,
  parentLayoutSystem: SettableLayoutSystem | null,
): LayoutPropChangeResult {
  const originalParentPath = TP.parentPath(originalPath)
  const originalParent = TP.isInstancePath(originalParentPath)
    ? MetadataUtils.getElementByInstancePathMaybe(targetOriginalContextMetadata, originalParentPath)
    : null
  const newParent = TP.isInstancePath(newParentPath)
    ? MetadataUtils.getElementByInstancePathMaybe(currentContextMetadata, newParentPath)
    : null

  let wasFlexContainer = MetadataUtils.isFlexLayoutedContainer(originalParent)
  let isFlexContainer =
    parentLayoutSystem === 'flex' || MetadataUtils.isFlexLayoutedContainer(newParent)
  let wasGroup = MetadataUtils.isGroup(originalParentPath, targetOriginalContextMetadata)
  let isGroup = MetadataUtils.isGroup(newParentPath, currentContextMetadata)

  // When wrapping elements in view/group the element is not available from the componentMetadata but we know the frame already.
  if (newParent == null && parentFrame != null) {
    // FIXME wrapping in a view now always switches to pinned props. maybe the user wants to keep the parent layoutsystem?
    const switchLayoutFunction =
      parentLayoutSystem === 'group'
        ? switchChildToGroupWithParentFrame
        : switchChildToPinnedWithParentFrame
    const { updatedComponents, updatedMetadata } = switchLayoutFunction(
      target,
      originalPath,
      targetOriginalContextMetadata,
      components,
      parentFrame,
    )
    return {
      components: updatedComponents,
      componentMetadata: updatedMetadata,
      didSwitch: true,
    }
  } else {
    const switchLayoutFunction = getLayoutFunction(
      wasFlexContainer,
      isFlexContainer,
      wasGroup,
      isGroup,
    )
    const { updatedComponents, updatedMetadata } = switchLayoutFunction.layoutFn(
      target,
      newParentPath,
      targetOriginalContextMetadata,
      currentContextMetadata,
      components,
    )
    return {
      components: updatedComponents,
      componentMetadata: updatedMetadata,
      didSwitch: switchLayoutFunction.didSwitch,
    }
  }
}

function getLayoutFunction(
  wasFlexContainer: boolean,
  isFlexContainer: boolean,
  wasGroup: boolean,
  isGroup: boolean,
): {
  layoutFn: (
    target: InstancePath,
    newParentPath: InstancePath,
    targetOriginalContextMetadata: ComponentMetadata[],
    currentContextMetadata: ComponentMetadata[],
    components: UtopiaJSXComponent[],
  ) => SwitchLayoutTypeResult
  didSwitch: boolean
} {
  if (wasFlexContainer) {
    if (isGroup) {
      // From flex to a group
      return {
        layoutFn: switchFlexChildToGroup,
        didSwitch: true,
      }
    } else if (isFlexContainer) {
      // From flex to flex
      return {
        layoutFn: keepLayoutProps,
        didSwitch: false,
      }
    } else {
      // From flex to pinned
      return {
        layoutFn: switchFlexChildToPinned,
        didSwitch: true,
      }
    }
  } else if (wasGroup) {
    if (isFlexContainer) {
      // From a group to flex
      return {
        layoutFn: switchPinnedChildToFlex,
        didSwitch: true,
      }
    } else if (isGroup) {
      // From group to group
      return {
        layoutFn: keepLayoutProps,
        didSwitch: false,
      }
    } else {
      // From a group to pinned
      return {
        layoutFn: keepLayoutProps,
        didSwitch: false,
      }
    }
  } else {
    // wasPinned
    if (isFlexContainer) {
      // From pinned to flex
      return {
        layoutFn: switchPinnedChildToFlex,
        didSwitch: true,
      }
    } else if (isGroup) {
      // From pinned to a group
      return {
        layoutFn: switchPinnedChildToGroup,
        didSwitch: true,
      }
    } else {
      // From pinned to pinned
      return {
        layoutFn: keepLayoutProps,
        didSwitch: false,
      }
    }
  }
}

export const PinningAndFlexPoints = [...AllFramePoints, 'FlexFlexBasis', 'FlexCrossBasis']

export const PinningAndFlexPointsExceptSize = [
  ...AllFramePointsExceptSize,
  'FlexFlexBasis',
  'FlexCrossBasis',
]

function keepLayoutProps(
  target: InstancePath,
  newParentPath: InstancePath,
  targetOriginalContextMetadata: ComponentMetadata[],
  currentContextMetadata: ComponentMetadata[],
  components: UtopiaJSXComponent[],
): SwitchLayoutTypeResult {
  return {
    updatedComponents: components,
    updatedMetadata: currentContextMetadata,
  }
}

export function switchLayoutMetadata(
  components: ComponentMetadata[],
  target: InstancePath,
  parentLayoutSystem: DetectedLayoutSystem | undefined,
  layoutSystemForChildren: DetectedLayoutSystem | undefined,
  position: CSSPosition | undefined,
): ComponentMetadata[] {
  return MetadataUtils.transformAtPathOptionally(components, target, (elementMetadata) => {
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
  }).elements
}

export function switchPinnedChildToFlex(
  target: InstancePath,
  newParentPath: InstancePath,
  targetOriginalContextMetadata: ComponentMetadata[],
  currentContextMetadata: ComponentMetadata[],
  components: UtopiaJSXComponent[],
): SwitchLayoutTypeResult {
  const currentFrame = MetadataUtils.getFrame(target, targetOriginalContextMetadata)
  const newParent = findJSXElementAtPath(newParentPath, components, currentContextMetadata)
  const element = findJSXElementAtPath(target, components, currentContextMetadata)

  let propsToAdd: Array<ValueAtPath> = []

  if (currentFrame != null && newParent != null && element != null) {
    // When moving pinned to flex, use width and height to set basis values
    const possibleFlexProps = FlexLayoutHelpers.convertWidthHeightToFlex(
      currentFrame.width,
      currentFrame.height,
      element.props,
      right(newParent.props),
      null,
    )

    forEachRight(possibleFlexProps, (flexProps) => {
      const { flexBasis, crossBasis } = flexProps
      if (flexBasis != null) {
        propsToAdd.push({
          path: createLayoutPropertyPath('FlexFlexBasis'),
          value: jsxAttributeValue(flexBasis),
        })
      }
      if (crossBasis != null) {
        propsToAdd.push({
          path: createLayoutPropertyPath('FlexCrossBasis'),
          value: jsxAttributeValue(crossBasis),
        })
      }
    })
  }

  const updatedComponents = transformElementAtPath(
    components,
    target,
    (e: JSXElement) => {
      // Remove the pinning props first...
      const pinnedPropsRemoved = unsetJSXValuesAtPaths(
        e.props,
        AllFramePoints.map((p) => createLayoutPropertyPath(pinnedPropForFramePoint(p))),
      )
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
    },
    currentContextMetadata,
  )

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

interface SwitchLayoutTypeResult {
  updatedComponents: UtopiaJSXComponent[]
  updatedMetadata: ComponentMetadata[]
}

export function switchFlexChildToPinned(
  target: InstancePath,
  newParentPath: InstancePath,
  targetOriginalContextMetadata: ComponentMetadata[],
  currentContextMetadata: ComponentMetadata[],
  components: UtopiaJSXComponent[],
): SwitchLayoutTypeResult {
  const currentFrame = Utils.defaultIfNull(
    zeroLocalRect,
    MetadataUtils.getFrame(target, targetOriginalContextMetadata),
  ) // TODO How should this behave if there is no rendered frame?
  const element = MetadataUtils.getElementByInstancePathMaybe(targetOriginalContextMetadata, target)
  const newParent = MetadataUtils.getElementByInstancePathMaybe(
    currentContextMetadata,
    newParentPath,
  )

  // When moving flex to pinned, use fixed values or basis values to set width and height
  // FIXME Right now this isn't taking into account groups
  const unstretched = FlexLayoutHelpers.getUnstretchedWidthHeight(
    element?.props ?? {},
    newParent?.props ?? {},
  )
  const width = Utils.defaultIfNull(currentFrame.width, unstretched.width)
  const height = Utils.defaultIfNull(currentFrame.height, unstretched.height)
  const oldParentFrame =
    MetadataUtils.getFrameInCanvasCoords(TP.parentPath(target), targetOriginalContextMetadata) ||
    zeroCanvasRect
  const newParentFrame =
    MetadataUtils.getFrameInCanvasCoords(newParentPath, currentContextMetadata) || zeroCanvasRect
  const newOffset = Utils.pointDifference(newParentFrame, oldParentFrame)

  const updatedComponents = removeFlexAndAddPinnedPropsToComponent(
    target,
    components,
    currentContextMetadata,
    newOffset.y + currentFrame.y,
    newOffset.x + currentFrame.x,
    width,
    height,
  )

  const updatedMetadata = switchLayoutMetadata(
    currentContextMetadata,
    target,
    'nonfixed',
    undefined,
    'absolute',
  )

  return {
    updatedComponents: updatedComponents,
    updatedMetadata: updatedMetadata,
  }
}

export function switchFlexChildToGroup(
  target: InstancePath,
  newParentPath: InstancePath,
  targetOriginalContextMetadata: ComponentMetadata[],
  currentContextMetadata: ComponentMetadata[],
  components: UtopiaJSXComponent[],
): SwitchLayoutTypeResult {
  const currentFrame = Utils.defaultIfNull(
    zeroLocalRect,
    MetadataUtils.getFrame(target, targetOriginalContextMetadata),
  ) // TODO How should this behave if there is no rendered frame?
  const element = MetadataUtils.getElementByInstancePathMaybe(targetOriginalContextMetadata, target)
  const newParent = MetadataUtils.getElementByInstancePathMaybe(
    currentContextMetadata,
    newParentPath,
  )

  // When moving flex to pinned, use fixed values or basis values to set width and height
  const unstretched = FlexLayoutHelpers.getUnstretchedWidthHeight(
    element?.props ?? {},
    newParent?.props ?? {},
  )
  const width = Utils.defaultIfNull(currentFrame.width, unstretched.width)
  const height = Utils.defaultIfNull(currentFrame.height, unstretched.height)
  const oldParentFrame =
    MetadataUtils.getFrameInCanvasCoords(TP.parentPath(target), targetOriginalContextMetadata) ||
    zeroCanvasRect
  const newParentFrame =
    MetadataUtils.getFrameInCanvasCoords(newParentPath, currentContextMetadata) || zeroCanvasRect
  const newOffset = Utils.pointDifference(newParentFrame, oldParentFrame)

  const updatedComponents = removeFlexAndNonDefaultPinsAddPinnedPropsToComponent(
    target,
    components,
    currentContextMetadata,
    newOffset.y + currentFrame.y,
    newOffset.x + currentFrame.x,
    width,
    height,
  )

  const updatedMetadata = switchLayoutMetadata(
    currentContextMetadata,
    target,
    'nonfixed',
    undefined,
    'absolute',
  )

  return {
    updatedComponents: updatedComponents,
    updatedMetadata: updatedMetadata,
  }
}

export function switchChildToGroupWithParentFrame(
  target: InstancePath,
  originalPath: InstancePath,
  componentMetadata: ComponentMetadata[],
  components: UtopiaJSXComponent[],
  parentFrame: CanvasRectangle,
): SwitchLayoutTypeResult {
  const isParentFlexContainer = MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
    target,
    componentMetadata,
  )
  const currentFrame = Utils.defaultIfNull(
    zeroLocalRect,
    MetadataUtils.getFrame(originalPath, componentMetadata),
  ) // TODO How should this behave if there is no rendered frame?
  const element = MetadataUtils.getElementByInstancePathMaybe(componentMetadata, originalPath)
  const oldParentFrame = Utils.defaultIfNull(
    zeroCanvasRect,
    MetadataUtils.getFrameInCanvasCoords(TP.parentPath(originalPath), componentMetadata),
  )
  const newOffset = Utils.pointDifference(parentFrame, oldParentFrame)

  if (isParentFlexContainer) {
    // When moving flex to pinned, use fixed values or basis values to set width and height
    // TODO LAYOUT unstretched can be different from other switchTo cases, here we don't have access to props from metadata
    const unstretched = FlexLayoutHelpers.getUnstretchedWidthHeight(element?.props ?? {}, {})
    const width = Utils.defaultIfNull(currentFrame.width, unstretched.width)
    const height = Utils.defaultIfNull(currentFrame.height, unstretched.height)

    const updatedComponents = removeFlexAndNonDefaultPinsAddPinnedPropsToComponent(
      target,
      components,
      componentMetadata,
      newOffset.y + currentFrame.y,
      newOffset.x + currentFrame.x,
      width,
      height,
    )

    const updatedMetadata = switchLayoutMetadata(
      componentMetadata,
      target,
      'nonfixed',
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
      componentMetadata,
      newOffset.y + currentFrame.y,
      newOffset.x + currentFrame.x,
      width,
      height,
    )

    const updatedMetadata = switchLayoutMetadata(
      componentMetadata,
      target,
      'nonfixed',
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
  target: InstancePath,
  newParentPath: InstancePath,
  targetOriginalContextMetadata: ComponentMetadata[],
  currentContextMetadata: ComponentMetadata[],
  components: UtopiaJSXComponent[],
): SwitchLayoutTypeResult {
  const currentFrame = Utils.defaultIfNull(
    zeroLocalRect,
    MetadataUtils.getFrame(target, targetOriginalContextMetadata),
  ) // TODO How should this behave if there is no rendered frame?
  const oldParentFrame =
    MetadataUtils.getFrameInCanvasCoords(TP.parentPath(target), currentContextMetadata) ||
    zeroCanvasRect
  const newParentFrame =
    MetadataUtils.getFrameInCanvasCoords(newParentPath, currentContextMetadata) || zeroCanvasRect
  const newOffset = Utils.pointDifference(newParentFrame, oldParentFrame)
  const width = currentFrame.width
  const height = currentFrame.height

  const updatedComponents = changePinsToDefaultOnComponent(
    target,
    components,
    currentContextMetadata,
    newOffset.y + currentFrame.y,
    newOffset.x + currentFrame.x,
    width,
    height,
  )

  const updatedMetadata = switchLayoutMetadata(
    currentContextMetadata,
    target,
    'nonfixed',
    undefined,
    'absolute',
  )

  return {
    updatedComponents: updatedComponents,
    updatedMetadata: updatedMetadata,
  }
}

export function switchChildToPinnedWithParentFrame(
  target: InstancePath,
  originalPath: InstancePath,
  componentMetadata: ComponentMetadata[],
  components: UtopiaJSXComponent[],
  parentFrame: CanvasRectangle,
): SwitchLayoutTypeResult {
  const currentFrame = Utils.defaultIfNull(
    zeroLocalRect,
    MetadataUtils.getFrame(originalPath, componentMetadata),
  ) // TODO How should this behave if there is no rendered frame?
  const element = MetadataUtils.getElementByInstancePathMaybe(componentMetadata, originalPath)
  const oldParentFrame =
    MetadataUtils.getFrameInCanvasCoords(TP.parentPath(originalPath), componentMetadata) ||
    zeroCanvasRect
  const newOffset = Utils.pointDifference(parentFrame, oldParentFrame)

  // When moving flex to pinned, use fixed values or basis values to set width and height
  // TODO LAYOUT unstretched can be different from other switchTo cases, here we don't have access to props from metadata
  const unstretched = FlexLayoutHelpers.getUnstretchedWidthHeight(element?.props ?? {}, {})
  const width = Utils.defaultIfNull(currentFrame.width, unstretched.width)
  const height = Utils.defaultIfNull(currentFrame.height, unstretched.height)

  const updatedComponents = removeFlexAndAddPinnedPropsToComponent(
    target,
    components,
    componentMetadata,
    newOffset.y + currentFrame.y,
    newOffset.x + currentFrame.x,
    width,
    height,
  )

  const updatedMetadata = switchLayoutMetadata(
    componentMetadata,
    target,
    'nonfixed',
    undefined,
    'absolute',
  )

  return {
    updatedComponents: updatedComponents,
    updatedMetadata: updatedMetadata,
  }
}

function removeFlexAndNonDefaultPinsAddPinnedPropsToComponent(
  target: InstancePath,
  components: UtopiaJSXComponent[],
  componentMetadata: ComponentMetadata[],
  top: number,
  left: number,
  width: string | number,
  height: string | number,
) {
  const propsToAdd: Array<ValueAtPath> = [
    { path: createLayoutPropertyPath('PinnedLeft'), value: jsxAttributeValue(left) },
    { path: createLayoutPropertyPath('PinnedTop'), value: jsxAttributeValue(top) },
    { path: createLayoutPropertyPath('Width'), value: jsxAttributeValue(width) },
    { path: createLayoutPropertyPath('Height'), value: jsxAttributeValue(height) },
  ]

  const propsToRemove: Array<LayoutProp | StyleLayoutProp> = [
    'PinnedBottom',
    'PinnedRight',
    'PinnedCenterX',
    'PinnedCenterY',
    'FlexFlexBasis',
    'FlexCrossBasis',
  ]

  return transformElementAtPath(
    components,
    target,
    (e: JSXElement) => {
      const flexPropsRemoved = unsetJSXValuesAtPaths(
        e.props,
        propsToRemove.map((p) => createLayoutPropertyPath(p)),
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
    },
    componentMetadata,
  )
}

function removeFlexAndAddPinnedPropsToComponent(
  target: InstancePath,
  components: UtopiaJSXComponent[],
  componentMetadata: ComponentMetadata[],
  top: number,
  left: number,
  width: string | number,
  height: string | number,
) {
  const propsToAdd: Array<ValueAtPath> = [
    { path: createLayoutPropertyPath('PinnedLeft'), value: jsxAttributeValue(left) },
    { path: createLayoutPropertyPath('PinnedTop'), value: jsxAttributeValue(top) },
    { path: createLayoutPropertyPath('Width'), value: jsxAttributeValue(width) },
    { path: createLayoutPropertyPath('Height'), value: jsxAttributeValue(height) },
  ]
  const propsToRemove: Array<LayoutProp | StyleLayoutProp> = ['FlexFlexBasis', 'FlexCrossBasis']

  return transformElementAtPath(
    components,
    target,
    (e: JSXElement) => {
      const flexPropsRemoved = unsetJSXValuesAtPaths(
        e.props,
        propsToRemove.map((p) => createLayoutPropertyPath(p)),
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
    },
    componentMetadata,
  )
}

function changePinsToDefaultOnComponent(
  target: InstancePath,
  components: UtopiaJSXComponent[],
  componentMetadata: ComponentMetadata[],
  top: number,
  left: number,
  width: string | number,
  height: string | number,
) {
  const propsToAdd: Array<ValueAtPath> = [
    { path: createLayoutPropertyPath('PinnedLeft'), value: jsxAttributeValue(left) },
    { path: createLayoutPropertyPath('PinnedTop'), value: jsxAttributeValue(top) },
    { path: createLayoutPropertyPath('Width'), value: jsxAttributeValue(width) },
    { path: createLayoutPropertyPath('Height'), value: jsxAttributeValue(height) },
  ]
  const propsToRemove: Array<LayoutProp | StyleLayoutProp> = [
    'PinnedBottom',
    'PinnedRight',
    'PinnedCenterX',
    'PinnedCenterY',
  ]
  return transformElementAtPath(
    components,
    target,
    (e: JSXElement) => {
      const pinPropsRemoved = unsetJSXValuesAtPaths(
        e.props,
        propsToRemove.map((p) => createLayoutPropertyPath(p)),
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
    },
    componentMetadata,
  )
}

const propertiesToRound: Array<PropertyPath> = [
  createLayoutPropertyPath('PinnedCenterX'),
  createLayoutPropertyPath('PinnedCenterY'),
  createLayoutPropertyPath('FlexFlexBasis'),
  createLayoutPropertyPath('FlexCrossBasis'),
  createLayoutPropertyPath('PinnedLeft'),
  createLayoutPropertyPath('PinnedTop'),
  createLayoutPropertyPath('Width'),
  createLayoutPropertyPath('Height'),
  createLayoutPropertyPath('PinnedRight'),
  createLayoutPropertyPath('PinnedBottom'),
]

export function roundAttributeLayoutValues(jsxAttributes: JSXAttributes): JSXAttributes {
  return propertiesToRound.reduce((workingAttributes, propertyToRound) => {
    // Lookup the attribute given the property path.
    const attributeResult = getJSXAttributeAtPath(workingAttributes, propertyToRound)
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
                jsxAttributeValue(rounded),
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
      case 'ATTRIBUTE_OTHER_JAVASCRIPT':
        return workingAttributes
      default:
        const _exhaustiveCheck: never = value
        throw new Error(`Unhandled type ${JSON.stringify(value)}`)
    }
  }, jsxAttributes)
}

export function roundJSXElementLayoutValues(element: JSXElement): JSXElement {
  return {
    ...element,
    props: roundAttributeLayoutValues(element.props),
  }
}
