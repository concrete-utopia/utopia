import createCachedSelector from 're-reselect'
import {
  HorizontalLayoutPinnedProps,
  VerticalLayoutPinnedProps,
  isHorizontalLayoutPinnedProp,
  isVerticalLayoutPinnedProp,
  type LayoutPinnedProp,
} from '../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { emptyComments, jsExpressionValue } from '../../core/shared/element-template'
import { localRectangle, nullIfInfinity } from '../../core/shared/math-utils'
import type { ElementPath } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import { assertNever } from '../../core/shared/utils'
import invariant from '../../third-party/remix/invariant'
import type { SelectOption } from '../../uuiui-deps'
import { valueToUseForPin } from '../canvas/canvas-utils'
import type { SetProp, UnsetProperty } from '../editor/action-types'
import { setProp_UNSAFE, unsetProperty } from '../editor/actions/action-creators'
import type { AllElementProps } from '../editor/store/editor-state'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { getFullFrame } from '../frame'
import { isCssNumberAndFixedSize, isCssNumberAndPercentage } from './common/css-utils'
import type { FramePinsInfo } from './common/layout-property-path-hooks'
import {
  allElementPropsSelector,
  metadataSelector,
  selectedViewsSelector,
} from './inpector-selectors'
import {
  getConstraintsIncludingImplicitForElement,
  getFramePointsFromMetadataTypeFixed,
} from './inspector-common'
import type { ElementPathTrees } from '../../core/shared/element-path-tree'
import { getLocalRectangleWithFixedWidthHandlingText } from '../text-editor/text-handling'

type HorizontalPinRequests =
  | 'left-and-width'
  | 'right-and-width'
  | 'left-and-right'
  | 'left'
  | 'right'
  | 'width'
  | 'scale-horizontal'

type VerticalPinRequests =
  | 'top-and-height'
  | 'bottom-and-height'
  | 'top-and-bottom'
  | 'top'
  | 'bottom'
  | 'height'
  | 'scale-vertical'

export type RequestedPins = HorizontalPinRequests | VerticalPinRequests

export type DetectedPins = {
  horizontal: HorizontalPinRequests | 'group-child-error-percentage' | 'mixed'
  vertical: VerticalPinRequests | 'group-child-error-percentage' | 'mixed'
}

export const FrameChildHorizontalPinChangeOptions: {
  [key in Exclude<HorizontalPinRequests, 'left' | 'right' | 'width'>]: SelectOption & {
    value: HorizontalPinRequests
  }
} = {
  'left-and-width': {
    value: 'left-and-width',
    label: 'Left',
  },
  'right-and-width': {
    value: 'right-and-width',
    label: 'Right',
  },
  'left-and-right': {
    value: 'left-and-right',
    label: 'Left and Right',
  },
  'scale-horizontal': {
    value: 'scale-horizontal',
    label: 'Scale',
  },
} as const

export const GroupChildHorizontalPinChangeOptions: {
  [key in HorizontalPinRequests]: SelectOption & { value: HorizontalPinRequests }
} = {
  left: {
    value: 'left',
    label: 'Left',
  },
  right: {
    value: 'right',
    label: 'Right',
  },
  width: {
    value: 'width',
    label: 'Width',
  },
  'left-and-width': {
    value: 'left-and-width',
    label: 'Left and Width',
  },
  'right-and-width': {
    value: 'right-and-width',
    label: 'Right and Width',
  },
  'left-and-right': {
    value: 'left-and-right',
    label: 'Left and Right',
  },
  'scale-horizontal': {
    value: 'scale-horizontal',
    label: 'Scale',
  },
} as const

export const DetectedFrameChildHorizontalPinChangeOptions: {
  [key in DetectedPins['horizontal']]: SelectOption & {
    value: DetectedPins['horizontal']
  }
} = {
  ...FrameChildHorizontalPinChangeOptions,
  left: {
    value: 'left',
    label: 'Left',
  },
  right: {
    value: 'right',
    label: 'Right',
  },
  width: {
    value: 'width',
    label: 'Width',
  },
  'group-child-error-percentage': {
    value: 'group-child-error-percentage',
    label: 'Invalid',
    invalid: true,
  },
  mixed: {
    value: 'mixed',
    label: 'Mixed',
  },
} as const

export const DetectedGroupChildHorizontalPinChangeOptions: {
  [key in DetectedPins['horizontal']]: SelectOption & {
    value: DetectedPins['horizontal']
  }
} = {
  ...GroupChildHorizontalPinChangeOptions,
  'group-child-error-percentage': {
    value: 'group-child-error-percentage',
    label: 'Invalid',
    invalid: true,
  },
  mixed: {
    value: 'mixed',
    label: 'Mixed',
  },
} as const

export const FrameChildVerticalPinChangeOptions: {
  [key in Exclude<VerticalPinRequests, 'top' | 'bottom' | 'height'>]: SelectOption & {
    value: VerticalPinRequests
  }
} = {
  'top-and-height': {
    value: 'top-and-height',
    label: 'Top',
  },
  'bottom-and-height': {
    value: 'bottom-and-height',
    label: 'Bottom',
  },
  'top-and-bottom': {
    value: 'top-and-bottom',
    label: 'Top and Bottom',
  },
  'scale-vertical': {
    value: 'scale-vertical',
    label: 'Scale',
  },
} as const

export const GroupChildVerticalPinChangeOptions: {
  [key in VerticalPinRequests]: SelectOption & { value: VerticalPinRequests }
} = {
  top: {
    value: 'top',
    label: 'Top',
  },
  bottom: {
    value: 'bottom',
    label: 'Bottom',
  },
  height: {
    value: 'height',
    label: 'Height',
  },
  'top-and-height': {
    value: 'top-and-height',
    label: 'Top and Height',
  },
  'bottom-and-height': {
    value: 'bottom-and-height',
    label: 'Bottom and Height',
  },
  'top-and-bottom': {
    value: 'top-and-bottom',
    label: 'Top and Bottom',
  },
  'scale-vertical': {
    value: 'scale-vertical',
    label: 'Scale',
  },
} as const

export const DetectedFrameChildVerticalPinChangeOptions: {
  [key in DetectedPins['vertical']]: SelectOption & {
    value: DetectedPins['vertical']
  }
} = {
  ...FrameChildVerticalPinChangeOptions,
  top: {
    value: 'top',
    label: 'Top',
  },
  bottom: {
    value: 'bottom',
    label: 'Bottom',
  },
  height: {
    value: 'height',
    label: 'Height',
  },
  'group-child-error-percentage': {
    value: 'group-child-error-percentage',
    label: 'Invalid',
    invalid: true,
  },
  mixed: {
    value: 'mixed',
    label: 'Mixed',
  },
} as const

export const DetectedGroupChildVerticalPinChangeOptions: {
  [key in DetectedPins['vertical']]: SelectOption & {
    value: DetectedPins['vertical']
  }
} = {
  ...GroupChildVerticalPinChangeOptions,
  'group-child-error-percentage': {
    value: 'group-child-error-percentage',
    label: 'Invalid',
    invalid: true,
  },
  mixed: {
    value: 'mixed',
    label: 'Mixed',
  },
} as const

export function useDetectedConstraints(isGroupChild: 'group-child' | 'frame-child') {
  return useEditorState(
    Substores.metadata,
    (store) => multiselectDetectConstraintsForChildSelector(store, isGroupChild),
    'FrameChildConstraintSelect pins',
  )
}

const multiselectDetectConstraintsForChildSelector = createCachedSelector(
  metadataSelector,
  allElementPropsSelector,
  selectedViewsSelector,
  (_: unknown, isGroupChild: 'group-child' | 'frame-child') => isGroupChild,
  (metadata, allElementProps, selectedViews, isGroupChild) =>
    multiselectDetectConstraintsForChild(metadata, allElementProps, selectedViews, isGroupChild),
)((_, isGroupChild: 'group-child' | 'frame-child') => isGroupChild)

function multiselectDetectConstraintsForChild(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  targets: Array<ElementPath>,
  isGroupChild: 'group-child' | 'frame-child',
): DetectedPins {
  if (targets.length == 0) {
    return { horizontal: 'left-and-width', vertical: 'top-and-height' }
  }
  const results = targets.map((t) =>
    isGroupChild === 'group-child'
      ? detectConstraintsSetForGroupChild(metadata, allElementProps, t)
      : detectPinsSetForFrameChild(metadata, t),
  )
  const isMixedHorizontal = results.some((r) => r.horizontal !== results[0].horizontal)
  const isMixedVertical = results.some((r) => r.vertical !== results[0].vertical)

  return {
    horizontal: isMixedHorizontal ? 'mixed' : results[0].horizontal,
    vertical: isMixedVertical ? 'mixed' : results[0].vertical,
  }
}

function detectPinsSetForFrameChild(
  metadata: ElementInstanceMetadataMap,
  target: ElementPath,
): { horizontal: HorizontalPinRequests | 'mixed'; vertical: VerticalPinRequests | 'mixed' } {
  const element = MetadataUtils.findElementByElementPath(metadata, target)
  if (element == null) {
    return { horizontal: 'mixed', vertical: 'mixed' }
  }

  const framePoints = getFramePointsFromMetadataTypeFixed(element)

  const horizontalPins: HorizontalPinRequests | 'mixed' = (() => {
    if (
      isCssNumberAndFixedSize(framePoints.left) &&
      framePoints.right == null &&
      !isCssNumberAndPercentage(framePoints.width)
    ) {
      return 'left-and-width'
    }
    if (
      framePoints.left == null &&
      isCssNumberAndFixedSize(framePoints.right) &&
      !isCssNumberAndPercentage(framePoints.width)
    ) {
      return 'right-and-width'
    }
    if (
      isCssNumberAndFixedSize(framePoints.left) &&
      isCssNumberAndFixedSize(framePoints.right) &&
      framePoints.width == null
    ) {
      return 'left-and-right'
    }
    if (
      framePoints.left == null &&
      framePoints.right == null &&
      isCssNumberAndFixedSize(framePoints.width)
    ) {
      return 'left-and-width' // this is technically true, although maybe we should be more honest and call this Width
    }
    if (
      isCssNumberAndPercentage(framePoints.left) &&
      framePoints.right == null &&
      isCssNumberAndPercentage(framePoints.width)
    ) {
      return 'scale-horizontal'
    }
    return 'mixed'
  })()

  const verticalPins: VerticalPinRequests | 'mixed' = (() => {
    if (
      isCssNumberAndFixedSize(framePoints.top) &&
      framePoints.bottom == null &&
      !isCssNumberAndPercentage(framePoints.height)
    ) {
      return 'top-and-height'
    }
    if (
      framePoints.top == null &&
      isCssNumberAndFixedSize(framePoints.bottom) &&
      !isCssNumberAndPercentage(framePoints.height)
    ) {
      return 'bottom-and-height'
    }
    if (
      isCssNumberAndFixedSize(framePoints.top) &&
      isCssNumberAndFixedSize(framePoints.bottom) &&
      framePoints.height == null
    ) {
      return 'top-and-bottom'
    }
    if (
      framePoints.top == null &&
      framePoints.bottom == null &&
      isCssNumberAndFixedSize(framePoints.height)
    ) {
      return 'top-and-height' // this is technically true, although maybe we should be more honest and call this Height
    }
    if (
      isCssNumberAndPercentage(framePoints.top) &&
      framePoints.bottom == null &&
      isCssNumberAndPercentage(framePoints.height)
    ) {
      return 'scale-vertical'
    }
    return 'mixed'
  })()

  return { horizontal: horizontalPins, vertical: verticalPins }
}

function detectConstraintsSetForGroupChild(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  target: ElementPath,
): DetectedPins {
  const constraints = getConstraintsIncludingImplicitForElement(
    metadata,
    allElementProps,
    target,
    'include-max-content', // TODO make sure to keep this in sync with getUpdateResizedGroupChildrenCommands
  )

  const element = MetadataUtils.findElementByElementPath(metadata, target)
  if (element == null) {
    return { horizontal: 'mixed', vertical: 'mixed' }
  }

  const framePoints = getFramePointsFromMetadataTypeFixed(element)

  const horizontalPins: DetectedPins['horizontal'] = (() => {
    const horizontalConstraints = HorizontalLayoutPinnedProps.filter((pin) =>
      constraints.includes(pin),
    )

    const anyPinPercentage = HorizontalLayoutPinnedProps.some((pin) =>
      isCssNumberAndPercentage(framePoints[pin]),
    )
    if (anyPinPercentage) {
      return 'group-child-error-percentage'
    }

    if (horizontalConstraints.length === 0) {
      return 'scale-horizontal'
    }

    if (constraints.includes('left') && constraints.includes('width')) {
      return 'left-and-width'
    }

    if (constraints.includes('right') && constraints.includes('width')) {
      return 'right-and-width'
    }
    if (constraints.includes('left') && constraints.includes('right')) {
      return 'left-and-right'
    }

    if (constraints.includes('left')) {
      return 'left'
    }

    if (constraints.includes('right')) {
      return 'right'
    }

    if (constraints.includes('width')) {
      return 'width'
    }

    return 'mixed'
  })()

  const verticalPins: DetectedPins['vertical'] = (() => {
    const verticalConstraints = VerticalLayoutPinnedProps.filter((pin) => constraints.includes(pin))

    const anyPinPercentage = VerticalLayoutPinnedProps.some((pin) =>
      isCssNumberAndPercentage(framePoints[pin]),
    )
    if (anyPinPercentage) {
      return 'group-child-error-percentage'
    }

    if (verticalConstraints.length === 0) {
      return 'scale-vertical'
    }

    if (constraints.includes('top') && constraints.includes('height')) {
      return 'top-and-height'
    }
    if (constraints.includes('bottom') && constraints.includes('height')) {
      return 'bottom-and-height'
    }
    if (constraints.includes('top') && constraints.includes('bottom')) {
      return 'top-and-bottom'
    }

    if (constraints.includes('top')) {
      return 'top'
    }
    if (constraints.includes('bottom')) {
      return 'bottom'
    }
    if (constraints.includes('height')) {
      return 'height'
    }

    return 'mixed'
  })()

  return { horizontal: horizontalPins, vertical: verticalPins }
}

export function getFixedPointsForPinning(pins: DetectedPins): FramePinsInfo {
  const ignore = { isPrimaryPosition: false, isRelativePosition: false }

  return {
    left: {
      isPrimaryPosition:
        pins.horizontal === 'left-and-right' ||
        pins.horizontal === 'left-and-width' ||
        pins.horizontal === 'left',
      isRelativePosition: false,
    },
    top: {
      isPrimaryPosition:
        pins.vertical === 'top-and-bottom' ||
        pins.vertical === 'top-and-height' ||
        pins.vertical === 'top',
      isRelativePosition: false,
    },
    bottom: {
      isPrimaryPosition:
        pins.vertical === 'top-and-bottom' ||
        pins.vertical === 'bottom-and-height' ||
        pins.vertical === 'bottom',
      isRelativePosition: false,
    },
    right: {
      isPrimaryPosition:
        pins.horizontal === 'left-and-right' ||
        pins.horizontal === 'right-and-width' ||
        pins.horizontal === 'right',
      isRelativePosition: false,
    },
    width: {
      isPrimaryPosition:
        pins.horizontal === 'left-and-width' ||
        pins.horizontal === 'right-and-width' ||
        pins.horizontal === 'width' ||
        pins.horizontal === 'scale-horizontal',
      isRelativePosition: pins.horizontal === 'scale-horizontal',
    },
    height: {
      isPrimaryPosition:
        pins.vertical === 'top-and-height' ||
        pins.vertical === 'bottom-and-height' ||
        pins.vertical === 'height' ||
        pins.vertical === 'scale-vertical',
      isRelativePosition: pins.vertical === 'scale-vertical',
    },
    centerX:
      pins.horizontal === 'scale-horizontal'
        ? { isPrimaryPosition: true, isRelativePosition: true }
        : ignore,
    centerY:
      pins.vertical === 'scale-vertical'
        ? { isPrimaryPosition: true, isRelativePosition: true }
        : ignore,
  }
}

export function getFrameChangeActionsForFrameChild(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  propertyTarget: ReadonlyArray<string>,
  targets: Array<ElementPath>,
  requestedPins: RequestedPins,
): Array<SetProp | UnsetProperty> {
  const pinChange = getPinChanges(metadata, pathTrees, propertyTarget, targets)
  switch (requestedPins) {
    case 'left-and-width':
    case 'left':
    case 'width':
      return pinChange(['left', 'width'], 'horizontal', 'px')
    case 'right-and-width':
    case 'right':
      return pinChange(['right', 'width'], 'horizontal', 'px')
    case 'left-and-right':
      return pinChange(['left', 'right'], 'horizontal', 'px')
    case 'scale-horizontal':
      return pinChange(['left', 'width'], 'horizontal', '%')

    case 'top-and-height':
    case 'top':
    case 'height':
      return pinChange(['top', 'height'], 'vertical', 'px')
    case 'bottom-and-height':
    case 'bottom':
      return pinChange(['bottom', 'height'], 'vertical', 'px')
    case 'top-and-bottom':
      return pinChange(['top', 'bottom'], 'vertical', 'px')
    case 'scale-vertical':
      return pinChange(['top', 'height'], 'vertical', '%')

    default:
      assertNever(requestedPins)
  }
}

const getPinChanges =
  (
    metadata: ElementInstanceMetadataMap,
    pathTrees: ElementPathTrees,
    propertyTarget: ReadonlyArray<string>,
    targets: Array<ElementPath>,
  ) =>
  (
    pinsToSet: Array<LayoutPinnedProp>,
    horizontal: 'horizontal' | 'vertical',
    setAsPercentage: 'px' | '%',
  ): Array<SetProp | UnsetProperty> => {
    const pinsToUnset =
      horizontal === 'horizontal' ? HorizontalLayoutPinnedProps : VerticalLayoutPinnedProps

    const unsetActions: Array<UnsetProperty> = targets.flatMap((target) =>
      pinsToUnset.map((pin) => unsetProperty(target, PP.create(...propertyTarget, pin))),
    )

    const setActions: Array<SetProp> = targets.flatMap((target) => {
      const localFrame = nullIfInfinity(
        getLocalRectangleWithFixedWidthHandlingText(metadata, pathTrees, target),
      )

      const coordinateSystemBounds = MetadataUtils.findElementByElementPath(metadata, target)
        ?.specialSizeMeasurements.coordinateSystemBounds

      invariant(localFrame != null, 'LocalFrame should never be null')
      invariant(coordinateSystemBounds != null, 'coordinateSystemBounds should never be null')

      const fullFrame = getFullFrame(localFrame)

      return pinsToSet.map((pin) =>
        setProp_UNSAFE(
          target,
          PP.create(...propertyTarget, pin),
          jsExpressionValue(
            valueToUseForPin(pin, fullFrame[pin], setAsPercentage === '%', coordinateSystemBounds),
            emptyComments,
          ),
        ),
      )
    })

    return [...unsetActions, ...setActions]
  }

export function getConstraintAndFrameChangeActionsForGroupChild(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  propertyTarget: ReadonlyArray<string>,
  targets: Array<ElementPath>,
  requestedPins: RequestedPins,
): Array<SetProp | UnsetProperty> {
  const pinChange = getPinChanges(metadata, pathTrees, propertyTarget, targets)
  const setConstraintsForDimension = (
    constraintsToSet: Array<LayoutPinnedProp>,
    dimension: 'horizontal' | 'vertical',
  ): Array<SetProp | UnsetProperty> => {
    return targets.map((target) => {
      const currentConstraints = getConstraintsIncludingImplicitForElement(
        metadata,
        allElementProps,
        target,
        'only-explicit-constraints',
      )
      const constraintsToKeepForOtherDimension = currentConstraints.filter(
        dimension === 'horizontal' ? isVerticalLayoutPinnedProp : isHorizontalLayoutPinnedProp,
      )
      const desiredConstraints = [...constraintsToKeepForOtherDimension, ...constraintsToSet]
      return desiredConstraints.length === 0
        ? unsetProperty(target, PP.create('data-constraints'))
        : setProp_UNSAFE(
            target,
            PP.create('data-constraints'),
            jsExpressionValue(desiredConstraints, emptyComments),
          )
    })
  }
  switch (requestedPins) {
    case 'left-and-width':
      return [
        ...setConstraintsForDimension(['left', 'width'], 'horizontal'),
        ...pinChange(['left', 'width'], 'horizontal', 'px'),
      ]
    case 'right-and-width':
      return [
        ...setConstraintsForDimension(['right', 'width'], 'horizontal'),
        ...pinChange(['right', 'width'], 'horizontal', 'px'),
      ]
    case 'left-and-right':
      return [
        ...setConstraintsForDimension(['left', 'right'], 'horizontal'),
        ...pinChange(['left', 'right'], 'horizontal', 'px'),
      ]
    case 'left':
      return [
        ...setConstraintsForDimension(['left'], 'horizontal'),
        ...pinChange(['left', 'width'], 'horizontal', 'px'),
      ]
    case 'right':
      return [
        ...setConstraintsForDimension(['right'], 'horizontal'),
        ...pinChange(['right', 'width'], 'horizontal', 'px'),
      ]
    case 'width':
      return [
        ...setConstraintsForDimension(['width'], 'horizontal'),
        ...pinChange(['left', 'width'], 'horizontal', 'px'),
      ]
    case 'scale-horizontal':
      return [
        ...setConstraintsForDimension([], 'horizontal'), // clearing constraints for dimension
        ...pinChange(['left', 'width'], 'horizontal', 'px'), // for Scale, we set the actual frame points to left,width,px and let the Group Resize True-up change these values during edits
      ]

    case 'top-and-height':
      return [
        ...setConstraintsForDimension(['top', 'height'], 'vertical'),
        ...pinChange(['top', 'height'], 'vertical', 'px'),
      ]
    case 'bottom-and-height':
      return [
        ...setConstraintsForDimension(['bottom', 'height'], 'vertical'),
        ...pinChange(['bottom', 'height'], 'vertical', 'px'),
      ]
    case 'top-and-bottom':
      return [
        ...setConstraintsForDimension(['top', 'bottom'], 'vertical'),
        ...pinChange(['top', 'bottom'], 'vertical', 'px'),
      ]
    case 'top':
      return [
        ...setConstraintsForDimension(['top'], 'vertical'),
        ...pinChange(['top', 'height'], 'vertical', 'px'),
      ]
    case 'bottom':
      return [
        ...setConstraintsForDimension(['bottom'], 'vertical'),
        ...pinChange(['bottom', 'height'], 'vertical', 'px'),
      ]
    case 'height':
      return [
        ...setConstraintsForDimension(['height'], 'vertical'),
        ...pinChange(['top', 'height'], 'vertical', 'px'),
      ]
    case 'scale-vertical':
      return [
        ...setConstraintsForDimension([], 'vertical'), // clearing constraints for dimension
        ...pinChange(['top', 'height'], 'vertical', 'px'), // for Scale, we set the actual frame points to top,height,px and let the Group Resize True-up change these values during edits
      ]

    default:
      assertNever(requestedPins)
  }
}
