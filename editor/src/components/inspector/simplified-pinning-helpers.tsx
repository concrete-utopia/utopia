import { createSelector } from 'reselect'
import {
  HorizontalLayoutPinnedProps,
  VerticalLayoutPinnedProps,
  type LayoutPinnedProp,
} from '../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { emptyComments, jsExpressionValue } from '../../core/shared/element-template'
import { nullIfInfinity } from '../../core/shared/math-utils'
import type { ElementPath } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import { assertNever } from '../../core/shared/utils'
import invariant from '../../third-party/remix/invariant'
import type { SelectOption } from '../../uuiui-deps'
import { valueToUseForPin } from '../canvas/canvas-utils'
import type { SetProp, UnsetProperty } from '../editor/action-types'
import { setProp_UNSAFE, unsetProperty } from '../editor/actions/action-creators'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { getFullFrame } from '../frame'
import { isCssNumberAndFixedSize, isCssNumberAndPercentage } from './common/css-utils'
import type { FramePinsInfo } from './common/layout-property-path-hooks'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import { getFramePointsFromMetadataTypeFixed } from './inspector-common'

type HorizontalPinRequests =
  | 'left-and-width'
  | 'right-and-width'
  | 'left-and-right'
  | 'scale-horizontal'

type VerticalPinRequests =
  | 'top-and-height'
  | 'bottom-and-height'
  | 'top-and-bottom'
  | 'scale-vertical'

export type RequestedPins = HorizontalPinRequests | VerticalPinRequests

type DetectedPins = {
  horizontal: HorizontalPinRequests | 'mixed'
  vertical: VerticalPinRequests | 'mixed'
}

export const HorizontalPinChangeOptions: {
  [key in HorizontalPinRequests]: SelectOption & { value: HorizontalPinRequests }
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

export const HorizontalPinChangeOptionsIncludingMixed = {
  ...HorizontalPinChangeOptions,
  mixed: {
    value: 'mixed',
    label: 'Mixed',
  },
} as const

export const VerticalPinChangeOptions: {
  [key in VerticalPinRequests]: SelectOption & { value: VerticalPinRequests }
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

export const VerticalPinChangeOptionsIncludingMixed = {
  ...VerticalPinChangeOptions,
  mixed: {
    value: 'mixed',
    label: 'Mixed',
  },
} as const

const multiselectDetectPinsSetSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  (metadata, selectedViews) => multiselectDetectPinsSet(metadata, selectedViews),
)

export function useDetectedPinning() {
  return useEditorState(
    Substores.metadata,
    multiselectDetectPinsSetSelector,
    'FrameChildConstraintSelect pins',
  )
}

function multiselectDetectPinsSet(
  metadata: ElementInstanceMetadataMap,
  targets: Array<ElementPath>,
): DetectedPins {
  if (targets.length == 0) {
    return { horizontal: 'left-and-width', vertical: 'top-and-height' }
  }
  const results = targets.map((t) => detectPinsSet(metadata, t))
  const isMixedHorizontal = results.some((r) => r.horizontal !== results[0].horizontal)
  const isMixedVertical = results.some((r) => r.vertical !== results[0].vertical)

  return {
    horizontal: isMixedHorizontal ? 'mixed' : results[0].horizontal,
    vertical: isMixedVertical ? 'mixed' : results[0].vertical,
  }
}

function detectPinsSet(
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
      isCssNumberAndFixedSize(framePoints.width)
    ) {
      return 'left-and-width'
    }
    if (
      framePoints.left == null &&
      isCssNumberAndFixedSize(framePoints.right) &&
      isCssNumberAndFixedSize(framePoints.width)
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
      isCssNumberAndFixedSize(framePoints.height)
    ) {
      return 'top-and-height'
    }
    if (
      framePoints.top == null &&
      isCssNumberAndFixedSize(framePoints.bottom) &&
      isCssNumberAndFixedSize(framePoints.height)
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

export function getFixedPointsForPinning(pins: DetectedPins): FramePinsInfo {
  const ignore = { isPrimaryPosition: false, isRelativePosition: false }

  return {
    left: {
      isPrimaryPosition:
        pins.horizontal === 'left-and-right' || pins.horizontal === 'left-and-width',
      isRelativePosition: false,
    },
    top: {
      isPrimaryPosition: pins.vertical === 'top-and-bottom' || pins.vertical === 'top-and-height',
      isRelativePosition: false,
    },
    bottom: {
      isPrimaryPosition:
        pins.vertical === 'top-and-bottom' || pins.vertical === 'bottom-and-height',
      isRelativePosition: false,
    },
    right: {
      isPrimaryPosition:
        pins.horizontal === 'left-and-right' || pins.horizontal === 'right-and-width',
      isRelativePosition: false,
    },
    width: ignore,
    height: ignore,
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

export function getFrameChangeActions(
  metadata: ElementInstanceMetadataMap,
  propertyTarget: ReadonlyArray<string>,
  targets: Array<ElementPath>,
  requestedPins: RequestedPins,
): Array<SetProp | UnsetProperty> {
  const pinChange = getPinChanges(metadata, propertyTarget, targets)
  switch (requestedPins) {
    case 'left-and-width':
      return pinChange(['left', 'width'], 'horizontal', 'px')
    case 'right-and-width':
      return pinChange(['right', 'width'], 'horizontal', 'px')
    case 'left-and-right':
      return pinChange(['left', 'right'], 'horizontal', 'px')
    case 'scale-horizontal':
      return pinChange(['left', 'width'], 'horizontal', '%')

    case 'top-and-height':
      return pinChange(['top', 'height'], 'vertical', 'px')
    case 'bottom-and-height':
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
        MetadataUtils.findElementByElementPath(metadata, target)?.localFrame,
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
