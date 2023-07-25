import fastDeepEqual from 'fast-deep-equal'
import type { Frame, FramePin, FramePoint } from 'utopia-api/core'
import {
  HorizontalFramePoints,
  isHorizontalPoint,
  isPercentPin,
  valueToUseForPin,
  VerticalFramePoints,
} from 'utopia-api/core'
import type { LayoutPinnedProp } from '../../../core/layout/layout-helpers-new'
import {
  framePointForPinnedProp,
  HorizontalLayoutPinnedProps,
  LayoutPinnedProps,
  VerticalLayoutPinnedProps,
} from '../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'
import type { LocalRectangle } from '../../../core/shared/math-utils'
import { isInfinityRectangle, zeroLocalRect } from '../../../core/shared/math-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import * as EP from '../../../core/shared/element-path'
import Utils from '../../../utils/utils'
import { resetPins, setProp_UNSAFE, unsetProperty } from '../../editor/actions/action-creators'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import type { FullFrame } from '../../frame'
import { getFullFrame } from '../../frame'
import type { InspectorInfo } from './property-path-hooks'
import {
  useInspectorLayoutInfo,
  useRefSelectedViews,
  InspectorPropsContext,
  stylePropPathMappingFn,
} from './property-path-hooks'

import React from 'react'
import type { CSSNumber } from './css-utils'
import { cssNumberToString } from './css-utils'
import { useContextSelector } from 'use-context-selector'
import { useDispatch } from '../../editor/store/dispatch-context'
import { getFramePointsFromMetadata, MaxContent } from '../inspector-common'
import { mapDropNulls } from '../../../core/shared/array-utils'
import {
  groupErrorToastAction,
  maybeGroupWithoutFixedSizeForFill,
} from '../../canvas/canvas-strategies/strategies/group-helpers'
import { maybeInvalidGroupStates } from '../inspector-strategies/inspector-strategies'

const HorizontalPropPreference: Array<LayoutPinnedProp> = ['left', 'width', 'right']
const HorizontalPropPreferenceHug: Array<LayoutPinnedProp> = ['width', 'left', 'right']
const VerticalPropPreference: Array<LayoutPinnedProp> = ['top', 'height', 'bottom']
const VerticalPropPreferenceHug: Array<LayoutPinnedProp> = ['height', 'top', 'bottom']

function allPinsMatch(point: FramePoint, framesToCheck: readonly Frame[]): boolean {
  const firstFrame = framesToCheck[0]
  if (firstFrame == null) {
    return true
  } else {
    const firstPin = firstFrame[point]
    const pinIsPercent = firstPin != null && isPercentPin(firstPin)
    return (
      firstPin != null &&
      framesToCheck.every((frame) => {
        const frameValue = frame[point]
        return frameValue != null && isPercentPin(frameValue) === pinIsPercent
      })
    )
  }
}

interface PinToSet {
  path: ElementPath
  pin: LayoutPinnedProp
  value: FramePin
}

interface PinToUnset {
  path: ElementPath
  pin: LayoutPinnedProp
}

interface ChangePinResult {
  pinsToSet: ReadonlyArray<PinToSet>
  pinsToUnset: ReadonlyArray<PinToUnset>
  shouldSetHorizontalPin: boolean
}

export interface ElementFrameInfo {
  path: ElementPath
  frame: Frame
  localFrame: LocalRectangle | null
  parentFrame: LocalRectangle | null
}

type PinInspectorInfo = InspectorInfo<CSSNumber | undefined>

export type PinsInfo = { [key in LayoutPinnedProp]: PinInspectorInfo }

function getLastSetPinOrNull(
  lastSetPin: LayoutPinnedProp | null,
  pinsInfo: PinsInfo,
): LayoutPinnedProp | null {
  if (lastSetPin == null) {
    return null
  } else {
    const lastSetPinIsValid = pinsInfo[lastSetPin].value != null
    return lastSetPinIsValid ? lastSetPin : null
  }
}

export function changePin(
  newFrameProp: LayoutPinnedProp,
  pinsInfo: PinsInfo,
  frameInfoForElements: ReadonlyArray<ElementFrameInfo>,
  lastHorizontalProp: LayoutPinnedProp | null,
  lastVerticalProp: LayoutPinnedProp | null,
): ChangePinResult {
  const lastSetHorizontalProp: LayoutPinnedProp | null = getLastSetPinOrNull(
    lastHorizontalProp,
    pinsInfo,
  )
  const lastSetVerticalProp: LayoutPinnedProp | null = getLastSetPinOrNull(
    lastVerticalProp,
    pinsInfo,
  )

  const newFramePoint = framePointForPinnedProp(newFrameProp)
  const pinInfoForProp = pinsInfo[newFrameProp]
  const toggleToRelative =
    pinInfoForProp.propertyStatus.identical &&
    pinInfoForProp.value != null &&
    pinInfoForProp.value.unit != 'px' &&
    !isPercentPin(cssNumberToString(pinInfoForProp.value, true))

  let pinsToSet: Array<PinToSet> = []
  let pinsToUnset: Array<PinToUnset> = []
  const isHorizontalPin = isHorizontalPoint(newFramePoint)

  Utils.fastForEach(frameInfoForElements, (frameInfo) => {
    const { path, frame, localFrame, parentFrame } = frameInfo
    if (localFrame == null) {
      // Can't set pins on non-layoutable elements
      return
    }
    if (parentFrame == null) {
      // Can't set pins on root level elements
      return
    }
    const fullFrame = getFullFrame(localFrame)

    const pointsToAdd = getPinsToAdd(
      newFrameProp,
      path,
      frame,
      fullFrame,
      parentFrame,
      lastSetHorizontalProp,
      lastSetVerticalProp,
    )
    if (pointsToAdd != null) {
      pinsToSet.push({
        path: path,
        pin: pointsToAdd.pin,
        value: pointsToAdd.value,
      })
    }

    const absoluteValue = fullFrame[newFramePoint]
    const newPinValue = valueToUseForPin(
      newFramePoint,
      absoluteValue,
      toggleToRelative,
      parentFrame,
    )

    pinsToSet.push({
      path: path,
      pin: newFrameProp,
      value: newPinValue,
    })

    const pointsToDelete = getPinsToDelete(
      newFrameProp,
      frame,
      lastSetHorizontalProp,
      lastSetVerticalProp,
    )

    Utils.fastForEach(pointsToDelete, (pointToDelete) => {
      pinsToUnset.push({
        path: path,
        pin: pointToDelete,
      })
    })
  })

  return {
    pinsToSet,
    pinsToUnset,
    shouldSetHorizontalPin: isHorizontalPin,
  }
}

function getPinsToAdd(
  newFrameProp: LayoutPinnedProp,
  path: ElementPath,
  frame: Frame,
  fullFrame: FullFrame,
  parentFrame: LocalRectangle,
  lastSetHorizontalProp: LayoutPinnedProp | null,
  lastSetVerticalProp: LayoutPinnedProp | null,
): PinToSet | null {
  const newFramePoint = framePointForPinnedProp(newFrameProp)
  const pinExists = frame[newFramePoint] != null
  const isHorizontalPin = isHorizontalPoint(newFramePoint)
  const lastSetHorizontalPin = Utils.optionalMap(framePointForPinnedProp, lastSetHorizontalProp)
  const lastSetVerticalPin = Utils.optionalMap(framePointForPinnedProp, lastSetVerticalProp)

  if (!pinExists) {
    if (isHorizontalPin) {
      if (lastSetHorizontalPin != null && lastSetHorizontalProp != null) {
        if (frame[lastSetHorizontalPin] == null) {
          const missingPinValue = valueToUseForPin(
            lastSetHorizontalPin,
            fullFrame[lastSetHorizontalPin],
            false,
            parentFrame,
          )
          return {
            path: path,
            pin: lastSetHorizontalProp,
            value: missingPinValue,
          }
        }
      }
    } else {
      if (lastSetVerticalPin != null && lastSetVerticalProp != null) {
        if (frame[lastSetVerticalPin] == null) {
          const missingPinValue = valueToUseForPin(
            lastSetVerticalPin,
            fullFrame[lastSetVerticalPin],
            false,
            parentFrame,
          )
          return {
            path: path,
            pin: lastSetVerticalProp,
            value: missingPinValue,
          }
        }
      }
    }
  }
  return null
}

export function getPinsToDelete(
  newFrameProp: LayoutPinnedProp,
  frame: Frame,
  lastSetHorizontalProp: LayoutPinnedProp | null,
  lastSetVerticalProp: LayoutPinnedProp | null,
): Array<LayoutPinnedProp> {
  const newFramePoint = framePointForPinnedProp(newFrameProp)
  const pinExists = frame[newFramePoint] != null
  const isHorizontalPin = isHorizontalPoint(newFramePoint)

  let pointsToDelete: Array<LayoutPinnedProp> = []
  if (!pinExists) {
    let pointsToKeep: Array<LayoutPinnedProp>
    if (isHorizontalPin) {
      if (lastSetHorizontalProp != null) {
        pointsToKeep = [newFrameProp, lastSetHorizontalProp, ...VerticalLayoutPinnedProps]
      } else {
        const HorizontalPreference: Array<LayoutPinnedProp> =
          frame.width === MaxContent ? HorizontalPropPreferenceHug : HorizontalPropPreference
        const pinToKeep = HorizontalPreference.find((p) => frame[p] != null)
        pointsToKeep = Utils.maybeToArray(pinToKeep).concat([
          newFrameProp,
          ...VerticalLayoutPinnedProps,
        ])
      }
    } else {
      if (lastSetVerticalProp != null) {
        pointsToKeep = [newFrameProp, lastSetVerticalProp, ...HorizontalLayoutPinnedProps]
      } else {
        const VerticalPreference: Array<LayoutPinnedProp> =
          frame.height === MaxContent ? VerticalPropPreferenceHug : VerticalPropPreference
        const pinToKeep = VerticalPreference.find((p) => frame[p] != null)
        pointsToKeep = Utils.maybeToArray(pinToKeep).concat([
          newFrameProp,
          ...HorizontalLayoutPinnedProps,
        ])
      }
    }

    Utils.fastForEach(LayoutPinnedProps, (framePoint) => {
      if (!pointsToKeep.includes(framePoint) && frame[framePoint] !== undefined) {
        pointsToDelete.push(framePoint)
      }
    })
  }
  return pointsToDelete
}

export interface FramePinInfo {
  isPrimaryPosition: boolean
  isRelativePosition: boolean
}

export type FramePinsInfo = { [key in FramePoint]: FramePinInfo }

export interface UsePinTogglingResult {
  framePins: FramePinsInfo
  togglePin: (newFrameProp: LayoutPinnedProp) => void
  resetAllPins: () => void
}

export function usePinToggling(): UsePinTogglingResult {
  const dispatch = useDispatch()
  const selectedViewsRef = useRefSelectedViews()
  const jsxMetadataRef = useRefEditorState((store) => {
    return store.editor.jsxMetadata
  })

  const elementsRef = useRefEditorState((store) =>
    selectedViewsRef.current.map((e) =>
      MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, e),
    ),
  )
  const propertyTarget = useContextSelector(InspectorPropsContext, (contextData) => {
    return contextData.targetPath
  })

  const elementFrames = useEditorState(
    Substores.fullStore,
    (store): ReadonlyArray<Frame> => {
      const selectedElementsMetadata = mapDropNulls(
        (path) => MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path),
        selectedViewsRef.current,
      )

      return selectedElementsMetadata.map((element) => getFramePointsFromMetadata(element))
    },
    'usePinToggling elementFrames',
    fastDeepEqual,
  )

  const framePins = React.useMemo((): FramePinsInfo => {
    const allHorizontalPoints = HorizontalFramePoints.filter((p) => allPinsMatch(p, elementFrames))
    const allVerticalPoints = VerticalFramePoints.filter((p) => allPinsMatch(p, elementFrames))
    const framePoints = [...allHorizontalPoints, ...allVerticalPoints]
    const firstFrame = elementFrames[0]

    return Utils.mapArrayToDictionary(
      framePoints,
      (point) => point,
      (point) => {
        const firstFrameAtPoint = firstFrame == null ? null : firstFrame[point]
        return {
          isPrimaryPosition: true,
          isRelativePosition: firstFrameAtPoint != null && isPercentPin(firstFrameAtPoint),
        }
      },
    )
  }, [elementFrames])

  const [lastHorizontalProp, setLastHorizontalProp] = React.useState<LayoutPinnedProp | null>(null)
  const [lastVerticalProp, setLastVerticalProp] = React.useState<LayoutPinnedProp | null>(null)

  const width = useInspectorLayoutInfo<LayoutPinnedProp>('width')
  const height = useInspectorLayoutInfo<LayoutPinnedProp>('height')
  const left = useInspectorLayoutInfo<LayoutPinnedProp>('left')
  const top = useInspectorLayoutInfo<LayoutPinnedProp>('top')
  const right = useInspectorLayoutInfo<LayoutPinnedProp>('right')
  const bottom = useInspectorLayoutInfo<LayoutPinnedProp>('bottom')

  const togglePin = React.useCallback(
    (newFrameProp: LayoutPinnedProp) => {
      const frameInfo: ReadonlyArray<ElementFrameInfo> = elementFrames.map((frame, index) => {
        const path = selectedViewsRef.current[index]
        const parentPath = EP.parentPath(path)
        const localFrame = elementsRef.current[index]?.localFrame
        const parentFrame = MetadataUtils.getFrameOrZeroRect(parentPath, jsxMetadataRef.current)
        return {
          path: path,
          frame: frame,
          localFrame:
            localFrame != null && isInfinityRectangle(localFrame)
              ? zeroLocalRect
              : localFrame ?? null,
          parentFrame: parentFrame,
        }
      })

      const maybeInvalidGroupState = maybeInvalidGroupStates(
        selectedViewsRef.current,
        jsxMetadataRef.current,
        (path) => {
          const group = MetadataUtils.getJSXElementFromMetadata(
            jsxMetadataRef.current,
            EP.parentPath(path),
          )
          return maybeGroupWithoutFixedSizeForFill(group) ?? null
        },
      )
      if (maybeInvalidGroupState != null) {
        dispatch([groupErrorToastAction(maybeInvalidGroupState)])
        return
      }

      const { pinsToSet, pinsToUnset, shouldSetHorizontalPin } = changePin(
        newFrameProp,
        {
          width,
          height,
          left,
          top,
          right,
          bottom,
        },
        frameInfo,
        lastHorizontalProp,
        lastVerticalProp,
      )

      const setPinActions = pinsToSet.map(({ path, pin, value }) =>
        setProp_UNSAFE(
          path,
          stylePropPathMappingFn(pin, propertyTarget),
          jsExpressionValue(value, emptyComments),
        ),
      )

      const unsetPinActions = pinsToUnset.map(({ path, pin }) =>
        unsetProperty(path, stylePropPathMappingFn(pin, propertyTarget)),
      )

      const actions = [...setPinActions, ...unsetPinActions]

      dispatch(actions, 'everyone')
      if (shouldSetHorizontalPin) {
        setLastHorizontalProp(newFrameProp)
      } else {
        setLastVerticalProp(newFrameProp)
      }
    },
    [
      elementFrames,
      selectedViewsRef,
      elementsRef,
      width,
      height,
      left,
      top,
      right,
      bottom,
      dispatch,
      jsxMetadataRef,
      lastHorizontalProp,
      lastVerticalProp,
      propertyTarget,
    ],
  )

  const resetAllPins = React.useCallback(() => {
    const actions = selectedViewsRef.current.map(resetPins)
    dispatch(actions, 'everyone')
  }, [selectedViewsRef, dispatch])

  return {
    framePins: framePins,
    togglePin: togglePin,
    resetAllPins: resetAllPins,
  }
}
