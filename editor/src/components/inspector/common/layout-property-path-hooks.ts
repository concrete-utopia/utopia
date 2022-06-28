import fastDeepEqual from 'fast-deep-equal'
import {
  AllFramePoints,
  Frame,
  FramePin,
  FramePoint,
  HorizontalFramePoints,
  isHorizontalPoint,
  isPercentPin,
  valueToUseForPin,
  VerticalFramePoints,
} from 'utopia-api/core'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import {
  framePointForPinnedProp,
  HorizontalLayoutPinnedProps,
  LayoutPinnedProp,
  LayoutPinnedProps,
  pinnedPropForFramePoint,
  VerticalLayoutPinnedProps,
} from '../../../core/layout/layout-helpers-new'
import { findElementAtPath, MetadataUtils } from '../../../core/model/element-metadata-utils'
import { isLeft, right as eitherRight } from '../../../core/shared/either'
import {
  emptyComments,
  isJSXElement,
  jsxAttributeValue,
} from '../../../core/shared/element-template'
import { LocalRectangle } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import * as EP from '../../../core/shared/element-path'
import Utils from '../../../utils/utils'
import { resetPins, setProp_UNSAFE, unsetProperty } from '../../editor/actions/action-creators'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { getFullFrame } from '../../frame'
import {
  InspectorInfo,
  useInspectorLayoutInfo,
  useSelectedViews,
  useRefSelectedViews,
  InspectorPropsContext,
  stylePropPathMappingFn,
} from './property-path-hooks'

import React from 'react'
import { CSSNumber, cssNumberToString } from './css-utils'
import { getJSXComponentsAndImportsForPathFromState } from '../../editor/store/editor-state'
import { useContextSelector } from 'use-context-selector'

const HorizontalPropPreference: Array<LayoutPinnedProp> = ['left', 'width', 'right']
const VerticalPropPreference: Array<LayoutPinnedProp> = ['top', 'height', 'bottom']

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
  const otherHorizontalProp: LayoutPinnedProp | null = getLastSetPinOrNull(
    lastHorizontalProp,
    pinsInfo,
  )
  const otherVerticalProp: LayoutPinnedProp | null = getLastSetPinOrNull(lastVerticalProp, pinsInfo)

  const newFramePoint = framePointForPinnedProp(newFrameProp)
  const pinInfoForProp = pinsInfo[newFrameProp]
  const otherHorizontalPin = Utils.optionalMap(framePointForPinnedProp, otherHorizontalProp)
  const otherVerticalPin = Utils.optionalMap(framePointForPinnedProp, otherVerticalProp)
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

    const pinExists = frame[newFramePoint] != null

    let pointsToDelete: Array<LayoutPinnedProp> = []
    if (!pinExists) {
      let pointsToKeep: Array<LayoutPinnedProp>
      if (isHorizontalPin) {
        if (otherHorizontalPin != null && otherHorizontalProp != null) {
          if (frame[otherHorizontalPin] == null) {
            const missingPinValue = valueToUseForPin(
              otherHorizontalPin,
              fullFrame[otherHorizontalPin],
              false,
              parentFrame,
            )
            pinsToSet.push({
              path: path,
              pin: otherHorizontalProp,
              value: missingPinValue,
            })
          }

          pointsToKeep = [newFrameProp, otherHorizontalProp, ...VerticalLayoutPinnedProps]
        } else {
          const pinToKeep = HorizontalPropPreference.find((p) => frame[p] != null)
          pointsToKeep = Utils.maybeToArray(pinToKeep).concat([
            newFrameProp,
            ...VerticalLayoutPinnedProps,
          ])
        }
      } else {
        if (otherVerticalPin != null && otherVerticalProp != null) {
          if (frame[otherVerticalPin] == null) {
            const missingPinValue = valueToUseForPin(
              otherVerticalPin,
              fullFrame[otherVerticalPin],
              false,
              parentFrame,
            )
            pinsToSet.push({
              path: path,
              pin: otherVerticalProp,
              value: missingPinValue,
            })
          }

          pointsToKeep = [newFrameProp, otherVerticalProp, ...HorizontalLayoutPinnedProps]
        } else {
          const pinToKeep = VerticalPropPreference.find((p) => frame[p] != null)
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
  const dispatch = useEditorState((store) => store.dispatch, 'usePinToggling dispatch')
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
    (store): ReadonlyArray<Frame> => {
      const jsxElements = selectedViewsRef.current.map((path) => {
        const rootComponents = getJSXComponentsAndImportsForPathFromState(
          path,
          store.editor,
          store.derived,
        ).components
        return findElementAtPath(path, rootComponents)
      })

      return jsxElements.map((elem) => {
        if (elem != null && isJSXElement(elem)) {
          return LayoutPinnedProps.reduce<Frame>((working, point) => {
            const value = getLayoutProperty(point, eitherRight(elem.props), propertyTarget)
            if (isLeft(value)) {
              return working
            } else {
              return {
                ...working,
                [point]: value.value,
              }
            }
          }, {})
        } else {
          return {}
        }
      })
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
        const parentFrame = MetadataUtils.getFrame(parentPath, jsxMetadataRef.current)
        return {
          path: path,
          frame: frame,
          localFrame: elementsRef.current[index]?.localFrame ?? null,
          parentFrame: parentFrame,
        }
      })

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
          jsxAttributeValue(value, emptyComments),
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
