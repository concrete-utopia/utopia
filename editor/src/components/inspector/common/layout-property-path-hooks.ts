import * as fastDeepEqual from 'fast-deep-equal'
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
} from 'utopia-api'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import {
  createLayoutPropertyPath,
  framePointForPinnedProp,
  LayoutPinnedProp,
  pinnedPropForFramePoint,
} from '../../../core/layout/layout-helpers-new'
import { findElementAtPath, MetadataUtils } from '../../../core/model/element-metadata-utils'
import { isLeft, right } from '../../../core/shared/either'
import { isJSXElement, jsxAttributeValue } from '../../../core/shared/element-template'
import { LocalRectangle } from '../../../core/shared/math-utils'
import { InstancePath } from '../../../core/shared/project-file-types'
import * as TP from '../../../core/shared/template-path'
import Utils from '../../../utils/utils'
import { resetPins, setProp_UNSAFE, unsetProperty } from '../../editor/actions/actions'
import { getOpenUtopiaJSXComponentsFromState } from '../../editor/store/editor-state'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { getFullFrame } from '../../frame'
import {
  InspectorInfo,
  useInspectorLayoutInfo,
  useSelectedViews,
  useRefSelectedViews,
} from './property-path-hooks'

import React = require('react')
import { usePropControlledRef_DANGEROUS } from './inspector-utils'

const HorizontalPinPreference = [
  FramePoint.Left,
  FramePoint.Width,
  FramePoint.Right,
  FramePoint.CenterX,
]
const VerticalPinPreference = [
  FramePoint.Top,
  FramePoint.Height,
  FramePoint.Bottom,
  FramePoint.CenterY,
]

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
  path: InstancePath
  pin: FramePoint
  value: FramePin
}

interface PinToUnset {
  path: InstancePath
  pin: FramePoint
}

interface ChangePinResult {
  pinsToSet: ReadonlyArray<PinToSet>
  pinsToUnset: ReadonlyArray<PinToUnset>
  shouldSetHorizontalPin: boolean
}

export interface ElementFrameInfo {
  path: InstancePath
  frame: Frame
  localFrame: LocalRectangle | null
  parentFrame: LocalRectangle | null
}

type PinInspectorInfo = InspectorInfo<string | number | undefined>

export type PinsInfo = { [key in LayoutPinnedProp]: PinInspectorInfo }

function getOtherHorizontalPin(
  newPin: LayoutPinnedProp,
  lastSetPin: LayoutPinnedProp | null,
): LayoutPinnedProp | null {
  if (newPin === 'PinnedCenterX') {
    // When setting the CX pin, we always want to pair it with width
    return 'Width'
  } else if (lastSetPin === 'PinnedCenterX' && newPin !== 'Width') {
    // When setting a new pin, if the last set pin was CX then replace it with width,
    // unless the user is just clicking the W pin again
    return 'Width'
  } else {
    return lastSetPin
  }
}

function getOtherVerticalPin(
  newPin: LayoutPinnedProp,
  lastSetPin: LayoutPinnedProp | null,
): LayoutPinnedProp | null {
  if (newPin === 'PinnedCenterY') {
    // When setting the CY pin, we always want to pair it with height
    return 'Height'
  } else if (lastSetPin === 'PinnedCenterY' && newPin !== 'Height') {
    // When setting a new pin, if the last set pin was CY then replace it with height,
    // unless the user is just clicking the H pin again
    return 'Height'
  } else {
    return lastSetPin
  }
}

export function changePin(
  newFrameProp: LayoutPinnedProp,
  pinsInfo: PinsInfo,
  frameInfoForElements: ReadonlyArray<ElementFrameInfo>,
  lastHorizontalProp: LayoutPinnedProp | null,
  lastVerticalProp: LayoutPinnedProp | null,
): ChangePinResult {
  const otherHorizontalProp: LayoutPinnedProp | null = getOtherHorizontalPin(
    newFrameProp,
    lastHorizontalProp,
  )
  const otherVerticalProp: LayoutPinnedProp | null = getOtherVerticalPin(
    newFrameProp,
    lastVerticalProp,
  )

  const newFramePoint = framePointForPinnedProp(newFrameProp)
  const pinInfoForProp = pinsInfo[newFrameProp]
  const otherHorizontalPin = Utils.optionalMap(framePointForPinnedProp, otherHorizontalProp)
  const otherVerticalPin = Utils.optionalMap(framePointForPinnedProp, otherVerticalProp)
  const toggleToRelative =
    pinInfoForProp.propertyStatus.identical &&
    pinInfoForProp.value != null &&
    !isPercentPin(pinInfoForProp.value)

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

    let pointsToDelete: Array<FramePoint> = []
    if (!pinExists) {
      let pointsToKeep: Array<FramePoint>
      if (isHorizontalPin) {
        if (otherHorizontalPin != null) {
          if (frame[otherHorizontalPin] == null) {
            const missingPinValue = valueToUseForPin(
              otherHorizontalPin,
              fullFrame[otherHorizontalPin],
              false,
              parentFrame,
            )
            pinsToSet.push({
              path: path,
              pin: otherHorizontalPin,
              value: missingPinValue,
            })
          }

          pointsToKeep = [newFramePoint, otherHorizontalPin, ...VerticalFramePoints]
        } else {
          const pinToKeep = HorizontalPinPreference.find((p) => frame[p] != null)
          pointsToKeep = Utils.maybeToArray(pinToKeep).concat([
            newFramePoint,
            ...VerticalFramePoints,
          ])
        }
      } else {
        if (otherVerticalPin != null) {
          if (frame[otherVerticalPin] == null) {
            const missingPinValue = valueToUseForPin(
              otherVerticalPin,
              fullFrame[otherVerticalPin],
              false,
              parentFrame,
            )
            pinsToSet.push({
              path: path,
              pin: otherVerticalPin,
              value: missingPinValue,
            })
          }

          pointsToKeep = [newFramePoint, otherVerticalPin, ...HorizontalFramePoints]
        } else {
          const pinToKeep = VerticalPinPreference.find((p) => frame[p] != null)
          pointsToKeep = Utils.maybeToArray(pinToKeep).concat([
            newFramePoint,
            ...HorizontalFramePoints,
          ])
        }
      }

      Utils.fastForEach(AllFramePoints, (framePoint) => {
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
      pin: newFramePoint,
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
    return store.editor.jsxMetadataKILLME
  })

  const filteredSelectedViews = usePropControlledRef_DANGEROUS(
    TP.filterScenes(selectedViewsRef.current),
  )

  const elementsRef = useRefEditorState((store) =>
    TP.filterScenes(selectedViewsRef.current).map((e) =>
      MetadataUtils.getElementByInstancePathMaybe(store.editor.jsxMetadataKILLME, e),
    ),
  )

  const elementFrames = useEditorState(
    (store): ReadonlyArray<Frame> => {
      const rootComponents = getOpenUtopiaJSXComponentsFromState(store.editor)

      const jsxElements = TP.filterScenes(selectedViewsRef.current).map((path) =>
        findElementAtPath(path, rootComponents, store.editor.jsxMetadataKILLME),
      )

      return jsxElements.map((elem) => {
        if (elem != null && isJSXElement(elem)) {
          return AllFramePoints.reduce<Frame>((working, point) => {
            const layoutProp = pinnedPropForFramePoint(point)
            const value = getLayoutProperty(layoutProp, right(elem.props))
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

  const Width = useInspectorLayoutInfo<LayoutPinnedProp>('Width')
  const Height = useInspectorLayoutInfo<LayoutPinnedProp>('Height')
  const PinnedLeft = useInspectorLayoutInfo<LayoutPinnedProp>('PinnedLeft')
  const PinnedTop = useInspectorLayoutInfo<LayoutPinnedProp>('PinnedTop')
  const PinnedRight = useInspectorLayoutInfo<LayoutPinnedProp>('PinnedRight')
  const PinnedBottom = useInspectorLayoutInfo<LayoutPinnedProp>('PinnedBottom')
  const PinnedCenterX = useInspectorLayoutInfo<LayoutPinnedProp>('PinnedCenterX')
  const PinnedCenterY = useInspectorLayoutInfo<LayoutPinnedProp>('PinnedCenterY')

  const togglePin = React.useCallback(
    (newFrameProp: LayoutPinnedProp) => {
      const frameInfo: ReadonlyArray<ElementFrameInfo> = elementFrames.map((frame, index) => {
        const path = filteredSelectedViews.current[index]
        const parentPath = TP.parentPath(path)
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
          Width,
          Height,
          PinnedLeft,
          PinnedTop,
          PinnedRight,
          PinnedBottom,
          PinnedCenterX,
          PinnedCenterY,
        },
        frameInfo,
        lastHorizontalProp,
        lastVerticalProp,
      )

      const setPinActions = pinsToSet.map(({ path, pin, value }) =>
        setProp_UNSAFE(
          path,
          createLayoutPropertyPath(pinnedPropForFramePoint(pin)),
          jsxAttributeValue(value),
        ),
      )

      const unsetPinActions = pinsToUnset.map(({ path, pin }) =>
        unsetProperty(path, createLayoutPropertyPath(pinnedPropForFramePoint(pin))),
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
      filteredSelectedViews,
      elementsRef,
      Width,
      Height,
      PinnedLeft,
      PinnedTop,
      PinnedRight,
      PinnedBottom,
      PinnedCenterX,
      PinnedCenterY,
      dispatch,
      jsxMetadataRef,
      lastHorizontalProp,
      lastVerticalProp,
    ],
  )

  const resetAllPins = React.useCallback(() => {
    const actions = filteredSelectedViews.current.map(resetPins)
    dispatch(actions, 'everyone')
  }, [filteredSelectedViews, dispatch])

  return {
    framePins: framePins,
    togglePin: togglePin,
    resetAllPins: resetAllPins,
  }
}
