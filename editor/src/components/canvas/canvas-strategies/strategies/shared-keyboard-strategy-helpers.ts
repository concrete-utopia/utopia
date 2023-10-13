import { last, mapDropNulls } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import type { CanvasRectangle, CanvasVector } from '../../../../core/shared/math-utils'
import { canvasRectangle, getRoundedRectPointsAlongAxes } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { setsEqual } from '../../../../core/shared/set-utils'
import type { KeyCharacter } from '../../../../utils/keyboard'
import type { Modifiers } from '../../../../utils/modifiers'
import { Modifier } from '../../../../utils/modifiers'
import Utils from '../../../../utils/utils'
import type { CanvasFrameAndTarget } from '../../canvas-types'
import {
  collectParentAndSiblingGuidelines,
  oneGuidelinePerDimension,
} from '../../controls/guideline-helpers'
import type { GuidelineWithSnappingVectorAndPointsOfRelevance } from '../../guideline'
import { Guidelines } from '../../guideline'
import type { InteractionSession, KeyState } from '../interaction-state'

export interface AccumulatedPresses extends KeyState {
  count: number
}

export function getLastKeyPressState(
  keyStates: Array<KeyState>,
  checkKey: (key: KeyCharacter) => boolean,
): KeyState | null {
  for (let index = keyStates.length - 1; index >= 0; index--) {
    const state = keyStates[index]
    if (Array.from(state.keysPressed).some(checkKey)) {
      return state
    }
  }
  return null
}

export function accumulatePresses(keyStates: Array<KeyState>): Array<AccumulatedPresses> {
  let result: Array<AccumulatedPresses> = []
  for (const keyState of keyStates) {
    if (keyState.keysPressed.size !== 0) {
      const latestAccumulatedValue = last(result)
      if (latestAccumulatedValue == null) {
        result.push({
          ...keyState,
          count: 1,
        })
      } else {
        if (
          setsEqual(keyState.keysPressed, latestAccumulatedValue.keysPressed) &&
          Modifier.equal(keyState.modifiers, latestAccumulatedValue.modifiers)
        ) {
          latestAccumulatedValue.count += 1
        } else {
          result.push({
            ...keyState,
            count: 1,
          })
        }
      }
    }
  }
  return result
}

export function getMovementDeltaFromKey(key: KeyCharacter, modifiers: Modifiers): CanvasVector {
  const step = modifiers.shift ? 10 : 1
  switch (key) {
    case 'left':
      return {
        x: -step,
        y: 0,
      } as CanvasVector
    case 'right':
      return {
        x: step,
        y: 0,
      } as CanvasVector
    case 'up':
      return {
        x: 0,
        y: -step,
      } as CanvasVector
    case 'down':
      return {
        x: 0,
        y: step,
      } as CanvasVector
    default:
      return {
        x: 0,
        y: 0,
      } as CanvasVector
  }
}

export function getKeyboardStrategyGuidelines(
  snapTargets: ElementPath[],
  interactionSession: InteractionSession,
  draggedFrame: CanvasRectangle,
): Array<GuidelineWithSnappingVectorAndPointsOfRelevance> {
  const moveGuidelines = collectParentAndSiblingGuidelines(
    snapTargets,
    interactionSession.latestMetadata,
  ).map((g) => g.guideline)

  const { horizontalPoints, verticalPoints } = getRoundedRectPointsAlongAxes(draggedFrame)
  const closestGuideLines: Array<GuidelineWithSnappingVectorAndPointsOfRelevance> = mapDropNulls(
    (guideline) => {
      switch (guideline.type) {
        case 'XAxisGuideline': {
          const snappingVector = Guidelines.getOffsetToSnapToXGuideline(
            horizontalPoints,
            guideline,
            null,
          )
          if (Utils.magnitude(snappingVector) === 0) {
            return {
              guideline: {
                ...guideline,
                yTop: Math.min(guideline.yTop, draggedFrame.y),
                yBottom: Math.max(guideline.yBottom, draggedFrame.y + draggedFrame.height),
              },
              snappingVector: snappingVector,
              pointsOfRelevance: [],
            } as GuidelineWithSnappingVectorAndPointsOfRelevance
          } else {
            return null
          }
        }
        case 'YAxisGuideline': {
          const snappingVector = Guidelines.getOffsetToSnapToYGuideline(
            verticalPoints,
            guideline,
            null,
          )
          if (Utils.magnitude(snappingVector) === 0) {
            return {
              guideline: {
                ...guideline,
                xLeft: Math.min(guideline.xLeft, draggedFrame.x),
                xRight: Math.max(guideline.xRight, draggedFrame.x + draggedFrame.width),
              },
              snappingVector: snappingVector,
              pointsOfRelevance: [],
            } as GuidelineWithSnappingVectorAndPointsOfRelevance
          } else {
            return null
          }
        }
        case 'CornerGuideline':
          return null
        default:
          const _exhaustiveCheck: never = guideline
          throw new Error(`Unexpected value for guideline of type: ${guideline}`)
      }
    },
    moveGuidelines,
  )
  const winningGuidelines = oneGuidelinePerDimension(closestGuideLines)
  return winningGuidelines
}

/**
 * Adds the new bounds to the array, if for a new element, or merges the existing element bounds
 * with the updated data from the new element bounds which have diverged from the original frame.
 */
export function addOrMergeIntendedBounds(
  boundsArray: CanvasFrameAndTarget[],
  originalFrame: CanvasRectangle,
  newBounds: CanvasFrameAndTarget,
): CanvasFrameAndTarget[] {
  const existingIndex = boundsArray.findIndex((other) =>
    EP.pathsEqual(other.target, newBounds.target),
  )

  // If the bounds for the current element don't exist in the array, return the array + the new bounds.
  if (existingIndex < 0) {
    return [...boundsArray, newBounds]
  }

  // …otherwise, merge the new values from the new bounds with the existing ones in the array.
  function replaceIfNew(newValue: number, originalValue: number, existingValue: number): number {
    return newValue !== originalValue ? newValue : existingValue
  }
  const result = [...boundsArray]
  const existing = result[existingIndex]
  result[existingIndex] = {
    ...existing,
    frame: canvasRectangle({
      x: replaceIfNew(newBounds.frame.x, originalFrame.x, existing.frame.x),
      y: replaceIfNew(newBounds.frame.y, originalFrame.y, existing.frame.y),
      width: replaceIfNew(newBounds.frame.width, originalFrame.width, existing.frame.width),
      height: replaceIfNew(newBounds.frame.height, originalFrame.height, existing.frame.height),
    }),
  }
  return result
}
