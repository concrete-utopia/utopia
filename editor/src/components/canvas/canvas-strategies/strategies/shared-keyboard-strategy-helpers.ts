import type { InteractionSession, KeyState } from '../interaction-state'
import { StrategyState } from '../interaction-state'
import { setsEqual } from '../../../../core/shared/set-utils'
import { last, mapDropNulls } from '../../../../core/shared/array-utils'
import type { Modifiers } from '../../../../utils/modifiers'
import { Modifier } from '../../../../utils/modifiers'
import type { CanvasRectangle, CanvasVector } from '../../../../core/shared/math-utils'
import type { KeyCharacter } from '../../../../utils/keyboard'
import {
  collectParentAndSiblingGuidelines,
  oneGuidelinePerDimension,
} from '../../controls/guideline-helpers'
import type { GuidelineWithSnappingVectorAndPointsOfRelevance } from '../../guideline'
import { Guidelines } from '../../guideline'
import type { InteractionCanvasState } from '../canvas-strategy-types'
import { getTargetPathsFromInteractionTarget } from '../canvas-strategy-types'
import Utils from '../../../../utils/utils'

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
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  draggedFrame: CanvasRectangle,
): Array<GuidelineWithSnappingVectorAndPointsOfRelevance> {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  const moveGuidelines = collectParentAndSiblingGuidelines(
    interactionSession.latestMetadata,
    canvasState.startingAllElementProps,
    canvasState.startingElementPathTree,
    selectedElements,
  ).map((g) => g.guideline)

  const { horizontalPoints, verticalPoints } = Utils.getRectPointsAlongAxes(draggedFrame)
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
