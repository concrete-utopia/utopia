import { KeyState } from './interaction-state'
import { setsEqual } from '../../../core/shared/set-utils'
import { last } from '../../../core/shared/array-utils'
import { Modifier, Modifiers } from '../../../utils/modifiers'
import { CanvasVector } from '../../../core/shared/math-utils'
import Keyboard, { KeyCharacter } from '../../../utils/keyboard'

export interface AccumulatedPresses extends KeyState {
  count: number
}

export function getLastKeyPressState(keyStates: Array<KeyState>): KeyState | null {
  for (let index = keyStates.length - 1; index >= 0; index--) {
    const state = keyStates[index]
    if (Array.from(state.keysPressed).some(Keyboard.keyIsArrow)) {
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

export function getDragDeltaFromKey(key: KeyCharacter, modifiers: Modifiers): CanvasVector {
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
