import * as PP from '../../../../core/shared/property-path'
import Keyboard from '../../../../utils/keyboard'
import { setProperty } from '../../commands/set-property-command'
import {
  InteractionCanvasState,
  CanvasStrategy,
  getTargetPathsFromInteractionTarget,
  emptyStrategyApplicationResult,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession, KeyState } from '../interaction-state'
import { getLastKeyPressState } from './shared-keyboard-strategy-helpers'

export function keyboardSetOpacityStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)

  if (selectedElements.length === 0) {
    return null
  }

  return {
    id: 'set-opacity',
    name: 'Set opacity',
    controlsToRender: [],
    fitness: fitness(interactionSession),
    apply: () => {
      if (interactionSession == null || interactionSession.interactionData.type !== 'KEYBOARD') {
        return emptyStrategyApplicationResult
      }

      const inputValue = parseOpacityFromKeyboard(
        getKeySequence(interactionSession.interactionData.keyStates),
      )
      if (inputValue == null) {
        return emptyStrategyApplicationResult
      }

      const commands = selectedElements.map((path) =>
        setProperty('always', path, PP.create(['style', 'opacity']), inputValue),
      )

      return strategyApplicationResult(commands)
    },
  }
}

function getKeySequence(keyStates: Array<KeyState>): string {
  return keyStates.flatMap((s) => Array.from(s.keysPressed.values())).join('')
}

export function parseOpacityFromKeyboard(keys: string): string | null {
  const tail = keys.slice(-2)
  if (tail === '00' || tail === '0') {
    return '100%'
  }

  if (tail.length === 1) {
    return tail + '0%'
  }

  if (tail.length === 2) {
    return tail + '%'
  }

  return null
}

function fitness(interactionSession: InteractionSession | null): number {
  if (interactionSession == null || interactionSession.interactionData.type !== 'KEYBOARD') {
    return 0
  }
  const lastKeyState = getLastKeyPressState(
    interactionSession.interactionData.keyStates,
    Keyboard.keyTriggersOpacityStrategy,
  )
  if (lastKeyState == null) {
    return 0
  }
  return 1
}
