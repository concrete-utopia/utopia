import { shallowEqual } from '../../../../core/shared/equality-utils'
import * as PP from '../../../../core/shared/property-path'
import Keyboard, { KeyCharacter } from '../../../../utils/keyboard'
import { emptyModifiers, Modifier, Modifiers } from '../../../../utils/modifiers'
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

const isDigit = (c: string) => ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'].includes(c)

export function parseOpacityFromKeyboard(keys: string): string | null {
  let tail = keys.slice(-3)
  if (tail === '0' || tail === '100') {
    return '100%'
  }

  tail = tail.slice(-2)
  if (tail.length === 1 && isDigit(tail[0])) {
    return tail + '0%'
  }

  if (tail.length === 2 && isDigit(tail[0]) && isDigit(tail[1])) {
    return tail + '%'
  }

  return null
}

function isSetOpacityShortcut(modifiers: Modifiers, key: KeyCharacter): boolean {
  return Modifier.equal(modifiers, emptyModifiers) && Keyboard.keyTriggersOpacityStrategy(key)
}

function fitness(interactionSession: InteractionSession | null): number {
  if (interactionSession == null || interactionSession.interactionData.type !== 'KEYBOARD') {
    return 0
  }
  const lastKeyState = getLastKeyPressState(
    interactionSession.interactionData.keyStates,
    Keyboard.keyTriggersOpacityStrategy,
  )

  const matches =
    lastKeyState != null &&
    Array.from(lastKeyState.keysPressed).every((key) =>
      isSetOpacityShortcut(lastKeyState.modifiers, key),
    )

  return matches ? 1 : 0
}
