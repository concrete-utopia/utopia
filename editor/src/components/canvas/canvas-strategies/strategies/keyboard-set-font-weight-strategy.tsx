import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { elementOnlyHasTextChildren } from '../../../../core/model/element-template-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { isLeft } from '../../../../core/shared/either'
import { ElementInstanceMetadataMap, isJSXElement } from '../../../../core/shared/element-template'
import { clamp, safeParseInt } from '../../../../core/shared/math-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import Keyboard, { KeyCharacter } from '../../../../utils/keyboard'
import { Modifiers } from '../../../../utils/modifiers'
import { setProperty } from '../../commands/set-property-command'
import {
  InteractionCanvasState,
  CanvasStrategy,
  getTargetPathsFromInteractionTarget,
  emptyStrategyApplicationResult,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { accumulatePresses, getLastKeyPressState } from './shared-keyboard-strategy-helpers'

const FontWeightProp = 'fontWeight'

export function keyboardSetFontWeightStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const validTargets = getTargetPathsFromInteractionTarget(canvasState.interactionTarget).filter(
    (path) => isValidTarget(canvasState.startingMetadata, path),
  )

  if (validTargets.length === 0) {
    return null
  }

  return {
    id: 'set-font-weight',
    name: 'Set font weight',
    controlsToRender: [],
    fitness: fitness(interactionSession),
    apply: () => {
      if (interactionSession == null || interactionSession.interactionData.type !== 'KEYBOARD') {
        return emptyStrategyApplicationResult
      }

      let fontSizeDelta: number = 0

      const accumulatedPresses = accumulatePresses(interactionSession.interactionData.keyStates)
      accumulatedPresses.forEach((accumulatedPress) => {
        accumulatedPress.keysPressed.forEach((key) => {
          if (accumulatedPress.modifiers.alt && accumulatedPress.modifiers.cmd) {
            if (key === 'period') {
              fontSizeDelta += accumulatedPress.count
            }
            if (key === 'comma') {
              fontSizeDelta -= accumulatedPress.count
            }
          }
        })
      })

      const commands = mapDropNulls(
        (path) =>
          optionalMap(
            (w): [number, ElementPath] => [w, path],
            getFontWeightFromComputedStyle(canvasState.startingMetadata, path),
          ),
        validTargets,
      ).map(([fontWeight, path]) =>
        setProperty(
          'always',
          path,
          PP.create(['style', FontWeightProp]),
          adjustFontWeight(fontWeight, fontSizeDelta),
        ),
      )

      return strategyApplicationResult(commands)
    },
  }
}

function isValidTarget(metadata: ElementInstanceMetadataMap, elementPath: ElementPath): boolean {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
    return false
  }
  return (
    elementOnlyHasTextChildren(element.element.value) ||
    getFontWeightFromComputedStyle(metadata, elementPath) != null
  )
}

export function isAdjustFontWeightShortcut(modifiers: Modifiers, key: KeyCharacter): boolean {
  return modifiers.alt && modifiers.cmd && Keyboard.keyTriggersFontWeightStrategy(key)
}

function fitness(interactionSession: InteractionSession | null): number {
  if (interactionSession == null || interactionSession.interactionData.type !== 'KEYBOARD') {
    return 0
  }
  const lastKeyState = getLastKeyPressState(
    interactionSession.interactionData.keyStates,
    Keyboard.keyTriggersFontWeightStrategy,
  )

  const matches =
    lastKeyState != null &&
    Array.from(lastKeyState.keysPressed).some((key) =>
      isAdjustFontWeightShortcut(lastKeyState.modifiers, key),
    )

  return matches ? 1 : 0
}

function parseMaybeFontWeight(maybeFontSize: unknown): number | null {
  return safeParseInt(maybeFontSize as string)
}

export function getFontWeightFromComputedStyle(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): number | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null) {
    return null
  }

  return parseMaybeFontWeight(element.computedStyle?.[FontWeightProp])
}

export function adjustFontWeight(value: number, delta: number): number {
  return clamp(100, 900, value + delta * 100)
}
