import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { elementOnlyHasTextChildren } from '../../../../core/model/element-template-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { isLeft } from '../../../../core/shared/either'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { isJSXElement } from '../../../../core/shared/element-template'
import { clamp, safeParseInt } from '../../../../core/shared/math-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import type { KeyCharacter } from '../../../../utils/keyboard'
import Keyboard from '../../../../utils/keyboard'
import type { Modifiers } from '../../../../utils/modifiers'
import { setProperty } from '../../commands/set-property-command'
import { getDescriptiveStrategyLabelWithRetargetedPaths } from '../canvas-strategies'
import type { InteractionCanvasState, CanvasStrategy } from '../canvas-strategy-types'
import { emptyStrategyApplicationResult, strategyApplicationResult } from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { retargetStrategyToChildrenOfFragmentLikeElements } from './fragment-like-helpers'
import { accumulatePresses, getLastKeyPressState } from './shared-keyboard-strategy-helpers'
import {
  adjustFontWeight,
  getFontWeightFromMetadata,
  isAdjustFontWeightShortcut,
} from './text-editor-utils'

const FontWeightProp = 'fontWeight'

export function keyboardSetFontWeightStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const { pathsWereReplaced, paths } = retargetStrategyToChildrenOfFragmentLikeElements(canvasState)
  const validTargets = paths.filter((path) => isValidTarget(canvasState.startingMetadata, path))

  if (validTargets.length === 0) {
    return null
  }

  return {
    id: 'set-font-weight',
    name: 'Set font weight',
    descriptiveLabel: getDescriptiveStrategyLabelWithRetargetedPaths(
      'Changing Font Weight',
      pathsWereReplaced,
    ),
    icon: {
      category: 'tools',
      type: 'pointer',
    },
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
            getFontWeightFromMetadata(canvasState.startingMetadata, path),
          ),
        validTargets,
      ).map(([fontWeight, path]) =>
        setProperty(
          'always',
          path,
          PP.create('style', FontWeightProp),
          adjustFontWeight(fontWeight, fontSizeDelta),
        ),
      )

      return strategyApplicationResult(commands, validTargets)
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
    getFontWeightFromMetadata(metadata, elementPath) != null
  )
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
