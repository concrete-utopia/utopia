import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { Keyboard } from '../../../../utils/keyboard'
import type { CanvasStrategy, InteractionCanvasState } from '../canvas-strategy-types'
import {
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { CanvasVector } from '../../../../core/shared/math-utils'
import {
  canvasRectangle,
  offsetPoint,
  offsetRect,
  scaleVector,
  zeroCanvasPoint,
  zeroRectangle,
} from '../../../../core/shared/math-utils'
import {
  getInteractionMoveCommandsForSelectedElement,
  getMultiselectBounds,
} from './shared-move-strategies-helpers'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setSnappingGuidelines } from '../../commands/set-snapping-guidelines-command'
import type { CanvasCommand } from '../../commands/commands'
import {
  accumulatePresses,
  getMovementDeltaFromKey,
  getKeyboardStrategyGuidelines,
  getLastKeyPressState,
} from './shared-keyboard-strategy-helpers'
import { defaultIfNull } from '../../../../core/shared/optional-utils'
import { pushIntendedBoundsAndUpdateGroups } from '../../commands/push-intended-bounds-and-update-groups-command'
import type { CanvasFrameAndTarget } from '../../canvas-types'
import { honoursPropsPosition } from './absolute-utils'
import type { InteractionSession } from '../interaction-state'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { retargetStrategyToChildrenOfFragmentLikeElements } from './fragment-like-helpers'
import { gatherParentAndSiblingTargets } from '../../controls/guideline-helpers'
import { getDescriptiveStrategyLabelWithRetargetedPaths } from '../canvas-strategies'

export function keyboardAbsoluteMoveStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const { pathsWereReplaced, paths: selectedElements } =
    retargetStrategyToChildrenOfFragmentLikeElements(canvasState)

  if (
    pathsWereReplaced &&
    selectedElements.some((originalTarget) =>
      MetadataUtils.isComponentInstanceFromMetadata(canvasState.startingMetadata, originalTarget),
    )
  ) {
    return null
  }

  if (selectedElements.length === 0) {
    return null
  }
  if (!isApplicable(canvasState, selectedElements)) {
    return null
  }

  return {
    id: 'KEYBOARD_ABSOLUTE_MOVE',
    name: 'Move',
    descriptiveLabel: getDescriptiveStrategyLabelWithRetargetedPaths(
      'Moving Elements',
      pathsWereReplaced,
    ),
    icon: {
      category: 'modalities',
      type: 'moveabs-large',
    },
    controlsToRender: [], // Uses existing hooks in select-mode-hooks.tsx
    fitness: getFitness(interactionSession),
    apply: () => {
      if (interactionSession != null && interactionSession.interactionData.type === 'KEYBOARD') {
        const accumulatedPresses = accumulatePresses(interactionSession.interactionData.keyStates)
        let commands: Array<CanvasCommand> = []
        let intendedBounds: Array<CanvasFrameAndTarget> = []
        let keyboardMovement: CanvasVector = zeroCanvasPoint
        accumulatedPresses.forEach((accumulatedPress) => {
          accumulatedPress.keysPressed.forEach((key) => {
            const keyPressMovement = scaleVector(
              getMovementDeltaFromKey(key, accumulatedPress.modifiers),
              accumulatedPress.count,
            )
            keyboardMovement = offsetPoint(keyboardMovement, keyPressMovement)
          })
        })
        selectedElements.forEach((selectedElement) => {
          const elementResult = getInteractionMoveCommandsForSelectedElement(
            selectedElement,
            keyboardMovement,
            canvasState,
            interactionSession,
          )
          commands.push(...elementResult.commands)
          intendedBounds.push(...elementResult.intendedBounds)
        })
        const multiselectBounds = getMultiselectBounds(
          canvasState.startingMetadata,
          selectedElements,
        )
        const newFrame = offsetRect(
          defaultIfNull(canvasRectangle(zeroRectangle), multiselectBounds),
          keyboardMovement,
        )

        const snapTargets: ElementPath[] = gatherParentAndSiblingTargets(
          canvasState.startingMetadata,
          canvasState.startingAllElementProps,
          canvasState.startingElementPathTree,
          getTargetPathsFromInteractionTarget(canvasState.interactionTarget),
        )
        const guidelines = getKeyboardStrategyGuidelines(snapTargets, interactionSession, newFrame)

        commands.push(setSnappingGuidelines('mid-interaction', guidelines))
        commands.push(pushIntendedBoundsAndUpdateGroups(intendedBounds, 'starting-metadata'))
        commands.push(setElementsToRerenderCommand(selectedElements))
        return strategyApplicationResult(commands)
      } else {
        return emptyStrategyApplicationResult
      }
    },
  }
}

function isApplicable(canvasState: InteractionCanvasState, selectedElements: Array<ElementPath>) {
  return selectedElements.every((element) => {
    const elementMetadata = MetadataUtils.findElementByElementPath(
      canvasState.startingMetadata,
      element,
    )
    return (
      elementMetadata?.specialSizeMeasurements.position === 'absolute' &&
      honoursPropsPosition(canvasState, element)
    )
  })
}

function getFitness(interactionSession: InteractionSession | null): number {
  if (interactionSession != null && interactionSession.interactionData.type === 'KEYBOARD') {
    const lastKeyState = getLastKeyPressState(
      interactionSession.interactionData.keyStates,
      Keyboard.keyIsArrow,
    )
    if (lastKeyState != null) {
      // 'Alt' determines if the distance guidelines should be shown.
      const shiftOrNoModifier = !lastKeyState.modifiers.cmd && !lastKeyState.modifiers.ctrl

      if (shiftOrNoModifier) {
        return 1
      }
    }
  }

  return 0
}
