import { flexDirectionToFlexForwardsOrBackwards } from '../../../../core/layout/layout-utils'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import { KeyCharacter } from '../../../../utils/keyboard'
import { absolute } from '../../../../utils/utils'
import { reorderElement } from '../../commands/reorder-element-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import {
  CanvasStrategy,
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { getOptionalDisplayPropCommandsForFlow } from './flow-reorder-helpers'
import { isReorderAllowed } from './reorder-utils'
import { accumulatePresses } from './shared-keyboard-strategy-helpers'

export function keyboardReorderStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): CanvasStrategy | null {
  if (interactionSession?.activeControl.type !== 'KEYBOARD_CATCHER_CONTROL') {
    return null
  }

  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }
  const target = selectedElements[0]
  const siblings = MetadataUtils.getSiblings(canvasState.startingMetadata, target).map(
    (element) => element.elementPath,
  )
  const elementMetadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    target,
  )
  const isFlexOrFlow =
    MetadataUtils.isParentYogaLayoutedContainerForElementAndElementParticipatesInLayout(
      elementMetadata,
    ) || MetadataUtils.isPositionedByFlow(elementMetadata)

  if (siblings.length <= 1 || !isReorderAllowed(siblings) || !isFlexOrFlow) {
    return null
  }

  return {
    id: 'KEYBOARD_REORDER',
    name: 'Reorder',
    controlsToRender: [],
    fitness: 1,
    apply: () => {
      if (interactionSession != null && interactionSession.interactionData.type === 'KEYBOARD') {
        const accumulatedPresses = accumulatePresses(interactionSession.interactionData.keyStates)
        let keyboardResult: number = 0
        accumulatedPresses.forEach((accumulatedPress) => {
          accumulatedPress.keysPressed.forEach((key) => {
            const keyPressIndexChange = getIndexChangeDeltaFromKey(
              key,
              accumulatedPress.count,
              elementMetadata,
            )
            keyboardResult = keyboardResult + keyPressIndexChange
          })
        })

        const unpatchedIndex = siblings.findIndex((sibling) => EP.pathsEqual(sibling, target))
        const result = unpatchedIndex + keyboardResult
        const newIndex = Math.min(Math.max(0, result), siblings.length - 1)

        if (newIndex === unpatchedIndex) {
          return strategyApplicationResult(
            [updateHighlightedViews('mid-interaction', []), setElementsToRerenderCommand(siblings)],
            {
              lastReorderIdx: newIndex,
            },
          )
        } else {
          return strategyApplicationResult(
            [
              reorderElement('always', target, absolute(newIndex)),
              setElementsToRerenderCommand(siblings),
              updateHighlightedViews('mid-interaction', []),
              ...getOptionalDisplayPropCommandsForFlow(
                newIndex,
                canvasState.interactionTarget,
                canvasState.startingMetadata,
              ),
            ],
            {
              lastReorderIdx: newIndex,
            },
          )
        }
      } else {
        return emptyStrategyApplicationResult
      }
    },
  }
}

// This function creates the map which describes which keyboard cursor button should move the index which way.
// In standard layouts keyboard up and left moves backward and keyboard down and right moves forward.
// In flex reverse layouts this is fully reversed: keyboard up and left moves forward and keyboard down and right moves backward.
// If you have rtl text direction on top of any layouts, that should switch the effect of the left and right keys (but leave up and down as it is)
function getIndexChangesForArrowKeys(element: ElementInstanceMetadata | null): {
  [key: string]: number
} {
  const textDirection = element?.specialSizeMeasurements.parentTextDirection ?? 'ltr'

  const deltasForKeypresses = (() => {
    if (
      MetadataUtils.isParentYogaLayoutedContainerForElementAndElementParticipatesInLayout(element)
    ) {
      const flexDirection = element?.specialSizeMeasurements.parentFlexDirection ?? null
      const forwardsOrBackwards = flexDirectionToFlexForwardsOrBackwards(flexDirection)

      // when flex is reversed we need to move the opposite way in the indexes as in the screen
      if (forwardsOrBackwards === 'reverse') {
        return {
          left: 1,
          up: 1,
          right: -1,
          down: -1,
        }
      }
    }
    return {
      left: -1,
      up: -1,
      right: 1,
      down: 1,
    }
  })()

  // when text direction is ltr, up and left keys should go backwards, down and right keys should go forward
  // when text direction is rtl, up and right keys should go backwards, down and left keys should go forward
  const deltasForKeypressesWithTextDirection =
    textDirection === 'ltr'
      ? deltasForKeypresses
      : {
          ...deltasForKeypresses,
          left: deltasForKeypresses['right'],
          right: deltasForKeypresses['left'],
        }

  return deltasForKeypressesWithTextDirection
}

function getIndexChangeDeltaFromKey(
  key: KeyCharacter,
  delta: number,
  element: ElementInstanceMetadata | null,
): number {
  const indexChanges = getIndexChangesForArrowKeys(element)
  return delta * (indexChanges[key] ?? 0)
}
