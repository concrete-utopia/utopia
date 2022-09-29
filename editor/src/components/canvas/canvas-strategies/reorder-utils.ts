import * as EP from '../../../core/shared/element-path'
import { ElementPath } from '../../../core/shared/project-file-types'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { CanvasVector, offsetPoint, rectContainsPoint } from '../../../core/shared/math-utils'
import { absolute } from '../../../utils/utils'
import { CSSCursor } from '../canvas-types'
import { reorderElement } from '../commands/reorder-element-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import {
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
  StrategyApplicationResult,
} from './canvas-strategy-types'
import { InteractionSession, StrategyState } from './interaction-state'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'

export function isReorderAllowed(siblings: Array<ElementPath>) {
  return siblings.every((sibling) => !isRootOfGeneratedElement(sibling))
}

function isRootOfGeneratedElement(target: ElementPath): boolean {
  const uid = EP.toUid(target)
  const staticUid = EP.toStaticUid(target)
  return uid !== staticUid
}

export function applyReorderCommon(
  canvasState: InteractionCanvasState,
  interactionState: InteractionSession,
  strategyState: StrategyState,
  isValidTarget: (path: ElementPath, metadata: ElementInstanceMetadataMap) => boolean,
): StrategyApplicationResult {
  if (interactionState.interactionData.type !== 'DRAG') {
    return emptyStrategyApplicationResult
  }

  if (interactionState.interactionData.drag != null) {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    const target = selectedElements[0]

    const siblingsOfTarget = MetadataUtils.getSiblingsProjectContentsOrdered(
      strategyState.startingMetadata,
      target,
    ).map((element) => element.elementPath)

    if (!isReorderAllowed(siblingsOfTarget)) {
      return strategyApplicationResult(
        [setCursorCommand('mid-interaction', CSSCursor.NotPermitted)],
        {},
        'failure',
      )
    }

    const pointOnCanvas = offsetPoint(
      interactionState.interactionData.dragStart,
      interactionState.interactionData.drag,
    )

    const unpatchedIndex = siblingsOfTarget.findIndex((sibling) => EP.pathsEqual(sibling, target))
    const lastReorderIdx = strategyState.customStrategyState.lastReorderIdx ?? unpatchedIndex

    const { newIndex, targetSiblingUnderMouse } = findSiblingIndexUnderPoint(
      interactionState.latestMetadata,
      pointOnCanvas,
      target,
      isValidTarget,
    )

    const newIndexFound = newIndex > -1
    const mouseStillOverPreviousTargetSibling = EP.pathsEqual(
      targetSiblingUnderMouse,
      strategyState.customStrategyState.previousReorderTargetSiblingUnderMouse,
    )
    const newResultOrLastIndex =
      !mouseStillOverPreviousTargetSibling && newIndexFound ? newIndex : lastReorderIdx

    if (newResultOrLastIndex === unpatchedIndex) {
      return strategyApplicationResult(
        [
          updateHighlightedViews('mid-interaction', []),
          setElementsToRerenderCommand(siblingsOfTarget),
          setCursorCommand('mid-interaction', CSSCursor.Move),
        ],
        {
          lastReorderIdx: newResultOrLastIndex,
          previousReorderTargetSiblingUnderMouse: targetSiblingUnderMouse,
        },
      )
    } else {
      return strategyApplicationResult(
        [
          reorderElement('always', target, absolute(newResultOrLastIndex)),
          setElementsToRerenderCommand(siblingsOfTarget),
          updateHighlightedViews('mid-interaction', []),
          setCursorCommand('mid-interaction', CSSCursor.Move),
        ],
        {
          lastReorderIdx: newResultOrLastIndex,
          previousReorderTargetSiblingUnderMouse: targetSiblingUnderMouse,
        },
      )
    }
  } else {
    // Fallback for when the checks above are not satisfied.
    return strategyApplicationResult([setCursorCommand('mid-interaction', CSSCursor.Move)])
  }
}

function findSiblingIndexUnderPoint(
  metadata: ElementInstanceMetadataMap,
  point: CanvasVector,
  target: ElementPath,
  isValidTarget: (path: ElementPath, metadata: ElementInstanceMetadataMap) => boolean,
): {
  newIndex: number
  targetSiblingUnderMouse: ElementPath | null
} {
  const siblings = MetadataUtils.getSiblingsProjectContentsOrdered(metadata, target).map(
    (element) => element.elementPath,
  )
  const targetSiblingIdx = siblings.findIndex((sibling) => {
    const frame = MetadataUtils.getFrameInCanvasCoords(sibling, metadata)
    return (
      frame != null &&
      rectContainsPoint(frame, point) &&
      isValidTarget(sibling, metadata) &&
      !EP.pathsEqual(target, sibling)
    )
  })
  if (targetSiblingIdx === -1) {
    // We were unable to find an appropriate entry.
    return {
      newIndex: -1,
      targetSiblingUnderMouse: null,
    }
  } else {
    return {
      newIndex: targetSiblingIdx,
      targetSiblingUnderMouse: siblings[targetSiblingIdx],
    }
  }
}
