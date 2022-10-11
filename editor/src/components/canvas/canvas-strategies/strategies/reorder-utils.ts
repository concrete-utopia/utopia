import * as EP from '../../../../core/shared/element-path'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { CanvasVector, offsetPoint, rectContainsPoint } from '../../../../core/shared/math-utils'
import { absolute } from '../../../../utils/utils'
import { CSSCursor } from '../../canvas-types'
import { reorderElement } from '../../commands/reorder-element-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import {
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
  StrategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'

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
  interactionSession: InteractionSession,
  customStrategyState: CustomStrategyState,
  isValidTarget: (path: ElementPath, metadata: ElementInstanceMetadataMap) => boolean,
): StrategyApplicationResult {
  if (interactionSession.interactionData.type !== 'DRAG') {
    return emptyStrategyApplicationResult
  }

  if (interactionSession.interactionData.drag != null) {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    const target = selectedElements[0]

    const siblings = MetadataUtils.getSiblings(canvasState.startingMetadata, target).map(
      (element) => element.elementPath,
    )

    if (!isReorderAllowed(siblings)) {
      return strategyApplicationResult(
        [setCursorCommand('mid-interaction', CSSCursor.NotPermitted)],
        {},
        'failure',
      )
    }

    const pointOnCanvas = offsetPoint(
      interactionSession.interactionData.dragStart,
      interactionSession.interactionData.drag,
    )

    const unpatchedIndex = siblings.findIndex((sibling) => EP.pathsEqual(sibling, target))
    const lastReorderIdx = customStrategyState.lastReorderIdx ?? unpatchedIndex

    const newIndex = findSiblingIndexUnderPoint(
      canvasState.startingMetadata,
      siblings,
      pointOnCanvas,
      isValidTarget,
    )

    const newIndexFound = newIndex > -1
    const newResultOrLastIndex = newIndexFound ? newIndex : lastReorderIdx

    if (newResultOrLastIndex === unpatchedIndex) {
      return strategyApplicationResult(
        [
          updateHighlightedViews('mid-interaction', []),
          setElementsToRerenderCommand(siblings),
          setCursorCommand('mid-interaction', CSSCursor.Move),
        ],
        {
          lastReorderIdx: newResultOrLastIndex,
        },
      )
    } else {
      return strategyApplicationResult(
        [
          reorderElement('always', target, absolute(newResultOrLastIndex)),
          setElementsToRerenderCommand(siblings),
          updateHighlightedViews('mid-interaction', []),
          setCursorCommand('mid-interaction', CSSCursor.Move),
        ],
        {
          lastReorderIdx: newResultOrLastIndex,
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
  siblings: Array<ElementPath>,
  point: CanvasVector,
  isValidTarget: (path: ElementPath, metadata: ElementInstanceMetadataMap) => boolean,
): number {
  return siblings.findIndex((sibling) => {
    const frame = MetadataUtils.getFrameInCanvasCoords(sibling, metadata)
    return frame != null && rectContainsPoint(frame, point) && isValidTarget(sibling, metadata)
  })
}
