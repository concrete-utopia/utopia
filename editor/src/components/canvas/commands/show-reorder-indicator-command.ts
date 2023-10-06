import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { findLastIndex, reverse } from '../../../core/shared/array-utils'
import type { CanvasRectangle } from '../../../core/shared/math-utils'
import {
  canvasRectangle,
  zeroCanvasRect,
  zeroRectIfNullOrInfinity,
} from '../../../core/shared/math-utils'
import { forceNotNull } from '../../../core/shared/optional-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { singleAxisAutoLayoutContainerDirections } from '../canvas-strategies/strategies/flow-reorder-helpers'
import type { SiblingPosition } from '../canvas-strategies/strategies/reparent-helpers/reparent-strategy-sibling-position-helpers'
import {
  getSiblingMidPointPosition,
  siblingAndPseudoPositions,
} from '../canvas-strategies/strategies/reparent-helpers/reparent-strategy-sibling-position-helpers'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'

export interface ShowReorderIndicator extends BaseCommand {
  type: 'SHOW_REORDER_INDICATOR'
  target: ElementPath
  index: number
}

export function showReorderIndicator(target: ElementPath, index: number): ShowReorderIndicator {
  return {
    type: 'SHOW_REORDER_INDICATOR',
    whenToRun: 'mid-interaction',
    target: target,
    index: index,
  }
}

export const runShowReorderIndicator: CommandFunction<ShowReorderIndicator> = (
  editor: EditorState,
  command: ShowReorderIndicator,
) => {
  const targetParent = MetadataUtils.findElementByElementPath(editor.jsxMetadata, command.target)
  const parentFrame = MetadataUtils.getFrameInCanvasCoords(command.target, editor.jsxMetadata)
  if (targetParent == null || parentFrame == null) {
    return { editorStatePatches: [], commandDescription: `Show reorder indicator FAILED` }
  }

  const staticContainerDirection = singleAxisAutoLayoutContainerDirections(
    command.target,
    editor.jsxMetadata,
    editor.elementPathTree,
  )

  if (staticContainerDirection === 'non-single-axis-autolayout') {
    return {
      editorStatePatches: [],
      commandDescription: `Show reorder indicator FAILED: non 1d static parent`,
    }
  }

  const newParentDirection = forceNotNull(
    'Should have a valid flex direction.',
    staticContainerDirection.direction,
  )
  const forwardsOrBackwards = forceNotNull(
    'Should have a valid flex orientation.',
    staticContainerDirection.forwardOrReverse,
  )
  const siblings = MetadataUtils.getChildrenOrdered(
    editor.jsxMetadata,
    editor.elementPathTree,
    command.target,
  ).map((sibling) => sibling.elementPath)

  if (siblings.length === 0) {
    return {
      editorStatePatches: [],
      commandDescription: `Show Reorder Indicator without children`,
    }
  }

  const siblingPositions: Array<SiblingPosition> = siblingAndPseudoPositions(
    newParentDirection,
    forwardsOrBackwards,
    zeroRectIfNullOrInfinity(parentFrame),
    siblings,
    editor.jsxMetadata,
  )

  const closestPreviousSiblingPositionIndex = findLastIndex(
    (siblingPosition) => siblingPosition.index <= command.index,
    siblingPositions,
  )
  const closestPreviousSiblingPosition = forceNotNull(
    'no sibling found for reorder indicator',
    siblingPositions[closestPreviousSiblingPositionIndex],
  )
  const closestNextSiblingPosition = forceNotNull(
    'no sibling found for reorder indicator',
    siblingPositions.find((siblingPosition) => siblingPosition.index > command.index),
  )

  const precedingSiblingPosition: CanvasRectangle = closestPreviousSiblingPosition.frame
  const succeedingSiblingPosition: CanvasRectangle = closestNextSiblingPosition.frame

  const targetLineBeforeSibling: CanvasRectangle =
    newParentDirection === 'horizontal'
      ? canvasRectangle({
          x: getSiblingMidPointPosition(
            precedingSiblingPosition,
            succeedingSiblingPosition,
            'horizontal',
            forwardsOrBackwards,
          ),
          y: (precedingSiblingPosition.y + succeedingSiblingPosition.y) / 2,
          height: (precedingSiblingPosition.height + succeedingSiblingPosition.height) / 2,
          width: 0,
        })
      : canvasRectangle({
          x: (precedingSiblingPosition.x + succeedingSiblingPosition.x) / 2,
          y: getSiblingMidPointPosition(
            precedingSiblingPosition,
            succeedingSiblingPosition,
            'vertical',
            forwardsOrBackwards,
          ),
          width: (precedingSiblingPosition.width + succeedingSiblingPosition.width) / 2,
          height: 0,
        })

  const editorStatePatch: EditorStatePatch = {
    canvas: {
      controls: {
        flexReparentTargetLines: { $set: [targetLineBeforeSibling] },
      },
    },
  }
  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Show Reorder Indicator at index ${command.index}`,
  }
}
