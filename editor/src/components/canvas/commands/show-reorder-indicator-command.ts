import {
  flexDirectionToFlexForwardsOrBackwards,
  flexDirectionToSimpleFlexDirection,
} from '../../../core/layout/layout-utils'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { canvasRectangle, CanvasRectangle, zeroCanvasRect } from '../../../core/shared/math-utils'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import {
  getSiblingMidPointPosition,
  siblingAndPseudoPositions,
} from '../canvas-strategies/reparent-strategy-helpers'
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
  const FlexReparentIndicatorSize = 1 / editor.canvas.scale

  const targetParent = MetadataUtils.findElementByElementPath(editor.jsxMetadata, command.target)
  const parentFrame = MetadataUtils.getFrameInCanvasCoords(command.target, editor.jsxMetadata)
  if (
    targetParent == null ||
    parentFrame == null ||
    targetParent.specialSizeMeasurements.display !== 'flex'
  ) {
    return { editorStatePatches: [], commandDescription: `` }
  }

  const flexDirection = MetadataUtils.getFlexDirection(targetParent)
  const newParentFlexDirection = forceNotNull(
    'Should have a valid flex direction.',
    flexDirectionToSimpleFlexDirection(flexDirection),
  )
  const forwardsOrBackwards = forceNotNull(
    'Should have a valid flex orientation.',
    flexDirectionToFlexForwardsOrBackwards(flexDirection),
  )
  const siblings = MetadataUtils.getChildren(editor.jsxMetadata, command.target).map(
    (sibling) => sibling.elementPath,
  )

  const siblingPositions: Array<CanvasRectangle> = siblingAndPseudoPositions(
    newParentFlexDirection,
    forwardsOrBackwards,
    parentFrame,
    siblings,
    editor.jsxMetadata,
  )

  if (siblings.length > 0) {
    const precedingSiblingPosition: CanvasRectangle = siblingPositions[command.index]
    const succeedingSiblingPosition: CanvasRectangle = siblingPositions[command.index + 1]

    const targetLineBeforeSibling: CanvasRectangle =
      newParentFlexDirection === 'row'
        ? canvasRectangle({
            x:
              getSiblingMidPointPosition(
                precedingSiblingPosition,
                succeedingSiblingPosition,
                'row',
              ) -
              FlexReparentIndicatorSize / 2,
            y: (precedingSiblingPosition.y + succeedingSiblingPosition.y) / 2,
            height: (precedingSiblingPosition.height + succeedingSiblingPosition.height) / 2,
            width: 0,
          })
        : canvasRectangle({
            x: (precedingSiblingPosition.x + succeedingSiblingPosition.x) / 2,
            y:
              getSiblingMidPointPosition(
                precedingSiblingPosition,
                succeedingSiblingPosition,
                'column',
              ) -
              FlexReparentIndicatorSize / 2,
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
      commandDescription: `Show Outline Highlight`,
    }
  } else {
    const targetLineBeginningOfParent: CanvasRectangle =
      newParentFlexDirection === 'row'
        ? canvasRectangle({
            x: parentFrame.x,
            y: parentFrame.y,
            height: parentFrame.height,
            width: 0,
          })
        : canvasRectangle({
            x: parentFrame.x,
            y: parentFrame.y,
            width: parentFrame.width,
            height: 0,
          })
    const editorStatePatch: EditorStatePatch = {
      canvas: {
        controls: {
          flexReparentTargetLines: { $set: [targetLineBeginningOfParent] },
        },
      },
    }
    return {
      editorStatePatches: [editorStatePatch],
      commandDescription: `Show Outline Highlight`,
    }
  }
}
