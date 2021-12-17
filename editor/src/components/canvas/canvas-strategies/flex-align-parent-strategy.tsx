import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadata } from '../../../core/shared/element-template'
import {
  canvasPoint,
  CanvasPoint,
  CanvasRectangle,
  magnitude,
  rectContainsPoint,
} from '../../../core/shared/math-utils'
import { EditorState, TransientCanvasState } from '../../editor/store/editor-state'
import {
  CanvasStrategy,
  CanvasStrategyUpdateFn,
  FlexAlignControlRectProps,
  SelectModeCanvasSession,
} from '../canvas-types'

export const flexAlignParentStrategy: CanvasStrategy = {
  name: "Change Parent's Flex Align and Justify",
  fitnessFn: (editor, currentSession, previousTransientState) => {
    if (editor.selectedViews.length === 1) {
      const selectedView = editor.selectedViews[0]

      const isFlexLayouted = MetadataUtils.isParentYogaLayoutedContainerForElementAndElementParticipatesInLayout(
        MetadataUtils.findElementByElementPath(editor.jsxMetadata, selectedView),
      )
      const hasNoSiblings = MetadataUtils.getSiblings(editor.jsxMetadata, selectedView).length === 1

      if (isFlexLayouted && hasNoSiblings) {
        return 10 // fit!
      }
    }
    return 0 // not fit
  },
  updateFn: (
    editorState: EditorState,
    activeSession: SelectModeCanvasSession,
    previousTransientState: TransientCanvasState | null,
  ): TransientCanvasState => {
    // only apply after a certain treshold IF we hadn't already passed that treshold once
    if (
      !previousTransientState?.sessionStatePatch.dragDeltaMinimumPassed &&
      magnitude(activeSession.drag ?? canvasPoint({ x: 0, y: 0 })) < 15
    ) {
      return {
        highlightedViews: [],
        selectedViews: editorState.selectedViews,
        filesState: {},
        toastsToApply: [],
        sessionStatePatch: {},
        editorStatePatch: {},
      }
    }

    const draggedElement = editorState.selectedViews[0]
    const targetParent = MetadataUtils.getParent(editorState.jsxMetadata, draggedElement)
    const indicatorBoxes = calcualteFlexAlignIndicatorBoxes(
      targetParent,
      activeSession.mousePosition,
    )

    return {
      highlightedViews: [],
      selectedViews: editorState.selectedViews,
      filesState: {},
      toastsToApply: [],
      sessionStatePatch: {
        dragDeltaMinimumPassed: true,
      },
      editorStatePatch: {
        canvas: {
          controls: {
            flexAlignDropTargets: indicatorBoxes,
          },
        },
      },
    }
  },
}

function flexIndicatorBox(
  mousePosition: CanvasPoint,
  canvasFrame: CanvasRectangle,
): FlexAlignControlRectProps {
  const mouseInRect = rectContainsPoint(canvasFrame, mousePosition)
  return {
    x: canvasFrame.x,
    y: canvasFrame.y,
    width: canvasFrame.width,
    height: canvasFrame.height,
    highlighted: mouseInRect,
  }
}

function calcualteFlexAlignIndicatorBoxes(
  targetMetadata: ElementInstanceMetadata | null,
  mousePosition: CanvasPoint,
): Array<FlexAlignControlRectProps> {
  const BoxHeight = 20

  if (targetMetadata?.globalFrame == null) {
    return []
  }

  return [
    flexIndicatorBox(mousePosition, {
      x: targetMetadata.globalFrame.x,
      y: targetMetadata.globalFrame.y,
      width: targetMetadata.globalFrame.width,
      height: BoxHeight,
    } as CanvasRectangle),
    flexIndicatorBox(mousePosition, {
      x: targetMetadata.globalFrame?.x,
      y: targetMetadata.globalFrame?.y + targetMetadata.globalFrame.height - BoxHeight,
      width: targetMetadata.globalFrame?.width,
      height: BoxHeight,
    } as CanvasRectangle),
  ]
}
