import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { foldEither } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  emptyComments,
  jsxAttributeValue,
  JSXElement,
} from '../../../core/shared/element-template'
import { setJSXValuesAtPaths, ValueAtPath } from '../../../core/shared/jsx-attributes'
import {
  canvasPoint,
  CanvasPoint,
  CanvasRectangle,
  magnitude,
  rectContainsPoint,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { CanvasStrategy } from '../../../interactions_proposal'
import {
  EditorState,
  forUnderlyingTargetFromEditorState,
  modifyUnderlyingForOpenFile,
  TransientFilesState,
} from '../../editor/store/editor-state'
import {
  applyValuesAtPath,
  setDragMinimumExceededCommand,
  wildcardPatch,
} from '../commands/commands'
import {
  CanvasStrategyUpdateFnResult,
  FlexAlignControlRectProps,
  SelectModeCanvasSessionProps,
  SelectModeCanvasSessionState,
} from './canvas-strategy-types'

// FIXME: Reimplement with new interface.
/*
export const flexAlignParentStrategy: CanvasStrategy = {
  name: "Change Parent's Flex Align and Justify",
  fitnessFn: (editor, currentSession) => {
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
    return null // not fit
  },
  updateFn: (
    editorState: EditorState,
    sessionProps: SelectModeCanvasSessionProps,
    sessionState: SelectModeCanvasSessionState,
  ): CanvasStrategyUpdateFnResult => {
    // only apply after a certain treshold IF we hadn't already passed that treshold once
    const draggedElement = editorState.selectedViews[0]

    if (
      !sessionState.dragDeltaMinimumPassed &&
      magnitude(sessionProps.drag ?? canvasPoint({ x: 0, y: 0 })) < 15
    ) {
      return wildcardPatch('transient', {
        canvas: {
          controls: {
            animatedPlaceholderTargetUids: {
              $set: [EP.toUid(draggedElement)],
            },
          },
        },
      })
    }

    const targetParent = MetadataUtils.getParent(editorState.jsxMetadata, draggedElement)
    const indicatorBoxes = calcualteFlexAlignIndicatorBoxes(
      targetParent,
      sessionProps.mousePosition,
    )

    // if any indicator box is highlighted, we also want to change the parent's style too
    const higlightedIndicator = indicatorBoxes.find((b) => b.highlighted === true)
    if (higlightedIndicator == null || targetParent == null) {
      return [
        setDragMinimumExceededCommand('transient'),
        wildcardPatch('transient', {
          canvas: {
            controls: {
              flexAlignDropTargets: { $set: indicatorBoxes },
              animatedPlaceholderTargetUids: {
                $set: [EP.toUid(draggedElement)],
              },
            },
          },
        }),
      ]
    } else {
      const flexPropToChange: AssociatedFlexProp = higlightedIndicator.associatedFlexProp

      // Change Parent Props
      const parentPropsToUpdate: Array<ValueAtPath> = [
        ...Object.entries(flexPropToChange).map(([key, value]) => ({
          path: PP.create(['style', key]),
          value: jsxAttributeValue(value, emptyComments),
        })),
      ]

      const {
        editorStateWithChanges: editorStateAfterParent,
        editorStatePatch: editorStatePatchAfterParent,
      } = applyValuesAtPath(editorState, targetParent.elementPath, parentPropsToUpdate)

      // Make child invisible
      const childOpacity0: Array<ValueAtPath> = [
        { path: PP.create(['style', 'opacity']), value: jsxAttributeValue(0, emptyComments) },
      ]

      const { editorStatePatch: editorStatePatchAfterChild } = applyValuesAtPath(
        editorStateAfterParent,
        draggedElement,
        childOpacity0,
      )

      return [
        setDragMinimumExceededCommand('transient'),
        wildcardPatch('permanent', editorStatePatchAfterParent),
        wildcardPatch('transient', editorStatePatchAfterChild),
        wildcardPatch('transient', {
          canvas: {
            controls: {
              flexAlignDropTargets: { $set: indicatorBoxes },
              animatedPlaceholderTargetUids: {
                $set: [EP.toUid(draggedElement)],
              },
            },
          },
        }),
      ]
    }
  },
}

interface AssociatedFlexProp {
  justifyContent?: React.CSSProperties['justifyContent']
  alignItems?: React.CSSProperties['alignItems']
}

function flexIndicatorBox(
  mousePosition: CanvasPoint,
  canvasFrame: CanvasRectangle,
  associatedFlexProp: AssociatedFlexProp,
): FlexAlignControlRectProps {
  const mouseInRect = rectContainsPoint(canvasFrame, mousePosition)
  return {
    x: canvasFrame.x,
    y: canvasFrame.y,
    width: canvasFrame.width,
    height: canvasFrame.height,
    highlighted: mouseInRect,
    associatedFlexProp: associatedFlexProp,
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
    flexIndicatorBox(
      mousePosition,
      {
        x: targetMetadata.globalFrame.x,
        y: targetMetadata.globalFrame.y,
        width: targetMetadata.globalFrame.width,
        height: BoxHeight,
      } as CanvasRectangle,
      { alignItems: 'flex-start' },
    ),
    flexIndicatorBox(
      mousePosition,
      {
        x: targetMetadata.globalFrame?.x,
        y: targetMetadata.globalFrame?.y + targetMetadata.globalFrame.height - BoxHeight,
        width: targetMetadata.globalFrame?.width,
        height: BoxHeight,
      } as CanvasRectangle,
      { alignItems: 'flex-end' },
    ),
  ]
}
*/
