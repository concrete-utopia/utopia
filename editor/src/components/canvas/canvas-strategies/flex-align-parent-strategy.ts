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
import {
  EditorState,
  forUnderlyingTargetFromEditorState,
  modifyUnderlyingForOpenFile,
  TransientFilesState,
} from '../../editor/store/editor-state'
import {
  CanvasStrategy,
  CanvasStrategyUpdateFnResult,
  FlexAlignControlRectProps,
  SelectModeCanvasSession,
  SelectModeCanvasSessionState,
} from './canvas-strategy-types'

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
    return 0 // not fit
  },
  updateFn: (
    lifecycle: 'transient' | 'final',
    editorState: EditorState,
    activeSession: SelectModeCanvasSession,
    sessionState: SelectModeCanvasSessionState,
  ): CanvasStrategyUpdateFnResult => {
    // only apply after a certain treshold IF we hadn't already passed that treshold once
    const draggedElement = editorState.selectedViews[0]

    if (
      !sessionState.dragDeltaMinimumPassed &&
      magnitude(activeSession.drag ?? canvasPoint({ x: 0, y: 0 })) < 15
    ) {
      return {
        newSessionState: sessionState,
        transientFilesState: {},
        editorStatePatch: {
          canvas: {
            controls: {
              animatedPlaceholderTargetUids: {
                $set: lifecycle === 'transient' ? [EP.toUid(draggedElement)] : [],
              },
            },
          },
        },
      }
    }

    const targetParent = MetadataUtils.getParent(editorState.jsxMetadata, draggedElement)
    const indicatorBoxes = calcualteFlexAlignIndicatorBoxes(
      targetParent,
      activeSession.mousePosition,
    )

    // if any indicator box is highlighted, we also want to change the parent's style too
    const higlightedIndicator = indicatorBoxes.filter((b) => b.highlighted === true)[0]
    if (higlightedIndicator == null || targetParent == null) {
      return {
        newSessionState: {
          ...sessionState,
          dragDeltaMinimumPassed: true,
        },
        transientFilesState: {},
        editorStatePatch: {
          canvas: {
            controls: {
              flexAlignDropTargets: { $set: indicatorBoxes },
              animatedPlaceholderTargetUids: {
                $set: lifecycle === 'transient' ? [EP.toUid(draggedElement)] : [],
              },
            },
          },
        },
      }
    } else {
      const flexPropToChange: AssociatedFlexProp = higlightedIndicator.associatedFlexProp

      let workingEditorState = { ...editorState }
      let transientFilesState: TransientFilesState = {}

      // Change Parent Props

      const parentPropsToUpdate: Array<ValueAtPath> = [
        ...Object.entries(flexPropToChange).map(([key, value]) => ({
          path: PP.create(['style', key]),
          value: jsxAttributeValue(value, emptyComments),
        })),
      ]

      const {
        editorState: editorStateAfterParent,
        transientFilesState: transientFilesStateAfterParent,
      } = applyValuesAtPath(
        workingEditorState,
        transientFilesState,
        targetParent.elementPath,
        parentPropsToUpdate,
      )

      // Make child invisible

      const childOpacity0: Array<ValueAtPath> =
        lifecycle === 'transient'
          ? [{ path: PP.create(['style', 'opacity']), value: jsxAttributeValue(0, emptyComments) }]
          : []

      const {
        editorState: editorStateAfterChild,
        transientFilesState: transientFilesStateAfterChild,
      } = applyValuesAtPath(
        editorStateAfterParent,
        transientFilesStateAfterParent,
        draggedElement,
        childOpacity0,
      )

      return {
        newSessionState: {
          ...sessionState,
          dragDeltaMinimumPassed: true,
        },
        transientFilesState: transientFilesStateAfterChild,
        editorStatePatch: {
          canvas: {
            controls: {
              flexAlignDropTargets: { $set: indicatorBoxes },
              animatedPlaceholderTargetUids: {
                $set: lifecycle === 'transient' ? [EP.toUid(draggedElement)] : [],
              },
            },
          },
        },
      }
    }
  },
}

type AssociatedFlexProp = {
  justifyContent?: React.CSSProperties['justifyContent']
  alignItems?: React.CSSProperties['alignItems']
}

function applyValuesAtPath(
  editorState: EditorState,
  filesState: TransientFilesState,
  target: ElementPath,
  jsxValuesAndPathsToSet: ValueAtPath[],
): { editorState: EditorState; transientFilesState: TransientFilesState } {
  let workingEditorState = { ...editorState }
  let transientFilesState = { ...filesState }

  workingEditorState = modifyUnderlyingForOpenFile(target, editorState, (element: JSXElement) => {
    return foldEither(
      () => {
        return element
      },
      (updatedProps) => {
        return {
          ...element,
          props: updatedProps,
        }
      },
      setJSXValuesAtPaths(element.props, jsxValuesAndPathsToSet),
    )
  })

  forUnderlyingTargetFromEditorState(
    target,
    workingEditorState,
    (success, underlyingElement, underlyingTarget, underlyingFilePath) => {
      transientFilesState[underlyingFilePath] = {
        topLevelElementsIncludingScenes: success.topLevelElements,
        imports: success.imports,
      }
      return success
    },
  )
  return { editorState: workingEditorState, transientFilesState: transientFilesState }
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
