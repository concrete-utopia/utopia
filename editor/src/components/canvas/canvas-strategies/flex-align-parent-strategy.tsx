import React from 'react'
import * as PP from '../../../core/shared/property-path'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { foldEither } from '../../../core/shared/either'
import {
  ElementInstanceMetadata,
  emptyComments,
  jsxAttributeValue,
  JSXElement,
} from '../../../core/shared/element-template'
import { setJSXValueAtPath, setJSXValuesAtPaths } from '../../../core/shared/jsx-attributes'
import {
  canvasPoint,
  CanvasPoint,
  CanvasRectangle,
  magnitude,
  rectContainsPoint,
} from '../../../core/shared/math-utils'
import {
  EditorState,
  forUnderlyingTargetFromEditorState,
  modifyUnderlyingForOpenFile,
  TransientCanvasState,
  TransientFilesState,
} from '../../editor/store/editor-state'
import {
  CanvasStrategy,
  CanvasStrategyUpdateFn,
  FlexAlignControlRectProps,
  SelectModeCanvasSession,
} from '../canvas-types'
import { objectMap } from '../../../core/shared/object-utils'

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

    // if any indicator box is highlighted, we also want to change the parent's style too
    const higlightedIndicator = indicatorBoxes.filter((b) => b.highlighted === true)[0]
    if (higlightedIndicator == null || targetParent == null) {
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
    } else {
      const flexPropToChange: AssociatedFlexProp = higlightedIndicator.associatedFlexProp

      let workingEditorState = { ...editorState }
      let transientFilesState: TransientFilesState = {}

      workingEditorState = modifyUnderlyingForOpenFile(
        targetParent.elementPath,
        editorState,
        (element: JSXElement) => {
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
            setJSXValuesAtPaths(
              element.props,
              Object.entries(flexPropToChange).map(([key, value]) => ({
                path: PP.create(['style', key]),
                value: jsxAttributeValue(value, emptyComments),
              })),
            ),
          )
        },
      )

      forUnderlyingTargetFromEditorState(
        targetParent.elementPath,
        workingEditorState,
        (success, underlyingElement, underlyingTarget, underlyingFilePath) => {
          transientFilesState[underlyingFilePath] = {
            topLevelElementsIncludingScenes: success.topLevelElements,
            imports: success.imports,
          }
          return success
        },
      )

      return {
        highlightedViews: [],
        selectedViews: editorState.selectedViews,
        filesState: transientFilesState,
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
    }
  },
}

type AssociatedFlexProp = {
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
