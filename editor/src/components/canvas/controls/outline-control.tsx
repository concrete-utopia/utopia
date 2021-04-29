import * as React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  ElementInstanceMetadataMap,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import { Imports, TemplatePath } from '../../../core/shared/project-file-types'
import Utils from '../../../utils/utils'
import * as TP from '../../../core/shared/template-path'
import { ControlProps } from './new-canvas-controls'
import { Outline } from './outline'
import { anyInstanceYogaLayouted } from './select-mode/yoga-utils'
import { MarginControls } from './margin-controls'
import { PaddingControls } from './padding-controls'
import { MoveDragState, ResizeDragState, DragState } from '../canvas-types'
import { CanvasRectangle, offsetRect } from '../../../core/shared/math-utils'
import { fastForEach } from '../../../core/shared/utils'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { colorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { getJSXComponentsAndImportsForPathInnerComponentFromState } from '../../editor/store/editor-state'

export function getSelectionColor(
  path: TemplatePath,
  rootElements: Array<UtopiaJSXComponent>,
  metadata: ElementInstanceMetadataMap,
  imports: Imports,
  focusedElementPath: TemplatePath | null,
): string {
  if (TP.isInsideFocusedComponent(path)) {
    if (MetadataUtils.isFocusableComponent(path, rootElements, metadata, imports)) {
      return colorTheme.canvasSelectionFocusableChild.value
    } else {
      return colorTheme.canvasSelectionNotFocusableChild.value
    }
  } else if (TP.isFocused(focusedElementPath, path)) {
    return colorTheme.canvasSelectionIsolatedComponent.value
  } else if (MetadataUtils.isFocusableComponent(path, rootElements, metadata, imports)) {
    return colorTheme.canvasSelectionFocusable.value
  } else {
    return colorTheme.CanvasSelectionNotFocusable.value
  }
}

export interface OutlineControlsProps extends ControlProps {
  dragState: MoveDragState | ResizeDragState | null
}

function isDraggingToMove(
  dragState: MoveDragState | ResizeDragState | null,
  target: TemplatePath,
): dragState is MoveDragState {
  // This is a bit of a cheeky cast because we only cast if the thing is target is one of the dragged elements
  const targetIsDragged = TP.containsPath(target, dragState?.draggedElements ?? [])
  return dragState != null && dragState?.type === 'MOVE_DRAG_STATE' && targetIsDragged
}

export const OutlineControls = (props: OutlineControlsProps) => {
  const { dragState } = props
  const getDragStateFrame = React.useCallback(
    (target: TemplatePath): CanvasRectangle | null => {
      if (isDraggingToMove(dragState, target)) {
        const startingFrameAndTarget = dragState.originalFrames.find((frameAndTarget) =>
          TP.pathsEqual(frameAndTarget.target, target),
        )
        if (
          startingFrameAndTarget == null ||
          startingFrameAndTarget.frame == null ||
          dragState.drag == null
        ) {
          return null
        } else {
          return offsetRect(startingFrameAndTarget.frame, dragState.drag)
        }
      } else {
        return null
      }
    },
    [dragState],
  )

  const getTargetFrame = React.useCallback(
    (target: TemplatePath): CanvasRectangle | null => {
      const dragRect = getDragStateFrame(target)
      return dragRect ?? MetadataUtils.getFrameInCanvasCoords(target, props.componentMetadata)
    },
    [getDragStateFrame, props.componentMetadata],
  )

  const getOverlayControls = React.useCallback(
    (targets: TemplatePath[]): Array<JSX.Element> => {
      if (
        isFeatureEnabled('Dragging Shows Overlay') &&
        props.dragState != null &&
        props.dragState?.type === 'MOVE_DRAG_STATE' &&
        props.dragState.drag != null
      ) {
        let result: Array<JSX.Element> = []
        fastForEach(targets, (target) => {
          const rect = MetadataUtils.getFrameInCanvasCoords(target, props.componentMetadata)
          if (rect != null) {
            result.push(
              <div
                key={`${TP.toComponentId(target)}-overlay`}
                style={{
                  position: 'absolute',
                  boxSizing: 'border-box',
                  left: props.canvasOffset.x + rect.x,
                  top: props.canvasOffset.y + rect.y,
                  width: rect.width,
                  height: rect.height,
                  pointerEvents: 'none',
                  backgroundColor: '#FFADAD80',
                }}
              />,
            )
          }
        })

        return result
      } else {
        return []
      }
    },
    [props.canvasOffset.x, props.canvasOffset.y, props.componentMetadata, props.dragState],
  )

  const anySelectedElementIsYogaLayouted = anyInstanceYogaLayouted(
    props.componentMetadata,
    props.selectedViews,
  )

  let selectionOutlines: Array<JSX.Element> = getOverlayControls(props.selectedViews)
  const targetPaths =
    props.dragState != null ? props.dragState.draggedElements : props.selectedViews

  const selectionColors = useEditorState((store) => {
    return targetPaths.map((path) => {
      const result = getJSXComponentsAndImportsForPathInnerComponentFromState(
        path,
        store.editor,
        store.derived,
      )
      return getSelectionColor(
        path,
        result.components,
        store.editor.jsxMetadata,
        result.imports,
        store.editor.focusedElementPath,
      )
    })
  }, 'OutlineControls')

  fastForEach(targetPaths, (selectedView, index) => {
    const rect = getTargetFrame(selectedView)
    if (rect == null) {
      // early return as we can't draw a selection outline
      return
    }

    const keyPrefix = TP.toComponentId(selectedView)
    const instance = MetadataUtils.findElementByTemplatePath(props.componentMetadata, selectedView)
    const createsYogaLayout = MetadataUtils.isFlexLayoutedContainer(instance)
    const selectionColor = selectionColors[index]

    if (props.dragState == null) {
      const margin = MetadataUtils.getElementMargin(selectedView, props.componentMetadata)
      selectionOutlines.push(
        <MarginControls
          key={`${keyPrefix}-margin-controls`}
          canvasOffset={props.canvasOffset}
          scale={props.scale}
          margin={margin}
          frame={rect}
        />,
      )
      const padding = MetadataUtils.getElementPadding(selectedView, props.componentMetadata)
      selectionOutlines.push(
        <PaddingControls
          key={`${keyPrefix}-padding-controls`}
          canvasOffset={props.canvasOffset}
          scale={props.scale}
          padding={padding}
          frame={rect}
        />,
      )
    }

    // FIXME the striped overlay needs to be separated from this
    selectionOutlines.push(
      <Outline
        key={`${keyPrefix}-outline-control`}
        rect={rect}
        offset={props.canvasOffset}
        scale={props.scale}
        color={selectionColor}
        striped={createsYogaLayout}
        stripedColor={colorTheme.canvasSelectionAlternateOutlineYogaParent.shade(50).value}
      />,
    )
  })
  let multiSelectOutline: JSX.Element | undefined
  if (targetPaths.length > 1 && TP.areAllElementsInSameScene(targetPaths)) {
    const globalFrames = targetPaths.map((selectedView) => getTargetFrame(selectedView))
    const boundingBox = Utils.boundingRectangleArray(globalFrames)
    if (boundingBox != null) {
      const outlineColor = colorTheme.canvasSelectionSecondaryOutline.value
      multiSelectOutline = (
        <Outline
          rect={boundingBox}
          offset={props.canvasOffset}
          scale={props.scale}
          color={outlineColor}
        />
      )
    }
  }
  return (
    <>
      {...selectionOutlines}
      {multiSelectOutline}
    </>
  )
}
