import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  ElementInstanceMetadataMap,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import { Imports, ElementPath } from '../../../core/shared/project-file-types'
import Utils from '../../../utils/utils'
import * as EP from '../../../core/shared/element-path'
import { ControlProps } from './new-canvas-controls'
import { Outline } from './outline'
import { anyInstanceYogaLayouted } from './select-mode/yoga-utils'
import { MarginControls } from './margin-controls'
import { PaddingControls } from './padding-controls'
import { MoveDragState, ResizeDragState, DragState } from '../canvas-types'
import { CanvasRectangle, offsetRect } from '../../../core/shared/math-utils'
import { fastForEach } from '../../../core/shared/utils'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { useColorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { KeysPressed } from '../../../utils/keyboard'
import { PositionOutline } from './position-outline'
import { stripNulls, uniqBy } from '../../../core/shared/array-utils'

export function getSelectionColor(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
  focusedElementPath: ElementPath | null,
  colorTheme: any,
): string {
  if (EP.isInsideFocusedComponent(path)) {
    if (MetadataUtils.isFocusableComponent(path, metadata)) {
      return colorTheme.canvasSelectionFocusableChild.value
    } else {
      return colorTheme.canvasSelectionNotFocusableChild.value
    }
  } else if (EP.isFocused(focusedElementPath, path)) {
    return colorTheme.canvasSelectionIsolatedComponent.value
  } else if (MetadataUtils.isFocusableComponent(path, metadata)) {
    return colorTheme.canvasSelectionFocusable.value
  } else {
    return colorTheme.canvasSelectionNotFocusable.value
  }
}

export interface OutlineControlsProps extends ControlProps {
  dragState: MoveDragState | ResizeDragState | null
  keysPressed: KeysPressed
}

function isDraggingToMove(
  dragState: MoveDragState | ResizeDragState | null,
  target: ElementPath,
): dragState is MoveDragState {
  // This is a bit of a cheeky cast because we only cast if the thing is target is one of the dragged elements
  const targetIsDragged = EP.containsPath(target, dragState?.draggedElements ?? [])
  return dragState != null && dragState?.type === 'MOVE_DRAG_STATE' && targetIsDragged
}

interface CenteredCrossSVGProps {
  id: string
  scale: number
  centerX: number
  centerY: number
}

export const CenteredCrossSVG = React.memo(
  ({ id, centerX, centerY, scale }: CenteredCrossSVGProps) => {
    const colorTheme = useColorTheme()
    return (
      <svg
        id={id}
        style={{
          left: centerX,
          top: centerY,
          position: 'absolute',
          width: 6,
          height: 6,
          transformOrigin: 'center center',
          transform: `translateX(-50%) translateY(-50%) scale(${1 / scale})`,
        }}
        width='4px'
        height='4px'
        viewBox='0 0 4 4'
        version='1.1'
      >
        <g
          stroke='none'
          strokeWidth='1'
          fill='none'
          fillRule='evenodd'
          strokeLinecap='round'
          strokeLinejoin='round'
        >
          <g id='cross_svg' stroke={colorTheme.primary.value}>
            <line x1='0.5' y1='0.5' x2='3.5' y2='3.5'></line>
            <line x1='0.5' y1='3.5' x2='3.5' y2='0.5'></line>
          </g>
        </g>
      </svg>
    )
  },
)

export const OutlineControls = (props: OutlineControlsProps) => {
  const colorTheme = useColorTheme()
  const { dragState } = props
  const layoutInspectorSectionHovered = useEditorState(
    (store) => store.editor.inspector.layoutSectionHovered,
    'OutlineControls layoutInspectorSectionHovered',
  )
  const getDragStateFrame = React.useCallback(
    (target: ElementPath): CanvasRectangle | null => {
      if (isDraggingToMove(dragState, target)) {
        const startingFrameAndTarget = dragState.originalFrames.find((frameAndTarget) =>
          EP.pathsEqual(frameAndTarget.target, target),
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
    (target: ElementPath): CanvasRectangle | null => {
      const dragRect = getDragStateFrame(target)
      return dragRect ?? MetadataUtils.getFrameInCanvasCoords(target, props.componentMetadata)
    },
    [getDragStateFrame, props.componentMetadata],
  )

  const parentHighlights: React.ReactNode = React.useMemo(() => {
    if (props.keysPressed['cmd'] || layoutInspectorSectionHovered) {
      return props.selectedViews.map((view) => {
        const parentPath = EP.parentPath(view)
        if (parentPath != null) {
          const parentFrame = MetadataUtils.getFrameInCanvasCoords(
            parentPath,
            props.componentMetadata,
          )
          if (parentFrame != null) {
            return (
              <>
                <div
                  style={{
                    position: 'absolute',
                    left: parentFrame.x + props.canvasOffset.x,
                    top: parentFrame.y + props.canvasOffset.y,
                    width: parentFrame.width,
                    height: parentFrame.height,
                    border: `#007aff`,
                  }}
                />

                <CenteredCrossSVG
                  id='parent-cross-top-left'
                  centerX={parentFrame.x + props.canvasOffset.x}
                  centerY={parentFrame.y + props.canvasOffset.y}
                  scale={props.scale}
                />

                <CenteredCrossSVG
                  id='parent-cross-top-right'
                  scale={props.scale}
                  centerX={parentFrame.x + parentFrame.width + props.canvasOffset.x}
                  centerY={parentFrame.y + props.canvasOffset.y}
                />
                <CenteredCrossSVG
                  id='parent-cross-bottom-right'
                  scale={props.scale}
                  centerX={parentFrame.x + parentFrame.width + props.canvasOffset.x}
                  centerY={parentFrame.y + parentFrame.height + props.canvasOffset.y}
                />
                <CenteredCrossSVG
                  id='parent-cross-bottom-left'
                  scale={props.scale}
                  centerX={parentFrame.x + props.canvasOffset.x}
                  centerY={parentFrame.y + parentFrame.height + props.canvasOffset.y}
                />
              </>
            )
          } else {
            return null
          }
        } else {
          return null
        }
      })
    } else {
      return null
    }
  }, [
    layoutInspectorSectionHovered,
    props.canvasOffset.x,
    props.canvasOffset.y,
    props.componentMetadata,
    props.keysPressed,
    props.selectedViews,
    props.scale,
  ])

  const parentOutlines: (JSX.Element | null)[] = React.useMemo(() => {
    const targetParents = uniqBy(
      stripNulls(props.selectedViews.map((view) => EP.parentPath(view))),
      EP.pathsEqual,
    )
    return targetParents.map((parentPath) => {
      const parentElement = MetadataUtils.findElementByElementPath(
        props.componentMetadata,
        parentPath,
      )
      const parentFrame = MetadataUtils.getFrameInCanvasCoords(parentPath, props.componentMetadata)
      if (
        MetadataUtils.isFlexLayoutedContainer(parentElement) ||
        MetadataUtils.isGridLayoutedContainer(parentElement)
      ) {
        if (parentFrame != null) {
          return (
            <div
              key={EP.toString(parentPath)}
              style={{
                position: 'absolute',
                left: parentFrame.x + props.canvasOffset.x,
                top: parentFrame.y + props.canvasOffset.y,
                width: parentFrame.width,
                height: parentFrame.height,
                outlineStyle: 'dotted',
                outlineColor: colorTheme.primary.value,
                outlineWidth: 1 / props.scale,
              }}
            />
          )
        } else {
          return null
        }
      } else {
        return null
      }
    })
  }, [
    colorTheme.primary.value,
    props.canvasOffset.x,
    props.canvasOffset.y,
    props.componentMetadata,
    props.scale,
    props.selectedViews,
  ])

  let selectionOutlines: Array<JSX.Element> = []
  const targetPaths =
    props.dragState != null ? props.dragState.draggedElements : props.selectedViews

  const selectionColors = useEditorState((store) => {
    return targetPaths.map((path) => {
      return getSelectionColor(
        path,
        store.editor.jsxMetadata,
        store.editor.focusedElementPath,
        colorTheme,
      )
    })
  }, 'OutlineControls')

  fastForEach(targetPaths, (selectedView, index) => {
    const rect = getTargetFrame(selectedView)
    if (rect == null) {
      // early return as we can't draw a selection outline
      return
    }

    const keyPrefix = EP.toComponentId(selectedView)
    const instance = MetadataUtils.findElementByElementPath(props.componentMetadata, selectedView)
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

    if (MetadataUtils.isPositionAbsolute(instance)) {
      selectionOutlines.push(
        <PositionOutline
          key={`${keyPrefix}-position-outline`}
          frame={rect}
          path={selectedView}
          scale={props.scale}
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
        stripedColor={colorTheme.selectionOutlines.value}
      />,
    )
  })
  let multiSelectOutline: JSX.Element | undefined
  if (targetPaths.length > 1 && EP.areAllElementsInSameInstance(targetPaths)) {
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
      {parentOutlines}
      {parentHighlights}
      {selectionOutlines}
      {multiSelectOutline}
    </>
  )
}
