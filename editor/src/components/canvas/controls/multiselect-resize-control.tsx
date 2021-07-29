import * as React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import Utils from '../../../utils/utils'
import { CanvasRectangle } from '../../../core/shared/math-utils'
import { OriginalCanvasAndLocalFrame } from '../../editor/store/editor-state'
import * as EP from '../../../core/shared/element-path'
import {
  EdgePosition,
  oppositeEdgePosition,
  oppositeEdgePositionPart,
  ResizeDragState,
} from '../canvas-types'
import { collectGuidelines, getOriginalFrames, pickPointOnRect } from '../canvas-utils'
import { GuidelineWithSnappingVector } from '../guideline'
import { GuidelineControl } from './guideline-control'
import { ControlProps } from './new-canvas-controls'
import { ResizeRectangle } from './size-box'
import { optionalMap } from '../../../core/shared/optional-utils'

interface MultiselectResizeProps extends ControlProps {
  dragState: ResizeDragState | null
}

interface SingleselectResizeProps extends MultiselectResizeProps {
  obtainOriginalFrames: () => OriginalCanvasAndLocalFrame[]
  onResizeStart: (originalSize: CanvasRectangle, draggedPoint: EdgePosition) => void
}

interface ConstraintsControlState {
  originalBoundingBox: CanvasRectangle
  guidelineStartPoint: EdgePosition | null
}

export class MultiselectResizeControl extends React.Component<
  MultiselectResizeProps,
  ConstraintsControlState
> {
  constructor(props: MultiselectResizeProps) {
    super(props)
    this.state = {
      originalBoundingBox: Utils.zeroRectangle as CanvasRectangle,
      guidelineStartPoint: null,
    }
  }

  obtainOriginalFrames = () =>
    getOriginalFrames(this.props.selectedViews, this.props.componentMetadata)

  onResizeStart = (originalSize: CanvasRectangle, draggedPoint: EdgePosition) => {
    this.setState({
      originalBoundingBox: originalSize,
      guidelineStartPoint: draggedPoint,
    })
  }

  getGuidelines = (
    hideGuidelines: boolean,
    boundingBox: CanvasRectangle,
    resizingFromPosition: EdgePosition | null,
    draggedElements: ElementPath[],
  ) => {
    if (hideGuidelines || this.state.guidelineStartPoint == null) {
      return []
    } else {
      return collectGuidelines(
        this.props.componentMetadata,
        draggedElements,
        this.props.scale,
        pickPointOnRect(boundingBox, this.state.guidelineStartPoint),
        resizingFromPosition,
      )
    }
  }

  render() {
    if (this.props.selectedViews.length === 0) {
      return null
    }
    let globalFrames: Array<CanvasRectangle> = []
    Utils.fastForEach(this.props.selectedViews, (selectedView) => {
      const frame = MetadataUtils.getFrameInCanvasCoords(selectedView, this.props.componentMetadata)
      if (frame != null) {
        globalFrames.push(frame)
      }
    })

    const boundingBox = Utils.boundingRectangleArray(globalFrames)
    if (boundingBox == null) {
      return null
    } else {
      const draggedElements =
        this.props.dragState != null && this.props.dragState.type === 'RESIZE_DRAG_STATE'
          ? this.props.dragState.draggedElements
          : []
      const draggedFrames = Utils.stripNulls(
        draggedElements.map((view) => {
          return MetadataUtils.getFrameInCanvasCoords(view, this.props.componentMetadata)
        }),
      )
      const draggedBoundingBox = Utils.boundingRectangleArray(draggedFrames)
      let guidelines: GuidelineWithSnappingVector[] = []

      if (draggedBoundingBox != null) {
        const hideGuidelines = this.props.cmdKeyPressed

        const oppositeStartCorner: EdgePosition | null = Utils.optionalMap((position) => {
          const flippedToStart = oppositeEdgePosition(position)
          // Need to account for dragging causing the controls to be flipped over.
          const flippedVertically =
            this.state.originalBoundingBox.x === draggedBoundingBox.x
              ? flippedToStart
              : {
                  ...flippedToStart,
                  x: oppositeEdgePositionPart(flippedToStart.x),
                }
          const flippedHorizontally =
            this.state.originalBoundingBox.y === draggedBoundingBox.y
              ? flippedVertically
              : {
                  ...flippedVertically,
                  y: oppositeEdgePositionPart(flippedVertically.y),
                }
          return flippedHorizontally
        }, this.state.guidelineStartPoint)
        guidelines = this.getGuidelines(
          hideGuidelines,
          draggedBoundingBox,
          oppositeStartCorner,
          draggedElements,
        )
      } else {
        guidelines = []
      }

      const guidelineElements = guidelines.map((g, index) => {
        return (
          <GuidelineControl
            key={`guideline-${index}`}
            guidelineWithSnapping={g}
            targetFrame={boundingBox}
            canvasOffset={this.props.canvasOffset}
            scale={this.props.scale}
          />
        )
      })

      if (
        this.props.selectedViews.length > 1 &&
        EP.areAllElementsInSameInstance(this.props.selectedViews)
      ) {
        return (
          <>
            <ResizeRectangle
              targetComponentMetadata={null}
              dispatch={this.props.dispatch}
              scale={this.props.scale}
              canvasOffset={this.props.canvasOffset}
              measureSize={boundingBox}
              visualSize={boundingBox}
              resizeStatus={this.props.resizeStatus}
              selectedViews={this.props.selectedViews}
              elementAspectRatioLocked={this.props.elementAspectRatioLocked}
              imageMultiplier={this.props.imageMultiplier}
              sideResizer={false}
              dragState={
                this.props.dragState != null && this.props.dragState.type === 'RESIZE_DRAG_STATE'
                  ? this.props.dragState
                  : null
              }
              windowToCanvasPosition={this.props.windowToCanvasPosition}
              getOriginalFrames={this.obtainOriginalFrames}
              metadata={this.props.componentMetadata}
              onResizeStart={this.onResizeStart}
              testID={'component-resize-control-0'}
              maybeClearHighlightsOnHoverEnd={this.props.maybeClearHighlightsOnHoverEnd}
              labels={
                {
                  vertical: 'Width',
                  horizontal: 'Height',
                } as const
              }
              resizeOptions={this.props.resizeOptions}
            />
            {guidelineElements}
          </>
        )
      } else {
        return (
          <>
            <SingleSelectResizeControls
              {...this.props}
              key={'single-select-resize-controls'}
              obtainOriginalFrames={this.obtainOriginalFrames}
              onResizeStart={this.onResizeStart}
            />
            {guidelineElements}
          </>
        )
      }
    }
  }
}

export class SingleSelectResizeControls extends React.Component<SingleselectResizeProps> {
  constructor(props: SingleselectResizeProps) {
    super(props)
  }

  render() {
    return this.props.selectedViews.map((view, index) => {
      const labels = {
        vertical: 'Width',
        horizontal: 'Height',
      } as const
      const target = MetadataUtils.findElementByElementPath(this.props.componentMetadata, view)
      const frame = optionalMap((metadata) => metadata.globalFrame, target)
      if (frame == null) {
        return null
      } else {
        return (
          <ResizeRectangle
            targetComponentMetadata={target}
            key={`single-select-resize-controls-rect-${index}`}
            dispatch={this.props.dispatch}
            scale={this.props.scale}
            canvasOffset={this.props.canvasOffset}
            measureSize={frame}
            visualSize={frame}
            resizeStatus={this.props.resizeStatus}
            selectedViews={[view]}
            elementAspectRatioLocked={this.props.elementAspectRatioLocked}
            imageMultiplier={this.props.imageMultiplier}
            sideResizer={false}
            dragState={
              this.props.dragState != null && this.props.dragState.type === 'RESIZE_DRAG_STATE'
                ? this.props.dragState
                : null
            }
            windowToCanvasPosition={this.props.windowToCanvasPosition}
            getOriginalFrames={this.props.obtainOriginalFrames}
            metadata={this.props.componentMetadata}
            onResizeStart={this.props.onResizeStart}
            testID={`component-resize-control-${EP.toComponentId(view)}-${index}`}
            maybeClearHighlightsOnHoverEnd={this.props.maybeClearHighlightsOnHoverEnd}
            labels={labels}
            resizeOptions={this.props.resizeOptions}
          />
        )
      }
    })
  }
}
