import React from 'react'
import { LayoutTargetableProp } from '../../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import {
  boundingRectangleArray,
  CanvasPoint,
  windowPoint,
} from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { useColorTheme } from '../../../../uuiui'
import { EditorDispatch } from '../../../editor/action-types'
import { setResizeOptionsTargetOptions } from '../../../editor/actions/action-creators'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import {
  CSSCursor,
  DirectionHorizontal,
  DirectionVertical,
  EdgePosition,
  resizeDragState,
  updateResizeDragState,
} from '../../canvas-types'
import { getOriginalFrames } from '../../canvas-utils'
import { windowToCanvasCoordinatesGlobal } from '../../dom-lookup'
import { useBoundingBox } from '../bounding-box-hooks'

interface FlexResizeControlProps {
  localSelectedElements: Array<ElementPath>
}

export const FlexResizeControl = React.memo<FlexResizeControlProps>((props) => {
  const localSelectedElements = props.localSelectedElements
  const allSelectedElementsFlex = useEditorState((store) => {
    return (
      localSelectedElements.length > 0 &&
      localSelectedElements.every((path) => {
        return (
          MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path)
            ?.specialSizeMeasurements.parentLayoutSystem === 'flex'
        )
      })
    )
  }, 'FlexResizeControl allSelectedElementsFlex')

  const flexElements = allSelectedElementsFlex ? localSelectedElements : []

  const controlRef = useBoundingBox(flexElements, (ref, boundingBox) => {
    ref.current.style.left = boundingBox.x + 'px'
    ref.current.style.top = boundingBox.y + 'px'
    ref.current.style.width = boundingBox.width + 'px'
    ref.current.style.height = boundingBox.height + 'px'
  })

  const rightRef = useBoundingBox(flexElements, (ref, boundingBox) => {
    ref.current.style.left = boundingBox.width + 'px'
    ref.current.style.top = boundingBox.height / 2 + 'px'
  })

  const bottomRef = useBoundingBox(flexElements, (ref, boundingBox) => {
    ref.current.style.left = boundingBox.width / 2 + 'px'
    ref.current.style.top = boundingBox.height + 'px'
  })

  if (allSelectedElementsFlex) {
    return (
      <div
        ref={controlRef}
        style={{
          position: 'absolute',
          transform: `translate(var(--utopia-canvas-offset-x), var(--utopia-canvas-offset-y))`,
        }}
      >
        <ResizeEdge
          ref={rightRef}
          position={{ x: 1, y: 0.5 }}
          cursor={CSSCursor.ResizeEW}
          direction='vertical'
          enabledDirection={DirectionHorizontal}
        />
        <ResizeEdge
          ref={bottomRef}
          position={{ x: 0.5, y: 1 }}
          cursor={CSSCursor.ResizeNS}
          direction='horizontal'
          enabledDirection={DirectionVertical}
        />
      </div>
    )
  }
  return null
})

interface ResizeEdgeProps {
  cursor: CSSCursor
  position: EdgePosition
  direction: 'horizontal' | 'vertical'
  enabledDirection: EdgePosition
}

const ResizeEdgeMouseAreaSize = 12
const ResizeEdge = React.memo(
  React.forwardRef<HTMLDivElement, ResizeEdgeProps>((props, ref) => {
    const LineSVGComponent =
      props.position.y === 0.5 ? DimensionableControlVertical : DimensionableControlHorizontal
    const dispatch = useEditorState((store) => store.dispatch, 'ResizeEdge dispatch')
    const jsxMetadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
    const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)

    const onEdgeMouseDown = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>) => {
        startResizeInteraction(
          event,
          dispatch,
          props.position,
          props.enabledDirection,
          jsxMetadataRef.current,
          selectedViewsRef.current,
        )
      },
      [dispatch, props.position, props.enabledDirection, jsxMetadataRef, selectedViewsRef],
    )

    return (
      <div ref={ref} style={{ position: 'absolute' }} onMouseDown={onEdgeMouseDown}>
        <LineSVGComponent />
        <div
          style={{
            position: 'absolute',
            pointerEvents: 'initial',
            width: `calc(${ResizeEdgeMouseAreaSize}px / var(--utopia-canvas-scale))`,
            height: `calc(${ResizeEdgeMouseAreaSize}px / var(--utopia-canvas-scale))`,
            top: `calc(${-ResizeEdgeMouseAreaSize / 2}px / var(--utopia-canvas-scale))`,
            left: `calc(${-ResizeEdgeMouseAreaSize / 2}px / var(--utopia-canvas-scale))`,
            backgroundColor: 'transparent',
            cursor: props.cursor,
          }}
        />
      </div>
    )
  }),
)

function startResizeInteraction(
  event: React.MouseEvent<HTMLDivElement>,
  dispatch: EditorDispatch,
  position: EdgePosition,
  enabledDirection: EdgePosition,
  metadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
) {
  event.stopPropagation()
  if (event.buttons === 1) {
    // TODO update this to call createInteractionState to use canvas strategies
    const centerBasedResize = event.altKey
    const keepAspectRatio = event.shiftKey // || props.elementAspectRatioLocked ???
    const enableSnapping = !event.metaKey
    const canvasPositions = windowToCanvasCoordinatesGlobal(
      windowPoint({ x: event.nativeEvent.x, y: event.nativeEvent.y }),
    )
    const start: CanvasPoint = canvasPositions.canvasPositionRaw
    const originalFrames = getOriginalFrames(selectedViews, metadata)
    const isMultiSelect = selectedViews.length > 1

    const originalSize = boundingRectangleArray(
      selectedViews.map((path) => MetadataUtils.getFrameInCanvasCoords(path, metadata)),
    )
    const flexDirection = MetadataUtils.getFlexDirection(
      MetadataUtils.getParent(metadata, selectedViews[0]),
    )
    let possibleTargetProperty: LayoutTargetableProp | undefined = undefined
    if (position.x === 0.5) {
      possibleTargetProperty =
        flexDirection === 'column' || flexDirection === 'column-reverse' ? 'flexBasis' : 'height'
    } else if (position.y === 0.5) {
      possibleTargetProperty =
        flexDirection === 'row' || flexDirection === 'row-reverse' ? 'flexBasis' : 'width'
    }

    if (originalSize != null) {
      const newDragState = updateResizeDragState(
        resizeDragState(
          originalSize,
          originalFrames,
          position,
          enabledDirection,
          metadata,
          selectedViews,
          isMultiSelect,
          [],
        ),
        start,
        null,
        possibleTargetProperty,
        enableSnapping,
        centerBasedResize,
        keepAspectRatio,
      )

      dispatch(
        [
          CanvasActions.createDragState(newDragState),
          setResizeOptionsTargetOptions(
            possibleTargetProperty != null ? [possibleTargetProperty] : [],
            0,
          ),
        ],
        'canvas',
      )
    }
  }
}

const ControlSideShort = 3
const ControlSideLong = 15
const DimensionableControlVertical = React.memo(() => {
  const colorTheme = useColorTheme()
  const controlLength = ControlSideLong
  const controlWidth = ControlSideShort

  return (
    <div
      className='label-dimensionableControlVertical'
      style={{
        position: 'relative',
        backgroundColor: colorTheme.primary.shade(10).value,
        borderRadius: `calc(5px / var(--utopia-canvas-scale))`,
        boxShadow: `0px 0px 0px calc(0.3px / var(--utopia-canvas-scale)) ${colorTheme.primary.value}, 0px calc(1px / var(--utopia-canvas-scale)) calc(3px / var(--utopia-canvas-scale)) rgba(140,140,140,.9)`,
        height: `calc(${controlLength}px / var(--utopia-canvas-scale))`,
        width: `calc(${controlWidth}px / var(--utopia-canvas-scale))`,
        left: -1,
        top: `calc(${-controlLength / 2}px / var(--utopia-canvas-scale))`,
      }}
    />
  )
})

const DimensionableControlHorizontal = React.memo(() => {
  const colorTheme = useColorTheme()
  const controlLength = ControlSideShort
  const controlWidth = ControlSideLong

  return (
    <div
      className='label-dimensionableControlVertical'
      style={{
        position: 'relative',
        backgroundColor: colorTheme.primary.shade(10).value,
        borderRadius: `calc(5px / var(--utopia-canvas-scale))`,
        boxShadow: `0px 0px 0px calc(0.3px / var(--utopia-canvas-scale)) ${colorTheme.primary.value}, 0px calc(1px / var(--utopia-canvas-scale)) calc(3px / var(--utopia-canvas-scale)) rgba(140,140,140,.9)`,
        height: `calc(${controlLength}px / var(--utopia-canvas-scale))`,
        width: `calc(${controlWidth}px / var(--utopia-canvas-scale))`,
        left: `calc(${-controlWidth / 2}px / var(--utopia-canvas-scale))`,
        top: -1,
      }}
    />
  )
})
