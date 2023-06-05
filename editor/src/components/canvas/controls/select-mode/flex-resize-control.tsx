import React from 'react'
import { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import { LayoutTargetableProp } from '../../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import {
  boundingRectangleArray,
  CanvasPoint,
  CanvasVector,
  nullIfInfinity,
  windowPoint,
} from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { useColorTheme } from '../../../../uuiui'
import { EditorDispatch } from '../../../editor/action-types'
import { setResizeOptionsTargetOptions } from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
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
import { windowToCanvasCoordinates } from '../../dom-lookup'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

interface FlexResizeControlProps {
  localSelectedElements: Array<ElementPath>
}

export const FlexResizeControl = React.memo<FlexResizeControlProps>((props) => {
  const localSelectedElements = props.localSelectedElements
  const allSelectedElementsFlex = useEditorState(
    Substores.metadata,
    (store) => {
      return (
        localSelectedElements.length > 0 &&
        localSelectedElements.every((path) => {
          return (
            MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path)
              ?.specialSizeMeasurements.parentLayoutSystem === 'flex'
          )
        })
      )
    },
    'FlexResizeControl allSelectedElementsFlex',
  )

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
      <CanvasOffsetWrapper>
        <div
          ref={controlRef}
          style={{
            position: 'absolute',
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
      </CanvasOffsetWrapper>
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
const ResizeEdgeMouseAreaOffset = ResizeEdgeMouseAreaSize / 2
const ResizeEdge = React.memo(
  React.forwardRef<HTMLDivElement, ResizeEdgeProps>((props, ref) => {
    const LineSVGComponent =
      props.position.y === 0.5 ? DimensionableControlVertical : DimensionableControlHorizontal
    const dispatch = useDispatch()
    const scale = useEditorState(
      Substores.canvas,
      (store) => store.editor.canvas.scale,
      'ResizeEdge scale',
    )
    const jsxMetadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
    const pathTreesRef = useRefEditorState((store) => store.editor.elementPathTree)
    const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
    const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)

    const onEdgeMouseDown = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>) => {
        startResizeInteraction(
          event,
          dispatch,
          props.position,
          props.enabledDirection,
          jsxMetadataRef.current,
          pathTreesRef.current,
          selectedViewsRef.current,
          canvasOffsetRef.current,
          scale,
        )
      },
      [
        dispatch,
        props.position,
        props.enabledDirection,
        jsxMetadataRef,
        pathTreesRef,
        selectedViewsRef,
        canvasOffsetRef,
        scale,
      ],
    )

    return (
      <div ref={ref} style={{ position: 'absolute' }} onMouseDown={onEdgeMouseDown}>
        <LineSVGComponent />
        <div
          style={{
            position: 'absolute',
            pointerEvents: 'initial',
            width: ResizeEdgeMouseAreaSize / scale,
            height: ResizeEdgeMouseAreaSize / scale,
            top: -ResizeEdgeMouseAreaOffset / scale,
            left: -ResizeEdgeMouseAreaOffset / scale,
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
  pathTrees: ElementPathTrees,
  selectedViews: Array<ElementPath>,
  canvasOffset: CanvasVector,
  scale: number,
) {
  event.stopPropagation()
  if (event.buttons === 1) {
    // TODO update this to call createInteractionSession to use canvas strategies
    const centerBasedResize = event.altKey
    const keepAspectRatio = event.shiftKey // || props.elementAspectRatioLocked ???
    const enableSnapping = !event.metaKey
    const canvasPositions = windowToCanvasCoordinates(
      scale,
      canvasOffset,
      windowPoint({ x: event.nativeEvent.x, y: event.nativeEvent.y }),
    )
    const start: CanvasPoint = canvasPositions.canvasPositionRaw
    const originalFrames = getOriginalFrames(selectedViews, metadata, pathTrees)
    const isMultiSelect = selectedViews.length > 1

    const originalSize = boundingRectangleArray(
      selectedViews.map((path) =>
        nullIfInfinity(MetadataUtils.getFrameInCanvasCoords(path, metadata)),
      ),
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
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'DimensionableControlVertical scale',
  )
  const colorTheme = useColorTheme()
  const controlLength = ControlSideLong
  const controlWidth = ControlSideShort
  const controlOffset = controlLength / 2

  return (
    <div
      className='label-dimensionableControlVertical'
      style={{
        position: 'relative',
        backgroundColor: colorTheme.canvasElementBackground.value,
        borderRadius: 5 / scale,
        boxShadow: `0px 0px 0px ${0.3 / scale}px ${colorTheme.primary.value}, 0px ${1 / scale}px ${
          3 / scale
        }px ${colorTheme.canvasControlsDimensionableControlShadow.value}`,
        height: controlLength / scale,
        width: controlWidth / scale,
        left: -1 / scale,
        top: -controlOffset / scale,
      }}
    />
  )
})

const DimensionableControlHorizontal = React.memo(() => {
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'DimensionableControlHorizontal scale',
  )
  const colorTheme = useColorTheme()
  const controlLength = ControlSideShort
  const controlWidth = ControlSideLong
  const controlOffset = controlWidth / 2

  return (
    <div
      className='label-dimensionableControlVertical'
      style={{
        position: 'relative',
        backgroundColor: colorTheme.canvasElementBackground.value,
        borderRadius: 5 / scale,
        boxShadow: `0px 0px 0px ${0.3 / scale}px ${colorTheme.primary.value}, 0px ${1 / scale}px ${
          3 / scale
        }px ${colorTheme.canvasControlsDimensionableControlShadow.value}`,
        height: controlLength / scale,
        width: controlWidth / scale,
        left: -controlOffset / scale,
        top: -1 / scale,
      }}
    />
  )
})
