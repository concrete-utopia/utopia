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
import { NO_OP } from '../../../../core/shared/utils'
import { useColorTheme } from '../../../../uuiui'
import { EditorDispatch } from '../../../editor/action-types'
import { setResizeOptionsTargetOptions } from '../../../editor/actions/action-creators'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import { useCanvasOffset } from '../../canvas-offset-hooks'
import {
  CSSCursor,
  DirectionAll,
  DirectionHorizontal,
  DirectionVertical,
  EdgePosition,
  resizeDragState,
  updateResizeDragState,
} from '../../canvas-types'
import { getOriginalFrames } from '../../canvas-utils'
import { windowToCanvasCoordinatesGlobal } from '../../dom-lookup'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

interface AbsoluteResizeControlProps {
  localSelectedElements: Array<ElementPath>
}

export const AbsoluteResizeControl = React.memo<AbsoluteResizeControlProps>((props) => {
  const localSelectedElements = props.localSelectedElements
  const allSelectedElementsAbsolute = useEditorState((store) => {
    return (
      localSelectedElements.length > 0 &&
      localSelectedElements.every((path) => {
        return (
          MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path)
            ?.specialSizeMeasurements.position === 'absolute'
        )
      })
    )
  }, 'AbsoluteResizeControl allSelectedElementsAbsolute')

  const absoluteElements = allSelectedElementsAbsolute ? localSelectedElements : []

  const controlRef = useBoundingBox(absoluteElements, (ref, boundingBox) => {
    ref.current.style.left = boundingBox.x + 'px'
    ref.current.style.top = boundingBox.y + 'px'
    ref.current.style.width = boundingBox.width + 'px'
    ref.current.style.height = boundingBox.height + 'px'
  })

  const leftRef = useBoundingBox(absoluteElements, (ref, boundingBox) => {
    ref.current.style.height = boundingBox.height + 'px'
  })
  const topRef = useBoundingBox(absoluteElements, (ref, boundingBox) => {
    ref.current.style.width = boundingBox.width + 'px'
  })
  const rightRef = useBoundingBox(absoluteElements, (ref, boundingBox) => {
    ref.current.style.left = boundingBox.width + 'px'
    ref.current.style.height = boundingBox.height + 'px'
  })

  const bottomRef = useBoundingBox(absoluteElements, (ref, boundingBox) => {
    ref.current.style.top = boundingBox.height + 'px'
    ref.current.style.width = boundingBox.width + 'px'
  })

  const topLeftRef = useBoundingBox(absoluteElements, NO_OP)
  const topRightRef = useBoundingBox(absoluteElements, (ref, boundingBox) => {
    ref.current.style.left = boundingBox.width + 'px'
  })
  const bottomLeftRef = useBoundingBox(absoluteElements, (ref, boundingBox) => {
    ref.current.style.top = boundingBox.height + 'px'
  })
  const bottomRightRef = useBoundingBox(absoluteElements, (ref, boundingBox) => {
    ref.current.style.left = boundingBox.width + 'px'
    ref.current.style.top = boundingBox.height + 'px'
  })

  if (allSelectedElementsAbsolute) {
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
          <ResizeEdge
            ref={leftRef}
            position={{ x: 0, y: 0.5 }}
            cursor={CSSCursor.ResizeEW}
            direction='vertical'
            enabledDirection={DirectionHorizontal}
          />
          <ResizeEdge
            ref={topRef}
            position={{ x: 0.5, y: 0 }}
            cursor={CSSCursor.ResizeNS}
            direction='horizontal'
            enabledDirection={DirectionVertical}
          />
          <ResizePoint ref={topLeftRef} position={{ x: 0, y: 0 }} cursor={CSSCursor.ResizeNWSE} />
          <ResizePoint ref={topRightRef} position={{ x: 1, y: 0 }} cursor={CSSCursor.ResizeNESW} />
          <ResizePoint
            ref={bottomLeftRef}
            position={{ x: 0, y: 1 }}
            cursor={CSSCursor.ResizeNESW}
          />
          <ResizePoint
            ref={bottomRightRef}
            position={{ x: 1, y: 1 }}
            cursor={CSSCursor.ResizeNWSE}
          />
        </div>
      </CanvasOffsetWrapper>
    )
  }
  return null
})

interface ResizePointProps {
  cursor: CSSCursor
  position: EdgePosition
}

const ResizePointMouseAreaSize = 12
const ResizePointSize = 6
const ResizePoint = React.memo(
  React.forwardRef<HTMLDivElement, ResizePointProps>((props, ref) => {
    const colorTheme = useColorTheme()
    const dispatch = useEditorState((store) => store.dispatch, 'ResizeEdge dispatch')
    const jsxMetadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
    const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)

    const onPointMouseDown = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>) => {
        startResizeInteraction(
          event,
          dispatch,
          props.position,
          DirectionAll,
          jsxMetadataRef.current,
          selectedViewsRef.current,
        )
      },
      [dispatch, props.position, jsxMetadataRef, selectedViewsRef],
    )

    return (
      <div
        ref={ref}
        style={{
          position: 'absolute',
          width: `calc(${ResizePointSize}px / var(--utopia-canvas-scale))`,
          height: `calc(${ResizePointSize}px / var(--utopia-canvas-scale))`,
        }}
        onMouseDown={onPointMouseDown}
      >
        <div
          style={{
            position: 'relative',
            pointerEvents: 'initial',
            width: '100%',
            height: '100%',
            top: `calc(${-ResizePointSize / 2}px / var(--utopia-canvas-scale))`,
            left: `calc(${-ResizePointSize / 2}px / var(--utopia-canvas-scale))`,
            boxSizing: 'border-box',
            borderWidth: `calc(1 / var(--utopia-canvas-scale))`,
            backgroundColor: colorTheme.canvasControlsSizeBoxBackground.value,
            borderRadius: '10%',
            borderStyle: 'none',
            borderColor: 'transparent',
            boxShadow: `${colorTheme.canvasControlsSizeBoxShadowColor.o(50).value} 0px 0px
              calc(1px / var(--utopia-canvas-scale)), ${
                colorTheme.canvasControlsSizeBoxShadowColor.o(21).value
              } 0px calc(1px / var(--utopia-canvas-scale)) calc(2px / var(--utopia-canvas-scale)) calc(1px / var(--utopia-canvas-scale))`,
          }}
        />
        <div
          style={{
            position: 'relative',
            width: `calc(${ResizePointMouseAreaSize}px / var(--utopia-canvas-scale))`,
            height: `calc(${ResizePointMouseAreaSize}px / var(--utopia-canvas-scale))`,
            top: `calc(${-ResizePointMouseAreaSize / 2}px / var(--utopia-canvas-scale))`,
            left: `calc(${-ResizePointMouseAreaSize / 2}px / var(--utopia-canvas-scale))`,
            backgroundColor: 'transparent',
            cursor: props.cursor,
          }}
        />
      </div>
    )
  }),
)

interface ResizeEdgeProps {
  cursor: CSSCursor
  direction: 'horizontal' | 'vertical'
  position: EdgePosition
  enabledDirection: EdgePosition
}

const ResizeMouseAreaSize = 10
const ResizeEdge = React.memo(
  React.forwardRef<HTMLDivElement, ResizeEdgeProps>((props, ref) => {
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

    const lineSize = `calc(${ResizeMouseAreaSize}px / var(--utopia-canvas-scale))`
    const width = props.direction === 'horizontal' ? undefined : lineSize
    const height = props.direction === 'vertical' ? undefined : lineSize
    const offsetLeft =
      props.direction === 'horizontal'
        ? `0px`
        : `calc(${-ResizeMouseAreaSize / 2}px / var(--utopia-canvas-scale))`
    const offsetTop =
      props.direction === 'vertical'
        ? `0px`
        : `calc(${-ResizeMouseAreaSize / 2}px / var(--utopia-canvas-scale))`
    return (
      <div
        ref={ref}
        style={{
          position: 'absolute',
          width: width,
          height: height,
          backgroundColor: 'transparent',
          cursor: props.cursor,
          pointerEvents: 'initial',
          transform: `translate(${offsetLeft}, ${offsetTop})`,
        }}
        onMouseDown={onEdgeMouseDown}
      />
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
    // TODO update this to call createInteractionSession to use canvas strategies
    const centerBasedResize = event.altKey
    const keepAspectRatio = event.shiftKey // || props.elementAspectRatioLocked ???
    const enableSnapping = !event.metaKey
    const canvasPositions = windowToCanvasCoordinatesGlobal(
      windowPoint({ x: event.nativeEvent.x, y: event.nativeEvent.y }),
    )
    const start: CanvasPoint = canvasPositions.canvasPositionRaw
    const originalFrames = getOriginalFrames(selectedViews, metadata)
    const isMultiSelect = selectedViews.length !== 1

    const originalSize = boundingRectangleArray(
      selectedViews.map((path) => MetadataUtils.getFrameInCanvasCoords(path, metadata)),
    )
    let possibleTargetProperty: LayoutTargetableProp | undefined = undefined
    if (position.x === 0.5) {
      possibleTargetProperty = 'height'
    } else if (position.y === 0.5) {
      possibleTargetProperty = 'width'
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
