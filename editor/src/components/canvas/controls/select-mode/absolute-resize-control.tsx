import React from 'react'
import { LayoutTargetableProp } from '../../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import {
  boundingRectangleArray,
  CanvasPoint,
  CanvasRectangle,
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
  DirectionAll,
  DirectionHorizontal,
  DirectionVertical,
  EdgePosition,
  resizeDragState,
  updateResizeDragState,
} from '../../canvas-types'
import { getOriginalFrames } from '../../canvas-utils'
import { windowToCanvasCoordinatesGlobal } from '../../dom-lookup'
import { findFramesFromDOM, useMutationObserver } from '../observer-hooks'

export const AbsoluteResizeControl = React.memo(() => {
  const controlRef = React.useRef<HTMLDivElement>(null)
  const topLeftRef = React.useRef<HTMLDivElement>(null)
  const topRightRef = React.useRef<HTMLDivElement>(null)
  const bottomLeftRef = React.useRef<HTMLDivElement>(null)
  const bottomRightRef = React.useRef<HTMLDivElement>(null)

  const leftRef = React.useRef<HTMLDivElement>(null)
  const topRef = React.useRef<HTMLDivElement>(null)
  const rightRef = React.useRef<HTMLDivElement>(null)
  const bottomRef = React.useRef<HTMLDivElement>(null)

  const selectedElementsRef = useRefEditorState((store) => store.editor.selectedViews)

  const selectedElements = useEditorState(
    (store) => store.editor.selectedViews,
    'AbsoluteResizeControl selectedElements',
  )
  const allSelectedElementsAbsolute = useEditorState((store) => {
    return (
      store.editor.selectedViews.length > 0 &&
      store.editor.selectedViews.every((path) => {
        return (
          MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path)
            ?.specialSizeMeasurements.position === 'absolute'
        )
      })
    )
  }, 'AbsoluteResizeControl allSelectedElementsAbsolute')

  const observerCallback = React.useCallback(() => {
    const frames: Array<CanvasRectangle> = findFramesFromDOM(selectedElementsRef.current)
    const boundingBox = boundingRectangleArray(frames)
    if (boundingBox != null && controlRef.current != null) {
      controlRef.current.style.left = boundingBox.x + 'px'
      controlRef.current.style.top = boundingBox.y + 'px'
      controlRef.current.style.width = boundingBox.width + 'px'
      controlRef.current.style.height = boundingBox.height + 'px'
      if (topRightRef.current != null) {
        topRightRef.current.style.left = boundingBox.width + 'px'
      }
      if (bottomLeftRef.current != null) {
        bottomLeftRef.current.style.top = boundingBox.height + 'px'
      }
      if (bottomRightRef.current != null) {
        bottomRightRef.current.style.left = boundingBox.width + 'px'
        bottomRightRef.current.style.top = boundingBox.height + 'px'
      }

      if (leftRef.current != null) {
        leftRef.current.style.height = boundingBox.height + 'px'
      }
      if (topRef.current != null) {
        topRef.current.style.width = boundingBox.width + 'px'
      }
      if (bottomRef.current != null) {
        bottomRef.current.style.top = boundingBox.height + 'px'
        bottomRef.current.style.width = boundingBox.width + 'px'
      }
      if (rightRef.current != null) {
        rightRef.current.style.left = boundingBox.width + 'px'
        rightRef.current.style.height = boundingBox.height + 'px'
      }
    }
  }, [selectedElementsRef])

  const absoluteElements = allSelectedElementsAbsolute ? selectedElements : []
  const observerRef = useMutationObserver(absoluteElements, observerCallback)

  if (allSelectedElementsAbsolute) {
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
        <ResizePoint ref={bottomLeftRef} position={{ x: 0, y: 1 }} cursor={CSSCursor.ResizeNESW} />
        <ResizePoint ref={bottomRightRef} position={{ x: 1, y: 1 }} cursor={CSSCursor.ResizeNWSE} />
      </div>
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
    const scale = useEditorState((store) => store.editor.canvas.scale, 'ResizePoint scale')
    const catcherSize = ResizePointMouseAreaSize / scale
    const size = ResizePointSize / scale

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
          width: size,
          height: size,
        }}
        onMouseDown={onPointMouseDown}
      >
        <div
          style={{
            position: 'relative',
            pointerEvents: 'initial',
            width: '100%',
            height: '100%',
            top: -size / 2,
            left: -size / 2,
            boxSizing: 'border-box',
            borderWidth: 1 / scale,
            backgroundColor: colorTheme.canvasControlsSizeBoxBackground.value,
            borderRadius: '10%',
            borderStyle: 'none',
            borderColor: 'transparent',
            boxShadow: `${colorTheme.canvasControlsSizeBoxShadowColor.o(50).value} 0px 0px ${
              1 / scale
            }px, ${colorTheme.canvasControlsSizeBoxShadowColor.o(21).value} 0px ${1 / scale}px ${
              2 / scale
            }px ${1 / scale}px `,
          }}
        />
        <div
          style={{
            position: 'relative',
            width: catcherSize,
            height: catcherSize,
            top: -catcherSize,
            left: -catcherSize / 2,
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
    const scale = useEditorState((store) => store.editor.canvas.scale, 'ResizeEdge scale')
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

    const lineSize = ResizeMouseAreaSize / scale
    const width = props.direction === 'horizontal' ? undefined : lineSize
    const height = props.direction === 'vertical' ? undefined : lineSize
    const offsetLeft = props.direction === 'horizontal' ? undefined : -lineSize / 2
    const offsetTop = props.direction === 'vertical' ? undefined : -lineSize / 2
    return (
      <div
        style={{
          position: 'absolute',
          top: offsetTop,
          left: offsetLeft,
          pointerEvents: 'none',
        }}
      >
        <div
          ref={ref}
          style={{
            position: 'relative',
            width: width,
            height: height,
            backgroundColor: 'transparent',
            cursor: props.cursor,
            pointerEvents: 'initial',
          }}
          onMouseDown={onEdgeMouseDown}
        ></div>
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
