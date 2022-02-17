import React, { useEffect } from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import {
  boundingRectangleArray,
  CanvasPoint,
  windowPoint,
  WindowRectangle,
} from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { fastForEach } from '../../../../core/shared/utils'
import { useColorTheme } from '../../../../uuiui'
import { EditorDispatch } from '../../../editor/action-types'
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

  const observerRef = React.useRef<MutationObserver | null>()
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
    const frames: Array<WindowRectangle> = []
    fastForEach(selectedElementsRef.current, (target) => {
      const htmlElement = document.querySelector(`*[data-paths~="${EP.toString(target)}"]`)
      const frame = htmlElement?.getBoundingClientRect()
      if (frame != null) {
        frames.push({
          x: frame.x,
          y: frame.y,
          width: frame.width,
          height: frame.height,
        } as WindowRectangle)
      }
    })

    const boundingBox = boundingRectangleArray(frames)
    if (boundingBox != null && controlRef.current != null) {
      const boundingBoxInCanvasCoords = windowToCanvasCoordinatesGlobal(
        windowPoint({ x: boundingBox.x, y: boundingBox.y }),
      ).canvasPositionRounded
      controlRef.current.style.left = boundingBoxInCanvasCoords.x + 'px'
      controlRef.current.style.top = boundingBoxInCanvasCoords.y + 'px'
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

  useEffect(() => {
    const observer = new MutationObserver(observerCallback)
    observerRef.current = observer
    return function cleanup() {
      observer.disconnect()
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  useEffect(() => {
    if (selectedElements.length > 0) {
      // TODO FIND SOMETHING NICER
      // this is a total hack that I found #node-connectors is still changing attributes on scroll. TODO use a HTMLElement that is deliberately for this
      const thisElementRerendersOnScroll = document.getElementById('node-connectors')

      if (thisElementRerendersOnScroll != null && observerRef.current != null) {
        observerRef.current.observe(thisElementRerendersOnScroll, {
          attributes: true,
        })
      }

      fastForEach(selectedElements, (path) => {
        const htmlElement = document.querySelector(`*[data-paths~="${EP.toString(path)}"]`)
        if (htmlElement != null && observerRef.current != null) {
          observerRef.current.observe(htmlElement, {
            attributes: true,
            childList: true,
            subtree: true,
          })
        }
      })

      observerCallback()
    }

    return function cleanup() {
      observerRef.current?.disconnect()
    }
  }, [selectedElements, observerCallback, allSelectedElementsAbsolute])

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
          ref={leftRef}
          position={{ x: 0, y: 0.5 }}
          cursor={CSSCursor.ResizeNS}
          direction='vertical'
          enabledDirection={DirectionHorizontal}
        />
        <ResizeEdge
          ref={topRef}
          position={{ x: 0.5, y: 0 }}
          cursor={CSSCursor.ResizeEW}
          direction='horizontal'
          enabledDirection={DirectionVertical}
        />
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
        <ResizePoint
          ref={topLeftRef}
          position={{ x: 0, y: 0 }}
          cursor={CSSCursor.ResizeNWSE}
          enabledDirection={DirectionAll}
        />
        <ResizePoint
          ref={topRightRef}
          position={{ x: 1, y: 0 }}
          cursor={CSSCursor.ResizeNESW}
          enabledDirection={DirectionAll}
        />
        <ResizePoint
          ref={bottomLeftRef}
          position={{ x: 0, y: 1 }}
          cursor={CSSCursor.ResizeNESW}
          enabledDirection={DirectionAll}
        />
        <ResizePoint
          ref={bottomRightRef}
          position={{ x: 1, y: 1 }}
          cursor={CSSCursor.ResizeNWSE}
          enabledDirection={DirectionAll}
        />
      </div>
    )
  }
  return null
})

interface ResizePointProps {
  cursor: CSSCursor
  position: EdgePosition
  enabledDirection: EdgePosition
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
          props.enabledDirection,
          jsxMetadataRef.current,
          selectedViewsRef.current,
        )
      },
      [dispatch, props.position, props.enabledDirection, jsxMetadataRef, selectedViewsRef],
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
            top: -catcherSize / 2,
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
          cursor: props.cursor,
        }}
        onMouseDown={onEdgeMouseDown}
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
      originalFrames.map((frameInfo) => frameInfo.canvasFrame ?? null),
    )

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
        undefined,
        enableSnapping,
        centerBasedResize,
        keepAspectRatio,
      )

      dispatch([CanvasActions.createDragState(newDragState)], 'canvas')
    }
  }
}
