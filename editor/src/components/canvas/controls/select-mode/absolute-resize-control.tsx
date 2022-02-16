import React, { useEffect } from 'react'
import * as EP from '../../../../core/shared/element-path'
import { windowPoint } from '../../../../core/shared/math-utils'
import { useColorTheme } from '../../../../uuiui'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { CSSCursor } from '../../canvas-types'
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
    return true
  }, 'AbsoluteResizeControl allSelectedElementsAbsolute')

  const observerCallback = React.useCallback(() => {
    // TODO MULTISELECT + BOUNDING BOX
    if (selectedElementsRef.current.length === 1) {
      const target = selectedElementsRef.current[0]
      const htmlElement = document.querySelector(`*[data-paths~="${EP.toString(target)}"]`)
      const frame = htmlElement?.getBoundingClientRect()
      if (frame != null && controlRef.current != null) {
        const frameInCanvasCoords = windowToCanvasCoordinatesGlobal(
          windowPoint({ x: frame.left, y: frame.top }),
        ).canvasPositionRounded
        controlRef.current.style.left = frameInCanvasCoords.x + 'px'
        controlRef.current.style.top = frameInCanvasCoords.y + 'px'
        controlRef.current.style.width = frame.width + 'px'
        controlRef.current.style.height = frame.height + 'px'
        if (topRightRef.current != null) {
          topRightRef.current.style.left = frameInCanvasCoords.x + frame.width + 'px'
          topRightRef.current.style.top = frameInCanvasCoords.y + 'px'
        }
        if (bottomLeftRef.current != null) {
          bottomLeftRef.current.style.left = frameInCanvasCoords.x + 'px'
          bottomLeftRef.current.style.top = frameInCanvasCoords.y + frame.height + 'px'
        }
        if (bottomRightRef.current != null) {
          bottomRightRef.current.style.left = frameInCanvasCoords.x + frame.width + 'px'
          bottomRightRef.current.style.top = frameInCanvasCoords.y + frame.height + 'px'
        }

        if (leftRef.current != null) {
          leftRef.current.style.left = frameInCanvasCoords.x + 'px'
          leftRef.current.style.top = frameInCanvasCoords.y + 'px'
          leftRef.current.style.height = frame.height + 'px'
        }
        if (topRef.current != null) {
          topRef.current.style.left = frameInCanvasCoords.x + 'px'
          topRef.current.style.top = frameInCanvasCoords.y + 'px'
          topRef.current.style.width = frame.width + 'px'
        }
        if (bottomRef.current != null) {
          bottomRef.current.style.left = frameInCanvasCoords.x + 'px'
          bottomRef.current.style.top = frameInCanvasCoords.y + frame.height + 'px'
          bottomRef.current.style.width = frame.width + 'px'
        }
        if (rightRef.current != null) {
          rightRef.current.style.left = frameInCanvasCoords.x + frame.width + 'px'
          rightRef.current.style.top = frameInCanvasCoords.y + 'px'
          rightRef.current.style.height = frame.height + 'px'
        }
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
    if (selectedElements.length === 1) {
      // TODO FIND SOMETHING NICER
      // this is a total hack that I found #node-connectors is still changing attributes on scroll. TODO use a HTMLElement that is deliberately for this
      const thisElementRerendersOnScroll = document.getElementById('node-connectors')

      if (thisElementRerendersOnScroll != null && observerRef.current != null) {
        observerRef.current.observe(thisElementRerendersOnScroll, {
          attributes: true,
        })
      }

      // TODO MULTISELECT, FILTER FOR ABSOLUTE
      const htmlElement = document.querySelector(
        `*[data-paths~="${EP.toString(selectedElements[0])}"]`,
      )
      if (htmlElement != null && observerRef.current != null) {
        observerRef.current.observe(htmlElement, {
          attributes: true,
          childList: true,
          subtree: true,
        })
      }

      observerCallback()
    }

    return function cleanup() {
      observerRef.current?.disconnect()
    }
  }, [selectedElements, observerCallback])

  if (allSelectedElementsAbsolute) {
    return (
      <div
        ref={controlRef}
        style={{
          position: 'absolute',
          transform: `translate(var(--utopia-canvas-offset-x), var(--utopia-canvas-offset-y))`,
        }}
      >
        <ResizeEdge ref={leftRef} cursor={CSSCursor.ResizeNS} direction='vertical' />
        <ResizeEdge ref={topRef} cursor={CSSCursor.ResizeEW} direction='horizontal' />
        <ResizeEdge ref={rightRef} cursor={CSSCursor.ResizeEW} direction='vertical' />
        <ResizeEdge ref={bottomRef} cursor={CSSCursor.ResizeNS} direction='horizontal' />
        <ResizePoint ref={topLeftRef} cursor={CSSCursor.ResizeNWSE} />
        <ResizePoint ref={topRightRef} cursor={CSSCursor.ResizeNESW} />
        <ResizePoint ref={bottomLeftRef} cursor={CSSCursor.ResizeNESW} />
        <ResizePoint ref={bottomRightRef} cursor={CSSCursor.ResizeNWSE} />
      </div>
    )
  }
  return null
})

interface ResizePointProps {
  cursor: CSSCursor
}

const ResizePoint = React.memo(
  React.forwardRef<HTMLDivElement, ResizePointProps>((props, ref) => {
    const colorTheme = useColorTheme()
    const scale = useEditorState((store) => store.editor.canvas.scale, 'ResizePoint scale')
    const catcherSize = 12
    const size = 6
    return (
      <div
        ref={ref}
        style={{
          position: 'absolute',
          width: 6,
          height: 6,
        }}
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
}

const ResizeEdge = React.memo(
  React.forwardRef<HTMLDivElement, ResizeEdgeProps>((props, ref) => {
    const scale = useEditorState((store) => store.editor.canvas.scale, 'ResizeEdge scale')
    const lineSize = 10 / scale
    const width = props.direction === 'horizontal' ? undefined : lineSize
    const height = props.direction === 'vertical' ? undefined : lineSize
    return (
      <div
        ref={ref}
        style={{
          pointerEvents: 'initial',
          position: 'absolute',
          width: width,
          height: height,
          boxSizing: 'border-box',
          backgroundColor: 'transparent',
          cursor: props.cursor,
        }}
      ></div>
    )
  }),
)
