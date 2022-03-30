/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import Tippy from '@tippyjs/react'
import React, { useEffect } from 'react'
import { offsetPoint, zeroCanvasPoint, zeroRectangle } from '../../../core/shared/math-utils'
import { useColorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { getMultiselectBounds } from '../canvas-strategies/shared-absolute-move-strategy-helpers'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

export const FlowMoveControlTooltip = React.memo(() => {
  const colorTheme = useColorTheme()
  const frame = useEditorState((store) => {
    return getMultiselectBounds(store.editor.jsxMetadata, store.editor.selectedViews)
  }, 'FlowMoveControlTooltip frame')
  const dragVector = useEditorState((store) => {
    if (store.editor.canvas.interactionSession?.interactionData.type === 'DRAG') {
      return store.editor.canvas.interactionSession.interactionData.drag ?? zeroCanvasPoint
    } else {
      return zeroCanvasPoint
    }
  }, 'FlowMoveControlTooltip dragVector')
  if (frame == null) {
    return null
  } else {
    return (
      <CanvasOffsetWrapper>
        <Tippy
          css={{
            fontWeight: 400,
            fontSize: 11,
            fontFamily:
              "utopian-inter, -apple-system, BlinkMacSystemFont, Helvetica, 'Segoe UI', Roboto, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol'",
            backgroundColor: `${colorTheme.neutralInvertedBackground.value} !important`,
            color: `${colorTheme.neutralInvertedForeground.value} !important`,
            '& .tippy-content': {
              padding: '4px 8px !important',
            },
            '&[data-placement^=top] .tippy-arrow::before': {
              borderTopColor: `${colorTheme.neutralInvertedBackground.value} !important`,
            },
            '&[data-placement^=right] .tippy-arrow::before': {
              borderRightColor: `${colorTheme.neutralInvertedBackground.value} !important`,
            },
            '&[data-placement^=bottom] .tippy-arrow::before': {
              borderBottomColor: `${colorTheme.neutralInvertedBackground.value} !important`,
            },
            '&[data-placement^=left] .tippy-arrow::before': {
              borderLeftColor: `${colorTheme.neutralInvertedBackground.value} !important`,
            },
          }}
          visible
          arrow
          content={'Hold Mouse to Convert to Absolute'}
          placement={'top'}
          delay={[200, 100]}
          animation='fade'
          theme='material'
        >
          <div
            style={{
              position: 'absolute',
              top: frame.y + dragVector.y,
              left: frame.x + dragVector.x,
              width: frame.width,
              height: frame.height,
            }}
          ></div>
        </Tippy>
      </CanvasOffsetWrapper>
    )
  }
})

export const AnimationTimer = 2000
const ShowControlDelay = 150
export const FlowMoveControlTimer = React.memo(() => {
  const cursorPosition = useEditorState((store) => {
    if (store.editor.canvas.interactionSession?.interactionData.type === 'DRAG') {
      if (store.editor.canvas.interactionSession.interactionData.drag != null) {
        return offsetPoint(
          store.editor.canvas.interactionSession.interactionData.dragStart,
          store.editor.canvas.interactionSession.interactionData.drag,
        )
      } else {
        return store.editor.canvas.interactionSession.interactionData.dragStart
      }
    } else {
      return null
    }
  }, 'FlowMoveControlTimer cursorPosition')

  const canvasOffset = useEditorState((store) => {
    return store.editor.canvas.roundedCanvasOffset
  }, 'FlowMoveControlTimer canvasOffset')

  const showControl = useEditorState((store) => {
    if (store.editor.canvas.interactionSession != null) {
      return (
        store.editor.canvas.interactionSession.globalTime -
          store.editor.canvas.interactionSession.lastInteractionTime >
        ShowControlDelay
      )
    } else {
      return false
    }
  }, 'FlowMoveControlTimer showControl')

  if (showControl) {
    return (
      <div
        style={{
          position: 'absolute',
          top: (cursorPosition?.y ?? 0) - 120 + canvasOffset.y,
          left: (cursorPosition?.x ?? 0) - 95 + canvasOffset.x,
        }}
      >
        <SvgLoader />
      </div>
    )
  } else {
    return null
  }
})

const SvgLoader = React.memo(() => {
  useEffect(() => {
    ;(function (w) {
      function draw(element: any, rate: number) {
        var count = element.length,
          angle = 360 * rate
        angle %= 360

        var rad = (angle * Math.PI) / 180,
          x = Math.sin(rad) * 125,
          y = Math.cos(rad) * -125,
          mid = angle > 180 ? 1 : 0,
          shape = 'M 0 0 v -125 A 125 125 1 ' + mid + ' 1 ' + x + ' ' + y + ' z'
        if (element instanceof Array) while (count--) element[count].setAttribute('d', shape)
        else element.setAttribute('d', shape)
      }
      ;(w as any).svgPieTimer = function (props: any) {
        var element = props.element,
          duration = props.duration || 1000,
          n = props.loops
        n = n === 0 ? 0 : n ? n : 1
        var end = Date.now() + duration * n,
          totaldur = duration * n
        ;(function frame() {
          var current = Date.now(),
            remaining = end - current,
            rate = n + 1 - remaining / duration
          if (remaining < 60) {
            draw(element, n - 0.0001)
            // Stop animating when we reach n loops (if n is set)
            if (remaining < totaldur && n) return
          }
          // To reverse, uncomment this line
          //rate = 360 - rate;
          draw(element, rate)
          requestAnimationFrame(frame)
        })()
      }
    })(window)

    var loader = document.getElementById('flow-move-loader')
    ;(window as any).svgPieTimer({
      element: [loader],
      duration: AnimationTimer - ShowControlDelay,
      loops: 1,
    })
  })

  return (
    <svg width='250' height='250' viewBox='0 0 250 250' id='container' transform='scale(.11)'>
      <path id='flow-move-loader' transform='translate(125, 125)' />
    </svg>
  )
})

export const FlowGhostOutline = React.memo(() => {
  const frame = useEditorState((store) => {
    return getMultiselectBounds(store.editor.jsxMetadata, store.editor.selectedViews)
  }, 'FlowGhostOutline frame')
  const dragVector = useEditorState((store) => {
    if (store.editor.canvas.interactionSession?.interactionData.type === 'DRAG') {
      return store.editor.canvas.interactionSession.interactionData.drag ?? zeroCanvasPoint
    } else {
      return zeroCanvasPoint
    }
  }, 'FlowGhostOutline dragVector')
  const colorTheme = useColorTheme()
  if (frame == null) {
    return null
  } else {
    return (
      <CanvasOffsetWrapper>
        <div
          style={{
            position: 'absolute',
            top: frame.y + dragVector.y,
            left: frame.x + dragVector.x,
            width: frame.width,
            height: frame.height,
            boxSizing: 'border-box',
            boxShadow: `0px 0px 0px 1px ${colorTheme.canvasSelectionFocusable.value}`,
            opacity: '50%',
          }}
        />
      </CanvasOffsetWrapper>
    )
  }
})
