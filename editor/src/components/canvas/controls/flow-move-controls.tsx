/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import Tippy from '@tippyjs/react'
import React, { useEffect } from 'react'
import { usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'
import { offsetPoint, zeroCanvasPoint, zeroRectangle } from '../../../core/shared/math-utils'
import { HeadlessStringInput, useColorTheme, UtopiaStyles } from '../../../uuiui'
import { undo } from '../../editor/actions/action-creators'
import { NavigatorWidthAtom } from '../../editor/store/editor-state'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import CanvasActions from '../canvas-actions'
import { getMultiselectBounds } from '../canvas-strategies/shared-absolute-move-strategy-helpers'
import { ModeSelectButton } from '../mode-select-buttons'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { toast, ToastContainer } from 'react-toastify'
import 'react-toastify/dist/ReactToastify.css'
import { NO_OP } from '../../../core/shared/utils'

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

const styleContent = `
.outline1 {
  background-image: linear-gradient(90deg, blue 50%, transparent 50%),
    linear-gradient(90deg, blue 50%, transparent 50%),
    linear-gradient(0deg, blue 50%, transparent 50%),
    linear-gradient(0deg, blue 50%, transparent 50%);
  background-repeat: repeat-x, repeat-x, repeat-y, repeat-y;
  background-size: 15px 2px, 15px 2px, 2px 15px, 2px 15px;
  background-position: left top, right bottom, left bottom, right top;
  animation: border-dance 1s infinite linear;
}
@keyframes border-dance {
  0% {
    background-position: left top, right bottom, left bottom, right top;
    background-image: none;
  }
  100% {
    background-position: left 15px top, right 15px bottom, left bottom 15px,
      right top 15px;
    background-image: linear-gradient(90deg, blue 50%, transparent 50%),
      linear-gradient(90deg, blue 50%, transparent 50%),
      linear-gradient(0deg, blue 50%, transparent 50%),
      linear-gradient(0deg, blue 50%, transparent 50%);
  }
}
.outline2 {
  box-sizing: border-box;
  animation: magic-border 2s ease-in-out infinite forwards;
}
@keyframes magic-border {
  0% {
    border-image-slice: 0;
  }
  50% {
    border-image-slice: 4;
    border-image-width: 6px;
    border-image-outset: 4px;
    border-image-repeat: repeat repeat;
    border-image-source: url("https://mdn.github.io/css-examples/tools/border-image-generator/border-image-5.png");
    border-style: solid;
  }
  100% {
    border-image-slice: 0;
  }
}
.outline3 {
  background-color: blue;
  left: 50%;
  top: 50%;
  transform: scale(0.8);
  animation: grow 1.5s ease-in-out infinite forwards;
  z-index: 10;
}
@keyframes grow {
  0% {
    opacity: 0;
    transform: scale(0.8);
  }
  25% {
    opacity: 0;
  }
  50% {
    opacity: 0.5;
  }
  75% {
    opacity: 0;
  }
  100% {
    opacity: 0;
    transform: scale(1.2);
  }
}

.outline4 {
  border:3px solid blue;
  border-image: url("data:image/svg+xml;charset=utf-8,%3Csvg width='102' height='102' viewBox='0 0 100 100' fill='none' xmlns='http://www.w3.org/2000/svg'%3E %3Cstyle%3Epath%7Banimation:stroke 3s infinite linear%3B%7D%40keyframes stroke%7Bto%7Bstroke-dashoffset:776%3B%7D%7D%3C/style%3E%3ClinearGradient id='g' x1='0%25' y1='0%25' x2='0%25' y2='100%25'%3E%3Cstop offset='0%25' stop-color='%blue' /%3E%3Cstop offset='25%25' stop-color='blue' /%3E%3Cstop offset='50%25' stop-color='blue' /%3E%3Cstop offset='100%25' stop-color='blue' /%3E%3C/linearGradient%3E %3Cpath d='M1.5 1.5 l97 0l0 97l-97 0 l0 -97' stroke-linecap='square' stroke='url(%23g)' stroke-width='3' stroke-dasharray='388'/%3E %3C/svg%3E") 1;
}

.outline5 {
  border: 1px solid #574BE2;
  animation: fadeinoutflow 2s linear infinite forwards;
}
@keyframes fadeinoutflow {
  0%,100% { opacity: 0; }
  50% { opacity: 1; }
}
`
export const ConversionHighlightOutline = React.memo(() => {
  const navigatorVisible = useEditorState(
    (store) => !store.editor.navigator.minimised,
    'ConversionHighlightOutline navigatorVisible',
  )
  const [animationTime, setAnimationTime] = React.useState(2000)
  const navigatorWidth = usePubSubAtomReadOnly(NavigatorWidthAtom)
  const [selectedOutline, setSelectedOutline] = React.useState('outline5')
  const outlineFrames = useEditorState(
    (store) => store.editor.canvas.controls.highlightOutlines,
    'ConversionHighlightOutline frames',
  )
  const colorTheme = useColorTheme()
  const dispatch = useRefEditorState((store) => store.dispatch)
  const ref = React.useRef<HTMLDivElement>(null)
  const timerRef = React.useRef<NodeJS.Timeout | null>(null)

  useEffect(() => {
    if (ref.current != null) {
      ref.current.style.setProperty('display', 'block')
      if (timerRef.current != null) {
        clearTimeout(timerRef.current)
        timerRef.current = null
      }
    }

    timerRef.current = setTimeout(() => {
      if (ref.current != null) {
        ref.current.style.setProperty('display', 'none')
        dispatch.current([CanvasActions.clearOutlineHighlights()], 'canvas')
      }
    }, animationTime)
  }, [outlineFrames, dispatch, animationTime])

  return (
    <React.Fragment>
      <CanvasOffsetWrapper>
        <style>{styleContent}</style>
        <div ref={ref} style={{ display: 'none' }}>
          {outlineFrames.map((frame, i) => (
            <div
              key={i}
              style={{
                position: 'absolute',
                top: frame.y,
                left: frame.x,
                width: frame.width,
                height: frame.height,
              }}
              className={selectedOutline}
            ></div>
          ))}
        </div>
      </CanvasOffsetWrapper>
    </React.Fragment>
  )
})

export const FlowUndoButton = React.memo(() => {
  const dispatch = useEditorState((store) => store.dispatch, 'flowundobutton dispatch')
  const undoButtonVisible = useEditorState(
    (store) => store.editor.canvas.controls.undoButtonVisible,
    'undoButtonVisible',
  )
  const isInteraction = useEditorState(
    (store) => store.editor.canvas.interactionSession != null,
    'isInteraction',
  )
  const onButtonClick = React.useCallback(
    () => dispatch([CanvasActions.removeUndoButton(), undo()], 'everyone'),
    [dispatch],
  )

  useEffect(() => {
    if (undoButtonVisible && !isInteraction) {
      toast(<UndoToast onUndo={onButtonClick} closeToast={NO_OP} />, {
        // hook will be called whent the component unmount
        onClose: () => dispatch([CanvasActions.removeUndoButton()], 'everyone'),
        toastId: 'customId',
      })
    }
  }, [isInteraction, onButtonClick, undoButtonVisible, dispatch])

  return null
})

const UndoToast = ({ onUndo, closeToast }: { onUndo: () => void; closeToast: () => void }) => {
  const handleClick = () => {
    onUndo()
    closeToast()
  }
  return (
    <div>
      <h3>
        div converted to absolute <button onClick={handleClick}>undo?</button>
      </h3>
    </div>
  )
}
