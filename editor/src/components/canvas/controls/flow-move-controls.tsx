/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import Tippy from '@tippyjs/react'
import React, { useEffect } from 'react'
import {
  atomWithPubSub,
  usePubSubAtomReadOnly,
  usePubSubAtomWriteOnly,
} from '../../../core/shared/atom-with-pub-sub'
import { offsetPoint, zeroCanvasPoint, zeroRectangle } from '../../../core/shared/math-utils'
import { HeadlessStringInput, PopupList, useColorTheme, UtopiaStyles } from '../../../uuiui'
import { NavigatorWidthAtom } from '../../editor/store/editor-state'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import CanvasActions from '../canvas-actions'
import { getMultiselectBounds } from '../canvas-strategies/shared-absolute-move-strategy-helpers'
import { ModeSelectButton } from '../mode-select-buttons'
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

type AnimationName =
  | 'simple'
  | 'glow'
  | 'glow2'
  | 'double'
  | 'gradient-wave'
  | 'scan'
  | 'flip'
  | 'dashed'
  | 'dotted'
  | 'sides2'
  | 'flashsides'

const defaultMixBlendMode = 'difference'
const defaultColor = '#0000FF'
const defaultOutline = 'simple'
const animationStyle = (styleName: AnimationName, color: string, mixBlendMode: string) => {
  switch (styleName) {
    case 'simple': {
      return `
      .utopia-highlight-animation::after {
        animation: simple 2s ease forwards;
        outline: 2px solid;
        mix-blend-mode: ${mixBlendMode};
        position: absolute;
        width: 100%;
        height: 100%;
        content: " ";
        outline-color: ${color};
      }
      @keyframes simple {
        0%, 100% {
          opacity: 0;
        }
        50% {
          opacity: 1;
        }
      }
      `
    }
    case 'glow': {
      return `
        .utopia-highlight-animation::after {
          animation: glow 2s ease  forwards;
          mix-blend-mode: ${mixBlendMode};
          position: absolute;
          width: 100%;
          height: 100%;
          content: " ";
        }

        @keyframes glow {
          0%, 100% {
          }
          50% {
            box-shadow: 0px 0px 8px 12px ${color};
          }
        }
        `
    }
    case 'glow2': {
      return `
      .utopia-highlight-animation::after {
        animation: glow2 2s ease  forwards;
        mix-blend-mode: ${mixBlendMode};
        position: absolute;
        width: 100%;
        height: 100%;
        content: " ";
      }
      
      @keyframes glow2 {
        0%, 100% {
        }
        50% {
          box-shadow: 0px 0px 8px 4px ${color};
        }
      }
      `
    }
    case 'double': {
      return `
      .utopia-highlight-animation::after {
        mix-blend-mode: ${mixBlendMode};
        position: absolute;
        width: 100%;
        height: 100%;
        content: " ";
        animation: double 2s ease  forwards;
        border: 8px double ${color};
      }
      @keyframes double {
        0% {
          border: 2px solid rgba(1, 1, 1, 0);
        }
        20% {
          border: 2px solid ${color};
        }
        50% {
          border: 8px double ${color};
        }
        80% {
          border: 2px solid ${color};
        }
        100% {
          border: 2px solid rgba(1, 1, 1, 0);
        }
      }
      `
    }
    case 'gradient-wave': {
      return `
      .utopia-highlight-animation::after {
        mix-blend-mode: ${mixBlendMode};
        position: absolute;
        width: 100%;
        height: 100%;
        content: " ";
        animation: gradient-wave 02s ease ;
      }
      
      @keyframes gradient-wave {
        0% {
          background: rgba(1, 1, 1, 0);
        }
        10% {
          background: linear-gradient(
            90deg,
            ${color} 0%,
            rgba(1, 1, 1, 0) 20%,
            rgba(1, 1, 1, 0) 100%
          );
        }
        30% {
          background: linear-gradient(
            90deg,
            rgba(1, 1, 1, 0) 20%,
            ${color} 30%,
            rgba(1, 1, 1, 0) 40%
          );
        }
        50% {
          background: linear-gradient(
            90deg,
            rgba(1, 1, 1, 0) 40%,
            ${color} 50%,
            rgba(1, 1, 1, 0) 60%
          );
        }
        70% {
          background: linear-gradient(
            90deg,
            rgba(1, 1, 1, 0) 60%,
            ${color} 70%,
            rgba(1, 1, 1, 0) 80%
          );
        }
        90% {
          background: linear-gradient(
            90deg,
            rgba(1, 1, 1, 0) 0%,
            rgba(1, 1, 1, 0) 80%,
            ${color} 100%
          );
        }
        100% {
          background: rgba(1, 1, 1, 0);
        }
      }
      `
    }
    case 'scan': {
      return `
      .utopia-highlight-animation::after {
        mix-blend-mode: ${mixBlendMode};
        position: absolute;
        width: 100%;
        height: 100%;
        content: " ";
        animation: scan 0.3s ease  forwards;
      }
      
      @keyframes scan {
        0% {
          background: rgba(1, 1, 1, 0);
        }
        10% {
          background: linear-gradient(
            90deg,
            ${color} 0%,
            rgba(1, 1, 1, 0) 5%,
            rgba(1, 1, 1, 0) 100%
          );
        }
        20% {
          background: linear-gradient(
            90deg,
            rgba(1, 1, 1, 0) 20%,
            ${color} 25%,
            rgba(1, 1, 1, 0) 30%,
            rgba(1, 1, 1, 0) 100%
          );
        }
        30% {
          background: linear-gradient(
            90deg,
            rgba(1, 1, 1, 0) 45%,
            ${color} 50%,
            rgba(1, 1, 1, 0) 55%,
            rgba(1, 1, 1, 0) 100%
          );
        }
        40% {
          background: linear-gradient(
            90deg,
            rgba(1, 1, 1, 0) 70%,
            ${color} 75%,
            rgba(1, 1, 1, 0) 80%,
            rgba(1, 1, 1, 0) 100%
          );
        }
      
        50% {
          background: linear-gradient(
            90deg,
            rgba(1, 1, 1, 0) 0%,
            rgba(1, 1, 1, 0) 95%,
            ${color} 100%
          );
        }
        60% {
          background: rgba(1, 1, 1, 0);
        }
        100% {
          background: rgba(1, 1, 1, 0);
        }
      }
      `
    }
    case 'flip': {
      return `
      .utopia-highlight-animation::after {
        mix-blend-mode: ${mixBlendMode};
        position: absolute;
        width: 100%;
        height: 100%;
        content: " ";
        animation: flip 2s ;
        border: 1px solid ${color};
      }
      
      @keyframes flip {
        0% {
          border: 1px solid rgba(1, 1, 1, 0);
        }
        10% {
          transform: perspective(600px) rotateY(0);
          animation-timing-function: ease-out;
          border: 1px solid rgba(1, 1, 1, 0);
        }
        40% {
          transform: perspective(600px) translateZ(150px) rotateY(170deg);
          animation-timing-function: ease-out;
          border: 1px solid ${color};
        }
        50% {
          transform: perspective(600px) translateZ(150px) rotateY(190deg) scale(1);
          animation-timing-function: ease-in;
          border: 1px solid ${color};
        }
        80% {
          transform: perspective(600px) rotateY(360deg) scale(0.95);
          animation-timing-function: ease-in;
          border: 1px solid rgba(1, 1, 1, 0);
        }
        90% {
          transform: perspective(600px) scale(1);
          animation-timing-function: ease-in;
          border: 1px solid rgba(1, 1, 1, 0);
        }
        100% {
          border: 1px solid rgba(1, 1, 1, 0);
        }
      }      
      `
    }
    case 'dashed': {
      return `
      .utopia-highlight-animation::after {
        mix-blend-mode: ${mixBlendMode};
        position: absolute;
        width: 100%;
        height: 100%;
        content: " ";
        animation: dashed1 2s ease  forwards;
        outline: dashed;
        outline-width: 2px;
      }
      
      @keyframes dashed1 {
        0% {
          outline-color: rgba(1, 1, 1, 0);
          outline-width: 2px;
        }
        50% {
          outline-color: ${color};
          outline-width: 4px;
        }
        100% {
          outline-color: rgba(1, 1, 1, 0);
          outline-width: 4px;
        }
      }
      `
    }
    case 'dotted': {
      return `
      .utopia-highlight-animation::after {
        mix-blend-mode: ${mixBlendMode};
        position: absolute;
        width: 100%;
        height: 100%;
        content: " ";
        animation: dotted 2s ease  forwards;
        outline: 3px;
        outline-style: dotted;
      }
      
      @keyframes dotted {
        0% {
          outline-color: rgba(1, 1, 1, 0);
        }
        50% {
          outline-color: ${color};
        }
        100% {
          outline-color: rgba(1, 1, 1, 0);
        }
      }
      
      `
    }
    case 'sides2': {
      return `
      .utopia-highlight-animation::after {
        mix-blend-mode: ${mixBlendMode};
        position: absolute;
        width: 100%;
        height: 100%;
        content: " ";
        animation: sides2 2s ease  forwards;
      }
      
      @keyframes sides2 {
        0% {
          border: 0px;
        }
        20% {
          border-top: 2px solid ${color};
          border-right: 2px solid ${color};
          border-bottom: 0px;
          border-left: 0px;
        }
        40% {
          border-top: 0px;
          border-right: 0px;
          border-bottom: 2px solid ${color};
          border-left: 2px solid ${color};
        }
        60% {
          border-top: 2px solid ${color};
          border-right: 2px solid ${color};
          border-bottom: 0px;
          border-left: 0px;
        }
        80% {
          border-top: 0px;
          border-right: 0px;
          border-bottom: 2px solid ${color};
          border-left: 2px solid ${color};
        }
        100% {
          border: 0px;
        }
      }
      
      `
    }
    case 'flashsides': {
      return `
      .utopia-highlight-animation::after {
        mix-blend-mode: ${mixBlendMode};
        position: absolute;
        width: 100%;
        height: 100%;
        content: " ";
        animation: flashsides 2s ease  forwards;
      }
      
      @keyframes flashsides {
        0% {
          border: 0px;
        }
        20% {
          border-top: 2px solid ${color};
          border-right: 0px;
          border-bottom: 0px;
          border-left: 0px;
        }
        40% {
          border-top: 0px;
          border-right: 2px solid ${color};
          border-bottom: 0px;
          border-left: 0px;
        }
        60% {
          border-top: 0px;
          border-right: 0px;
          border-bottom: 2px solid ${color};
          border-left: 0px;
        }
        80% {
          border-top: 0px;
          border-right: 0px;
          border-bottom: 0px;
          border-left: 2px solid ${color};
        }
        100% {
          border: 0px;
        }
      }
      `
    }
    default:
      const _exhaustiveCheck: never = styleName
      throw new Error(`Missing style for ${styleName}`)
  }
}

export const OutlineAnimationAtom = atomWithPubSub({
  key: 'OutlineAnimationAtom',
  defaultValue: animationStyle(defaultOutline, defaultColor, defaultMixBlendMode),
})

export const ConversionHighlightOutline = React.memo(() => {
  const navigatorVisible = useEditorState(
    (store) => !store.editor.navigator.minimised,
    'ConversionHighlightOutline navigatorVisible',
  )
  const navigatorWidth = usePubSubAtomReadOnly(NavigatorWidthAtom)
  const updateStyleTag = usePubSubAtomWriteOnly(OutlineAnimationAtom)
  const [selectedOutline, setSelectedOutline] = React.useState<AnimationName>(defaultOutline)
  const [baseColor, setBaseColor] = React.useState(defaultColor)
  const [blendModeType, setBlendModeType] = React.useState(defaultMixBlendMode)
  const colorTheme = useColorTheme()

  const collectStyleContent = React.useCallback(
    (outline: AnimationName, color: string, mixBlendModeType: string) => {
      return updateStyleTag(animationStyle(outline, color, mixBlendModeType))
    },
    [updateStyleTag],
  )

  return (
    <React.Fragment>
      <div
        style={{
          paddingTop: 4,
          paddingLeft: navigatorVisible ? navigatorWidth + 4 : 4,
          display: 'flex',
        }}
      >
        <div
          style={{
            height: 29,
            display: 'flex',
            alignItems: 'center',
            paddingLeft: 4,
            paddingRight: 4,
            gap: 4,
            borderRadius: 4,
            background: colorTheme.bg0.value,
            boxShadow: UtopiaStyles.popup.boxShadow,
            cursor: 'pointer',
          }}
        >
          <PopupList
            options={[
              'simple',
              'glow',
              'glow2',
              'double',
              'gradient-wave',
              'scan',
              'flip',
              'dashed',
              'dotted',
              'sides2',
              'flashsides',
            ].map((name) => ({ label: name, value: name }))}
            value={{ label: selectedOutline, value: selectedOutline }}
            // eslint-disable-next-line react/jsx-no-bind
            onSubmitValue={(name) => {
              setSelectedOutline(name.value)
              collectStyleContent(name.value, baseColor, blendModeType)
            }}
          />
          <PopupList
            options={[
              'normal',
              'multiply',
              'screen',
              'overlay',
              'darken',
              'lighten',
              'color-dodge',
              'color-burn',
              'hard-light',
              'soft-light',
              'difference',
              'exclusion',
              'hue',
              'saturation',
              'color',
              'luminosity',
            ].map((name) => ({ label: name, value: name }))}
            value={{ label: blendModeType, value: blendModeType }}
            // eslint-disable-next-line react/jsx-no-bind
            onSubmitValue={(name) => {
              setBlendModeType(name.value)
              collectStyleContent(selectedOutline, baseColor, name.value)
            }}
          />
          <HeadlessStringInput
            value={baseColor}
            onChange={React.useCallback(
              (event) => {
                setBaseColor(event.target.value)
                collectStyleContent(selectedOutline, event.target.value, blendModeType)
              },
              [selectedOutline, blendModeType, collectStyleContent],
            )}
          />
        </div>
      </div>
    </React.Fragment>
  )
})
