import React from 'react'
import { AlwaysTrue, usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'
import { canvasPoint } from '../../../core/shared/math-utils'
import { CompanionIconTypeAtom } from '../../editor/store/editor-state'
import { CanvasOffsetWrapper } from '../controls/canvas-offset-wrapper'
import { useApplyCursorPositionToStyle } from '../controls/select-mode/pie-timer'

const CursorCompanionOffset = canvasPoint({ x: 15, y: -20 })

const SVGWidth = 24
const SVGHeight = 24

export const AbsoluteMoveCompanion = React.memo(() => {
  const elementRef = useApplyCursorPositionToStyle(CursorCompanionOffset)
  const iconType = usePubSubAtomReadOnly(CompanionIconTypeAtom, AlwaysTrue).absoluteMove
  const IconForType = iconType === 1 ? ColorWhite : ColorBlack
  return (
    <CanvasOffsetWrapper>
      <div style={{ position: 'absolute', width: SVGWidth, height: SVGHeight }} ref={elementRef}>
        <IconForType />
      </div>
    </CanvasOffsetWrapper>
  )
})

const ColorBlack = React.memo(() => {
  return (
    <svg
      width={SVGWidth}
      height={SVGHeight}
      viewBox='0 0 73 73'
      fill='none'
      xmlns='http://www.w3.org/2000/svg'
    >
      <g filter='url(#filter0_d_108_6)'>
        <rect x={7.5} y={1.5} width={62} height={62} fill='black' stroke='white' strokeWidth={3} />
        <path
          d='M44.9393 28.0607C45.5251 28.6464 46.4749 28.6464 47.0607 28.0607L56.6066 18.5147C57.1924 17.9289 57.1924 16.9792 56.6066 16.3934C56.0208 15.8076 55.0711 15.8076 54.4853 16.3934L46 24.8787L37.5147 16.3934C36.9289 15.8076 35.9792 15.8076 35.3934 16.3934C34.8076 16.9792 34.8076 17.9289 35.3934 18.5147L44.9393 28.0607ZM44.5 3V27H47.5V3H44.5Z'
          fill='white'
        />
        <path
          d='M34.0607 42.0607C34.6464 41.4749 34.6464 40.5251 34.0607 39.9393L24.5147 30.3934C23.9289 29.8076 22.9792 29.8076 22.3934 30.3934C21.8076 30.9792 21.8076 31.9289 22.3934 32.5147L30.8787 41L22.3934 49.4853C21.8076 50.0711 21.8076 51.0208 22.3934 51.6066C22.9792 52.1924 23.9289 52.1924 24.5147 51.6066L34.0607 42.0607ZM9 42.5H33V39.5H9V42.5Z'
          fill='white'
        />
      </g>
      <defs>
        <filter
          id='filter0_d_108_6'
          x={0}
          y={0}
          width={73}
          height={73}
          filterUnits='userSpaceOnUse'
          colorInterpolationFilters='sRGB'
        >
          <feFlood floodOpacity={0} result='BackgroundImageFix' />
          <feColorMatrix
            in='SourceAlpha'
            type='matrix'
            values='0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 127 0'
            result='hardAlpha'
          />
          <feOffset dx={-2} dy={4} />
          <feGaussianBlur stdDeviation={2} />
          <feComposite in2='hardAlpha' operator='out' />
          <feColorMatrix type='matrix' values='0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.25 0' />
          <feBlend mode='normal' in2='BackgroundImageFix' result='effect1_dropShadow_108_6' />
          <feBlend mode='normal' in='SourceGraphic' in2='effect1_dropShadow_108_6' result='shape' />
        </filter>
      </defs>
    </svg>
  )
})

const ColorWhite = React.memo(() => {
  return (
    <svg
      width={SVGWidth}
      height={SVGHeight}
      viewBox='0 0 73 73'
      fill='none'
      xmlns='http://www.w3.org/2000/svg'
    >
      <g filter='url(#filter0_d_108_2)'>
        <rect x={7.5} y={1.5} width={62} height={62} fill='white' stroke='black' strokeWidth={3} />
        <path
          d='M44.9393 28.0607C45.5251 28.6464 46.4749 28.6464 47.0607 28.0607L56.6066 18.5147C57.1924 17.9289 57.1924 16.9792 56.6066 16.3934C56.0208 15.8076 55.0711 15.8076 54.4853 16.3934L46 24.8787L37.5147 16.3934C36.9289 15.8076 35.9792 15.8076 35.3934 16.3934C34.8076 16.9792 34.8076 17.9289 35.3934 18.5147L44.9393 28.0607ZM44.5 3V27H47.5V3H44.5Z'
          fill='black'
        />
        <path
          d='M34.0607 42.0607C34.6464 41.4749 34.6464 40.5251 34.0607 39.9393L24.5147 30.3934C23.9289 29.8076 22.9792 29.8076 22.3934 30.3934C21.8076 30.9792 21.8076 31.9289 22.3934 32.5147L30.8787 41L22.3934 49.4853C21.8076 50.0711 21.8076 51.0208 22.3934 51.6066C22.9792 52.1924 23.9289 52.1924 24.5147 51.6066L34.0607 42.0607ZM9 42.5H33V39.5H9V42.5Z'
          fill='black'
        />
      </g>
      <defs>
        <filter
          id='filter0_d_108_2'
          x={0}
          y={0}
          width={73}
          height={73}
          filterUnits='userSpaceOnUse'
          colorInterpolationFilters='sRGB'
        >
          <feFlood floodOpacity={0} result='BackgroundImageFix' />
          <feColorMatrix
            in='SourceAlpha'
            type='matrix'
            values='0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 127 0'
            result='hardAlpha'
          />
          <feOffset dx={-2} dy={4} />
          <feGaussianBlur stdDeviation={2} />
          <feComposite in2='hardAlpha' operator='out' />
          <feColorMatrix type='matrix' values='0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.25 0' />
          <feBlend mode='normal' in2='BackgroundImageFix' result='effect1_dropShadow_108_2' />
          <feBlend mode='normal' in='SourceGraphic' in2='effect1_dropShadow_108_2' result='shape' />
        </filter>
      </defs>
    </svg>
  )
})
