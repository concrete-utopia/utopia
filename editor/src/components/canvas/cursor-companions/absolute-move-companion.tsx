import React from 'react'
import { canvasPoint } from '../../../core/shared/math-utils'
import { CanvasOffsetWrapper } from '../controls/canvas-offset-wrapper'
import { useApplyCursorPositionToStyle } from '../controls/select-mode/pie-timer'

const CursorCompanionOffset = canvasPoint({ x: 15, y: -20 })

const SVGWidth = 18
const SVGHeight = 18

export const AbsoluteMoveCompanion = React.memo(() => {
  const elementRef = useApplyCursorPositionToStyle(CursorCompanionOffset)
  return (
    <CanvasOffsetWrapper>
      <div style={{ position: 'absolute', width: SVGWidth, height: SVGHeight }} ref={elementRef}>
        <ColorWhite />
      </div>
    </CanvasOffsetWrapper>
  )
})

const ColorBlack = React.memo(() => {
  return (
    <svg
      width={SVGWidth}
      height={SVGHeight}
      viewBox='0 0 65 65'
      fill='none'
      xmlns='http://www.w3.org/2000/svg'
    >
      <rect x='1.5' y='1.5' width='62' height='62' fill='black' stroke='white' strokeWidth='3' />
      <path
        d='M38.9393 28.0607C39.5251 28.6464 40.4749 28.6464 41.0607 28.0607L50.6066 18.5147C51.1924 17.9289 51.1924 16.9792 50.6066 16.3934C50.0208 15.8076 49.0711 15.8076 48.4853 16.3934L40 24.8787L31.5147 16.3934C30.9289 15.8076 29.9792 15.8076 29.3934 16.3934C28.8076 16.9792 28.8076 17.9289 29.3934 18.5147L38.9393 28.0607ZM38.5 3V27H41.5V3H38.5Z'
        fill='white'
      />
      <path
        d='M28.0607 42.0607C28.6464 41.4749 28.6464 40.5251 28.0607 39.9393L18.5147 30.3934C17.9289 29.8076 16.9792 29.8076 16.3934 30.3934C15.8076 30.9792 15.8076 31.9289 16.3934 32.5147L24.8787 41L16.3934 49.4853C15.8076 50.0711 15.8076 51.0208 16.3934 51.6066C16.9792 52.1924 17.9289 52.1924 18.5147 51.6066L28.0607 42.0607ZM3 42.5H27V39.5H3V42.5Z'
        fill='white'
      />
    </svg>
  )
})

const ColorWhite = React.memo(() => {
  return (
    <svg
      width={SVGWidth}
      height={SVGHeight}
      viewBox='0 0 65 65'
      fill='none'
      xmlns='http://www.w3.org/2000/svg'
    >
      <rect x='1.5' y='1.5' width='62' height='62' fill='white' stroke='black' strokeWidth='3' />
      <path
        d='M38.9393 28.0607C39.5251 28.6464 40.4749 28.6464 41.0607 28.0607L50.6066 18.5147C51.1924 17.9289 51.1924 16.9792 50.6066 16.3934C50.0208 15.8076 49.0711 15.8076 48.4853 16.3934L40 24.8787L31.5147 16.3934C30.9289 15.8076 29.9792 15.8076 29.3934 16.3934C28.8076 16.9792 28.8076 17.9289 29.3934 18.5147L38.9393 28.0607ZM38.5 3V27H41.5V3H38.5Z'
        fill='black'
      />
      <path
        d='M28.0607 42.0607C28.6464 41.4749 28.6464 40.5251 28.0607 39.9393L18.5147 30.3934C17.9289 29.8076 16.9792 29.8076 16.3934 30.3934C15.8076 30.9792 15.8076 31.9289 16.3934 32.5147L24.8787 41L16.3934 49.4853C15.8076 50.0711 15.8076 51.0208 16.3934 51.6066C16.9792 52.1924 17.9289 52.1924 18.5147 51.6066L28.0607 42.0607ZM3 42.5H27V39.5H3V42.5Z'
        fill='black'
      />
    </svg>
  )
})
