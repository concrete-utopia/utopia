import React from 'react'
import { canvasPoint } from '../../../core/shared/math-utils'
import { CanvasOffsetWrapper } from '../controls/canvas-offset-wrapper'
import { useApplyCursorPositionToStyle } from '../controls/select-mode/pie-timer'

const CursorCompanionOffset = canvasPoint({ x: 15, y: -15 })

const SVGWidth = 25
const SVGHeight = 12

export const ElementReorderCompanion = React.memo(() => {
  const elementRef = useApplyCursorPositionToStyle(CursorCompanionOffset)
  return (
    <CanvasOffsetWrapper>
      <div style={{ position: 'absolute', width: 25, height: 12 }} ref={elementRef}>
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
      viewBox='0 0 75 38'
      fill='none'
      xmlns='http://www.w3.org/2000/svg'
    >
      <path
        fillRule='evenodd'
        clipRule='evenodd'
        d='M18 19.0156H2V35.0156H18V19.0156ZM0 17.0156V37.0156H20V17.0156H0Z'
        fill='white'
      />
      <path
        d='M17.6548 21.1922L18.623 21.4421L18.873 20.4739C19.6059 17.6351 22.0416 13.8864 26.0428 10.8848C29.9891 7.92438 35.3136 5.8051 41.6714 6.01426C47.3597 6.20138 51.5661 8.29679 54.9132 11.6309C57.0803 13.7897 58.9193 16.5021 60.5369 19.6363L54.0863 16.8142C52.8214 16.2607 51.3473 16.8376 50.7939 18.1025C50.2404 19.3675 50.8173 20.8415 52.0822 21.3949L64.4503 26.806C65.7153 27.3594 67.1894 26.7826 67.7428 25.5177L73.1538 13.1495C73.7072 11.8846 73.1304 10.4105 71.8655 9.85709C70.6005 9.30367 69.1265 9.88049 68.573 11.1454L65.452 18.2794C63.5653 14.4453 61.298 10.9337 58.4419 8.08858C54.2393 3.90224 48.8464 1.24759 41.8358 1.01696C34.2251 0.76659 27.8027 3.31405 23.0423 6.88512C18.3371 10.4149 15.1061 15.0626 14.0317 19.224L13.7818 20.1922L14.75 20.4422L17.6548 21.1922Z'
        fill='black'
        stroke='white'
        strokeWidth='2'
      />
      <path d='M2 19H18V35H2V19Z' fill='black' />
      <rect x='28' y='18.0156' width='18' height='18' fill='black' stroke='white' strokeWidth='2' />
    </svg>
  )
})

const ColorWhite = React.memo(() => {
  return (
    <svg
      width={SVGWidth}
      height={SVGHeight}
      viewBox='0 0 77 37'
      fill='none'
      xmlns='http://www.w3.org/2000/svg'
    >
      <rect x='28' y='18' width='18' height='18' fill='white' stroke='black' strokeWidth='2' />
      <rect x='1' y='18' width='18' height='18' fill='white' stroke='black' strokeWidth='2' />
      <path
        d='M17.6548 21.1922L18.623 21.4421L18.873 20.4739C19.6059 17.6351 22.0416 13.8864 26.0428 10.8848C29.9891 7.92438 35.3136 5.8051 41.6714 6.01426C47.5293 6.20696 52.0193 7.9152 55.688 11.0185C58.212 13.1535 60.4304 16.0194 62.4301 19.6884L55.9804 16.9727C54.7079 16.437 53.242 17.0342 52.7062 18.3067C52.1704 19.5792 52.7676 21.0451 54.0402 21.5809L66.4822 26.8197C67.7547 27.3555 69.2207 26.7583 69.7565 25.4857L74.9952 13.0437C75.531 11.7712 74.9338 10.3052 73.6613 9.76944C72.3888 9.23364 70.9229 9.83088 70.3871 11.1034L67.3554 18.3036C65.0151 13.777 62.2706 10.0377 58.9171 7.20104C54.286 3.28368 48.6765 1.242 41.8358 1.01696C34.2251 0.76659 27.8027 3.31405 23.0423 6.88512C18.3371 10.4149 15.1061 15.0626 14.0317 19.224L13.7818 20.1922L14.75 20.4422L17.6548 21.1922Z'
        fill='white'
        stroke='black'
        strokeWidth='2'
      />
      <rect x='2' y='19' width='16' height='16' fill='white' />
    </svg>
  )
})
