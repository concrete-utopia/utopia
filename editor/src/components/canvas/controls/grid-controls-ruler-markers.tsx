import React from 'react'
import { colorTheme } from '../../../uuiui'

export type RulerMarkerType = 'span-start' | 'span-end' | 'auto' | 'pinned'

const upFacingTriangle = (
  <svg width='11' height='11' xmlns='http://www.w3.org/2000/svg'>
    <polygon
      points='5,1 10,10 0,10'
      fill={colorTheme.primary.value}
      stroke={colorTheme.white.value}
      strokeWidth='0.5'
    />
  </svg>
)

const rightFacingTriangle = (
  <svg width='11' height='11' xmlns='http://www.w3.org/2000/svg'>
    <polygon
      points='10,5 0,0 0,10'
      fill={colorTheme.primary.value}
      stroke={colorTheme.white.value}
      strokeWidth='0.5'
    />
  </svg>
)

const downFacingTriangle = (
  <svg width='11' height='11' xmlns='http://www.w3.org/2000/svg'>
    <polygon
      points='5,10 0,0 10,0'
      fill={colorTheme.primary.value}
      stroke={colorTheme.white.value}
      strokeWidth='0.5'
    />
  </svg>
)

const leftFacingTriangle = (
  <svg width='11' height='11' xmlns='http://www.w3.org/2000/svg'>
    <polygon
      points='0,5 10,0 10,10'
      fill={colorTheme.primary.value}
      stroke={colorTheme.white.value}
      strokeWidth='0.5'
    />
  </svg>
)

const verticalPipe = (
  <svg width='4' height='11' xmlns='http://www.w3.org/2000/svg'>
    <rect
      x='0.25'
      y='0.25'
      width='3'
      height='10'
      fill={colorTheme.primary.value}
      stroke={colorTheme.white.value}
      strokeWidth='0.5'
    />
  </svg>
)

const horizontalPipe = (
  <svg width='11' height='11' xmlns='http://www.w3.org/2000/svg'>
    <rect
      x='0.25'
      y='3.50'
      width='10'
      height='3'
      fill={colorTheme.primary.value}
      stroke={colorTheme.white.value}
      strokeWidth='0.5'
    />
  </svg>
)

export const rulerMarkerIcons: {
  [key in RulerMarkerType]: { column: React.ReactNode; row: React.ReactNode }
} = {
  'span-start': {
    column: rightFacingTriangle,
    row: downFacingTriangle,
  },
  'span-end': {
    column: leftFacingTriangle,
    row: upFacingTriangle,
  },
  auto: {
    column: verticalPipe,
    row: horizontalPipe,
  },
  pinned: {
    column: downFacingTriangle,
    row: rightFacingTriangle,
  },
}
