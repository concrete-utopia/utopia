import React from 'react'
import type { UtopiColor } from '../../../uuiui'

export type RulerMarkerType = 'span-start' | 'span-end' | 'auto' | 'auto-short' | 'pinned'

function upFacingTriangle(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <svg
      width={`${11 / scale}`}
      height={`${11 / scale}`}
      viewBox={`0 0 11 11`}
      xmlns='http://www.w3.org/2000/svg'
    >
      <polygon points='5.5,0 11,11 0,11' fill={fillColor.value} />
    </svg>
  )
}

function rightFacingTriangle(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <svg
      width={`${11 / scale}`}
      height={`${11 / scale}`}
      viewBox={`0 0 11 11`}
      xmlns='http://www.w3.org/2000/svg'
    >
      <polygon points='11,5.5 0,0 0,11' fill={fillColor.value} />
    </svg>
  )
}

function downFacingTriangle(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <svg
      width={`${11 / scale}`}
      height={`${11 / scale}`}
      viewBox={`0 0 11 11`}
      xmlns='http://www.w3.org/2000/svg'
    >
      <polygon points='5.5,11 0,0 11,0' fill={fillColor.value} />
    </svg>
  )
}

function leftFacingTriangle(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <svg
      width={`${11 / scale}`}
      height={`${11 / scale}`}
      viewBox={`0 0 11 11`}
      xmlns='http://www.w3.org/2000/svg'
    >
      <polygon points='0,5.5 11,0 11,11' fill={fillColor.value} />
    </svg>
  )
}

function regularVerticalPipe(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <svg
      width={`${11 / scale}`}
      height={`${11 / scale}`}
      viewBox={`0 0 11 11`}
      xmlns='http://www.w3.org/2000/svg'
    >
      <rect x='4' y='0' width='3' height='11' fill={fillColor.value} />
    </svg>
  )
}

function regularHorizontalPipe(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <svg
      width={`${11 / scale}`}
      height={`${11 / scale}`}
      viewBox={`0 0 11 11`}
      xmlns='http://www.w3.org/2000/svg'
    >
      <rect x='0' y='4' width='11' height='3' fill={fillColor.value} />
    </svg>
  )
}

function shortVerticalPipe(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <svg
      width={`${11 / scale}`}
      height={`${11 / scale}`}
      viewBox={`0 0 11 11`}
      xmlns='http://www.w3.org/2000/svg'
    >
      <rect x='4' y='2' width='3' height='9' fill={fillColor.value} />
    </svg>
  )
}

function shortHorizontalPipe(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <svg
      width={`${11 / scale}`}
      height={`${11 / scale}`}
      viewBox={`0 0 11 11`}
      xmlns='http://www.w3.org/2000/svg'
    >
      <rect x='2' y='4' width='9' height='3' fill={fillColor.value} />
    </svg>
  )
}

type ColorToReactNode = (fillColor: UtopiColor, scale: number) => React.ReactNode

export const rulerMarkerIcons: {
  [key in RulerMarkerType]: { column: ColorToReactNode; row: ColorToReactNode }
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
    column: regularVerticalPipe,
    row: regularHorizontalPipe,
  },
  'auto-short': {
    column: shortVerticalPipe,
    row: shortHorizontalPipe,
  },
  pinned: {
    column: downFacingTriangle,
    row: rightFacingTriangle,
  },
}
