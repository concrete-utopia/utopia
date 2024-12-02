import React from 'react'
import type { UtopiColor } from '../../../uuiui'
import { RulerMarkerIconSize } from './grid-controls'

export type RulerMarkerType = 'span-start' | 'span-end' | 'auto' | 'auto-short' | 'pinned'

interface MarkerSVGProps {
  scale: number
}

function MarkerSVG({ scale, children }: React.PropsWithChildren<MarkerSVGProps>) {
  return (
    <svg
      width={`${RulerMarkerIconSize / scale}`}
      height={`${RulerMarkerIconSize / scale}`}
      viewBox={`0 0 ${RulerMarkerIconSize} ${RulerMarkerIconSize}`}
      xmlns='http://www.w3.org/2000/svg'
    >
      {children}
    </svg>
  )
}

function upFacingTriangle(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <MarkerSVG scale={scale}>
      <polygon
        points={`${
          RulerMarkerIconSize / 2
        },0 ${RulerMarkerIconSize},${RulerMarkerIconSize} 0,${RulerMarkerIconSize}`}
        fill={fillColor.value}
      />
    </MarkerSVG>
  )
}

function rightFacingTriangle(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <MarkerSVG scale={scale}>
      <polygon
        points={`${RulerMarkerIconSize},${RulerMarkerIconSize / 2} 0,0 0,${RulerMarkerIconSize}`}
        fill={fillColor.value}
      />
    </MarkerSVG>
  )
}

function downFacingTriangle(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <MarkerSVG scale={scale}>
      <polygon
        points={`${RulerMarkerIconSize / 2},${RulerMarkerIconSize} 0,0 ${RulerMarkerIconSize},0`}
        fill={fillColor.value}
      />
    </MarkerSVG>
  )
}

function leftFacingTriangle(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <MarkerSVG scale={scale}>
      <polygon
        points={`0,${
          RulerMarkerIconSize / 2
        } ${RulerMarkerIconSize},0 ${RulerMarkerIconSize},${RulerMarkerIconSize}`}
        fill={fillColor.value}
      />
    </MarkerSVG>
  )
}

function regularVerticalPipe(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <MarkerSVG scale={scale}>
      <rect x='4' y='0' width='3' height={`${RulerMarkerIconSize}`} fill={fillColor.value} />
    </MarkerSVG>
  )
}

function regularHorizontalPipe(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <MarkerSVG scale={scale}>
      <rect x='0' y='4' width={`${RulerMarkerIconSize}`} height='3' fill={fillColor.value} />
    </MarkerSVG>
  )
}

function shortVerticalPipe(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <MarkerSVG scale={scale}>
      <rect x='4' y='2' width='3' height='9' fill={fillColor.value} />
    </MarkerSVG>
  )
}

function shortHorizontalPipe(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <MarkerSVG scale={scale}>
      <rect x='2' y='4' width='9' height='3' fill={fillColor.value} />
    </MarkerSVG>
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
