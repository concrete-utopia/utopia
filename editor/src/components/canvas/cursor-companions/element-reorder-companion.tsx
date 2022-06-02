import React from 'react'
import { AlwaysTrue, usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'
import { canvasPoint } from '../../../core/shared/math-utils'
import { CompanionIconTypeAtom } from '../../editor/store/editor-state'
import { CanvasOffsetWrapper } from '../controls/canvas-offset-wrapper'
import { useApplyCursorPositionToStyle } from '../controls/select-mode/pie-timer'

const CursorCompanionOffset = canvasPoint({ x: 15, y: -15 })

const SVGWidth = 31
const SVGHeight = 16

export const ElementReorderCompanion = React.memo(() => {
  const elementRef = useApplyCursorPositionToStyle(CursorCompanionOffset)
  const iconType = usePubSubAtomReadOnly(CompanionIconTypeAtom, AlwaysTrue).flexReorder
  const IconForType = iconType === 1 ? ColorWhite : ColorBlack
  return (
    <CanvasOffsetWrapper>
      <div style={{ position: 'absolute', width: 25, height: 12 }} ref={elementRef}>
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
      viewBox='0 0 95 48'
      fill='none'
      xmlns='http://www.w3.org/2000/svg'
    >
      <g filter='url(#filter0_d_109_23)'>
        <rect x={34} y={21} width={18} height={18} fill='black' stroke='white' strokeWidth={2} />
        <rect x={7} y={21} width={18} height={18} fill='white' stroke='white' strokeWidth={2} />
        <path
          d='M23.2327 24.9737L24.2037 25.2261L24.4529 24.2543C25.3464 20.7701 28.3013 16.2063 33.1286 12.5607C37.8947 8.96133 44.3243 6.38575 51.9986 6.6399C59.0627 6.87385 64.4923 8.94941 68.9314 12.7295C72.0997 15.4274 74.8633 19.0789 77.3422 23.7741L69.0807 20.2722C67.6505 19.666 66.0091 20.3444 65.4114 21.7735L66.3339 22.1594L65.4114 21.7735C64.8151 23.1992 65.4771 24.8476 66.9037 25.4523L81.8279 31.7783C83.258 32.3845 84.8995 31.7061 85.4972 30.277L91.7811 15.2528C92.3774 13.8271 91.7154 12.1787 90.2888 11.574C88.8587 10.9678 87.2172 11.6462 86.6195 13.0753L82.8178 22.1649C79.9773 16.5447 76.6417 11.9236 72.552 8.44098C67.0356 3.74347 60.351 1.29087 52.1831 1.02037C43.1022 0.719639 35.4413 3.77927 29.7639 8.06686C24.1478 12.3081 20.3056 17.8832 19.0313 22.8519L18.7837 23.8174L19.7485 24.0682L23.2327 24.9737Z'
          fill='black'
          stroke='white'
          strokeWidth={2}
        />
        <rect x={8} y={22} width={16} height={16} fill='black' />
      </g>
      <defs>
        <filter
          id='filter0_d_109_23'
          x={0}
          y={-0.0000152588}
          width={95.0002}
          height={48}
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
          <feBlend mode='normal' in2='BackgroundImageFix' result='effect1_dropShadow_109_23' />
          <feBlend
            mode='normal'
            in='SourceGraphic'
            in2='effect1_dropShadow_109_23'
            result='shape'
          />
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
      viewBox='0 0 95 48'
      fill='none'
      xmlns='http://www.w3.org/2000/svg'
    >
      <g filter='url(#filter0_d_108_10)'>
        <rect x={34} y={21} width={18} height={18} fill='white' stroke='black' strokeWidth={2} />
        <rect x={7} y={21} width={18} height={18} fill='white' stroke='black' strokeWidth={2} />
        <path
          d='M23.2327 24.9737L24.2037 25.2261L24.4529 24.2543C25.3464 20.7701 28.3013 16.2063 33.1286 12.5607C37.8947 8.96133 44.3243 6.38575 51.9986 6.6399C59.0627 6.87385 64.4923 8.94941 68.9314 12.7295C72.0997 15.4274 74.8633 19.0789 77.3422 23.7741L69.0807 20.2722C67.6505 19.666 66.0091 20.3444 65.4114 21.7735L66.3339 22.1594L65.4114 21.7735C64.8151 23.1992 65.4771 24.8476 66.9037 25.4523L81.8279 31.7783C83.258 32.3845 84.8995 31.7061 85.4972 30.277L91.7811 15.2528C92.3774 13.8271 91.7154 12.1787 90.2888 11.574C88.8587 10.9678 87.2172 11.6462 86.6195 13.0753L82.8178 22.1649C79.9773 16.5447 76.6417 11.9236 72.552 8.44098C67.0356 3.74347 60.351 1.29087 52.1831 1.02037C43.1022 0.719639 35.4413 3.77927 29.7639 8.06686C24.1478 12.3081 20.3056 17.8832 19.0313 22.8519L18.7837 23.8174L19.7485 24.0682L23.2327 24.9737Z'
          fill='white'
          stroke='black'
          strokeWidth={2}
        />
        <rect x={8} y={22} width={16} height={16} fill='white' />
      </g>
      <defs>
        <filter
          id='filter0_d_108_10'
          x={0}
          y={-0.0000152588}
          width={95.0002}
          height={48}
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
          <feBlend mode='normal' in2='BackgroundImageFix' result='effect1_dropShadow_108_10' />
          <feBlend
            mode='normal'
            in='SourceGraphic'
            in2='effect1_dropShadow_108_10'
            result='shape'
          />
        </filter>
      </defs>
    </svg>
  )
})
