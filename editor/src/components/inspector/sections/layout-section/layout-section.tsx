import * as React from 'react'
import { CanvasRectangle, LocalRectangle } from '../../../../core/shared/math-utils'
import { betterReactMemo } from 'uuiui-deps'
import { LayoutSystemSubsection } from './layout-system-subsection/layout-system-subsection'
import { SelfLayoutSubsection } from './self-layout-subsection/self-layout-subsection'
import { CSSPosition } from '../../common/css-utils'
import {
  DetectedLayoutSystem,
  SpecialSizeMeasurements,
} from '../../../../core/shared/element-template'
import { useRecoilState } from 'recoil'
import { layoutHoveredState } from '../../../../core/shared/inspector-recoil'

export interface ResolvedLayoutProps {
  frame: LocalRectangle | null
  parentFrame: CanvasRectangle | null
}

interface LayoutSectionProps {
  input: ResolvedLayoutProps
  specialSizeMeasurements: SpecialSizeMeasurements
  isChildOfFlexComponent: boolean
  position: CSSPosition | null
  hasNonDefaultPositionAttributes: boolean
  parentFlexAxis: 'horizontal' | 'vertical' | null
  aspectRatioLocked: boolean
  toggleAspectRatioLock: () => void
}

export const LayoutSection = betterReactMemo('LayoutSection', (props: LayoutSectionProps) => {
  const [, setLayoutHovered] = useRecoilState(layoutHoveredState)

  return (
    <div
      onMouseOver={() => {
        setLayoutHovered(true)
      }}
      onMouseOut={() => {
        setLayoutHovered(false)
      }}
    >
      <SelfLayoutSubsection
        input={props.input}
        position={props.position}
        isChildOfFlexComponent={props.isChildOfFlexComponent}
        parentFlexAxis={props.parentFlexAxis}
        aspectRatioLocked={props.aspectRatioLocked}
        toggleAspectRatioLock={props.toggleAspectRatioLock}
      />
      <LayoutSystemSubsection specialSizeMeasurements={props.specialSizeMeasurements} />
    </div>
  )
})
