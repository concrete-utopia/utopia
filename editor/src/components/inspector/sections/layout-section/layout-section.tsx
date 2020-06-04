import * as React from 'react'
import { CanvasRectangle, LocalRectangle } from '../../../../core/shared/math-utils'
import { betterReactMemo } from 'uuiui-deps'
import { LayoutSystemSubsection } from './layout-system-subsection/layout-system-subsection'
import { SelfLayoutSubsection } from './self-layout-subsection/self-layout-subsection'
import { CSSPosition } from '../../common/css-utils'
import { DetectedLayoutSystem } from '../../../../core/shared/element-template'

export interface ResolvedLayoutProps {
  frame: LocalRectangle | null
  parentFrame: CanvasRectangle | null
}

interface LayoutSectionProps {
  input: ResolvedLayoutProps
  isFlexComponent: boolean
  layoutSystem: DetectedLayoutSystem | null
  isChildOfFlexComponent: boolean
  position: CSSPosition
  hasNonDefaultPositionAttributes: boolean
  parentFlexAxis: 'horizontal' | 'vertical' | null
  aspectRatioLocked: boolean
  toggleAspectRatioLock: () => void
}

export const LayoutSection = betterReactMemo('LayoutSection', (props: LayoutSectionProps) => {
  return (
    <>
      <SelfLayoutSubsection
        input={props.input}
        position={props.position}
        isChildOfFlexComponent={props.isChildOfFlexComponent}
        parentFlexAxis={props.parentFlexAxis}
        aspectRatioLocked={props.aspectRatioLocked}
        toggleAspectRatioLock={props.toggleAspectRatioLock}
      />
      <LayoutSystemSubsection
        isFlexComponent={props.isFlexComponent}
        layoutSystem={props.layoutSystem}
      />
    </>
  )
})
