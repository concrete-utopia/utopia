import { FramePin, FlexLength, LayoutSystem, FramePoint } from 'utopia-api/core'
import { PropertyPath, PropertyPathPart } from '../shared/project-file-types'
import * as PP from '../shared/property-path'
import { ElementInstanceMetadata } from '../shared/element-template'
import type { CSSNumber } from '../../components/inspector/common/css-utils'
import { ParsedCSSProperties } from '../../components/inspector/common/css-utils'

export type LayoutDimension = 'width' | 'height'

export type LayoutFlexContainerProp = LayoutDimension | 'gap'

export type LayoutFlexElementNumericProp = 'width' | 'height' | 'flexBasis'

export type LayoutFlexElementProp = LayoutFlexElementNumericProp

export type LayoutTargetableProp =
  | LayoutFlexElementProp
  | 'width'
  | 'height'
  | 'left'
  | 'top'
  | 'right'
  | 'bottom'
  | 'minWidth'
  | 'maxWidth'
  | 'minHeight'
  | 'maxHeight'
  | 'marginTop'
  | 'marginBottom'
  | 'marginLeft'
  | 'marginRight'
  | 'flexGrow'
  | 'flexShrink'

export type LayoutEdgeProp = 'left' | 'top' | 'right' | 'bottom'

export type LayoutPinnedProp = LayoutDimension | LayoutEdgeProp

export function layoutPinnedPropIsEdgeProp(prop: LayoutPinnedProp): prop is LayoutEdgeProp {
  return prop === 'left' || prop === 'right' || prop === 'top' || prop === 'bottom'
}

export type LayoutPinnedPropIncludingCenter = LayoutPinnedProp | 'centerX' | 'centerY'

export const VerticalLayoutPinnedProps: Array<LayoutPinnedProp> = ['top', 'bottom', 'height']
export function isVerticalLayoutPinnedProp(prop: LayoutPinnedProp): boolean {
  return VerticalLayoutPinnedProps.includes(prop)
}

export const HorizontalLayoutPinnedProps: Array<LayoutPinnedProp> = ['left', 'right', 'width']
export function isHorizontalLayoutPinnedProp(prop: LayoutPinnedProp): boolean {
  return HorizontalLayoutPinnedProps.includes(prop)
}

export const LayoutPinnedProps: Array<LayoutPinnedProp> = [
  ...VerticalLayoutPinnedProps,
  ...HorizontalLayoutPinnedProps,
]

export function isLayoutPinnedProp(prop: string): prop is LayoutPinnedProp {
  return (
    prop === 'left' ||
    prop === 'top' ||
    prop === 'right' ||
    prop === 'bottom' ||
    prop === 'width' ||
    prop === 'height'
  )
}

export type StyleLayoutProp =
  | LayoutTargetableProp
  | 'flex'
  | 'flexWrap'
  | 'flexDirection'
  | 'flexGrow'
  | 'flexShrink'
  | 'gap'
  | 'alignItems'
  | 'alignContent'
  | 'justifyContent'
  | 'padding'
  | 'paddingTop'
  | 'paddingRight'
  | 'paddingBottom'
  | 'paddingLeft'
  | 'alignSelf'
  | 'position'
  | 'left'
  | 'top'
  | 'right'
  | 'bottom'
  | 'minWidth'
  | 'maxWidth'
  | 'minHeight'
  | 'maxHeight'
  | 'marginTop'
  | 'marginRight'
  | 'marginBottom'
  | 'marginLeft'
  | 'margin'
  | 'display'
  | 'borderRadius'
  | 'borderTopLeftRadius'
  | 'borderTopRightRadius'
  | 'borderBottomLeftRadius'
  | 'borderBottomRightRadius'
  | 'zIndex'
  | 'rowGap'
  | 'columnGap'

export function framePointForPinnedProp(pinnedProp: LayoutPinnedProp): FramePoint {
  switch (pinnedProp) {
    case 'left':
      return FramePoint.Left
    case 'top':
      return FramePoint.Top
    case 'right':
      return FramePoint.Right
    case 'bottom':
      return FramePoint.Bottom
    case 'width':
      return FramePoint.Width
    case 'height':
      return FramePoint.Height
    default:
      const _exhaustiveCheck: never = pinnedProp
      throw new Error(`Unhandled prop ${JSON.stringify(pinnedProp)}`)
  }
}

export function pinnedPropForFramePoint(point: FramePoint): LayoutPinnedProp | null {
  switch (point) {
    case FramePoint.Left:
      return 'left'
    case FramePoint.Top:
      return 'top'
    case FramePoint.Right:
      return 'right'
    case FramePoint.Bottom:
      return 'bottom'
    case FramePoint.Width:
      return 'width'
    case FramePoint.Height:
      return 'height'
    case FramePoint.CenterX:
    case FramePoint.CenterY:
      return null
    default:
      const _exhaustiveCheck: never = point
      throw new Error(`Unhandled point ${JSON.stringify(point)}`)
  }
}

export interface LayoutPropertyTypes {
  width: CSSNumber | undefined
  height: CSSNumber | undefined

  gap: CSSNumber
  flexBasis: CSSNumber | undefined

  left: CSSNumber | undefined
  top: CSSNumber | undefined
  right: CSSNumber | undefined
  bottom: CSSNumber | undefined
}
