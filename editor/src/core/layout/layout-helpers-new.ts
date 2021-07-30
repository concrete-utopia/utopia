import { FramePin, FlexLength, LayoutSystem, FramePoint } from 'utopia-api'
import { PropertyPath, PropertyPathPart } from '../shared/project-file-types'
import * as PP from '../shared/property-path'
import { ElementInstanceMetadata } from '../shared/element-template'
import { CSSNumber, ParsedCSSProperties } from '../../components/inspector/common/css-utils'

export type LayoutDimension = 'Width' | 'Height'

export type LayoutFlexContainerProp = LayoutDimension | 'FlexGap'

export type LayoutFlexElementNumericProp = 'Width' | 'Height' | 'flexBasis' | 'FlexCrossBasis'

export type LayoutFlexElementProp = LayoutFlexElementNumericProp

export type LayoutTargetableProp =
  | LayoutFlexElementProp
  | 'Width'
  | 'Height'
  | 'minWidth'
  | 'maxWidth'
  | 'minHeight'
  | 'maxHeight'
  | 'marginTop'
  | 'marginBottom'
  | 'marginLeft'
  | 'marginRight'

export type LayoutPinnedProp =
  | LayoutDimension
  | 'PinnedLeft'
  | 'PinnedTop'
  | 'PinnedRight'
  | 'PinnedBottom'
  | 'PinnedCenterX'
  | 'PinnedCenterY'

export const LayoutPinnedProps: Array<LayoutPinnedProp> = [
  'Width',
  'Height',
  'PinnedLeft',
  'PinnedTop',
  'PinnedRight',
  'PinnedBottom',
  'PinnedCenterX',
  'PinnedCenterY',
]

export type StyleLayoutProp =
  | 'flex'
  | 'flexWrap'
  | 'flexDirection'
  | 'flexGrow'
  | 'flexShrink'
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

export type LayoutProp =
  | 'LayoutSystem'
  | LayoutPinnedProp
  | LayoutFlexContainerProp
  | LayoutFlexElementProp

export function framePointForPinnedProp(pinnedProp: LayoutPinnedProp): FramePoint {
  switch (pinnedProp) {
    case 'PinnedLeft':
      return FramePoint.Left
    case 'PinnedTop':
      return FramePoint.Top
    case 'PinnedRight':
      return FramePoint.Right
    case 'PinnedBottom':
      return FramePoint.Bottom
    case 'PinnedCenterX':
      return FramePoint.CenterX
    case 'PinnedCenterY':
      return FramePoint.CenterY
    case 'Width':
      return FramePoint.Width
    case 'Height':
      return FramePoint.Height
    default:
      const _exhaustiveCheck: never = pinnedProp
      throw new Error(`Unhandled prop ${JSON.stringify(pinnedProp)}`)
  }
}

export function pinnedPropForFramePoint(point: FramePoint): LayoutPinnedProp {
  switch (point) {
    case FramePoint.Left:
      return 'PinnedLeft'
    case FramePoint.Top:
      return 'PinnedTop'
    case FramePoint.Right:
      return 'PinnedRight'
    case FramePoint.Bottom:
      return 'PinnedBottom'
    case FramePoint.CenterX:
      return 'PinnedCenterX'
    case FramePoint.CenterY:
      return 'PinnedCenterY'
    case FramePoint.Width:
      return 'Width'
    case FramePoint.Height:
      return 'Height'
    default:
      const _exhaustiveCheck: never = point
      throw new Error(`Unhandled point ${JSON.stringify(point)}`)
  }
}

const LayoutPathMap: { [key in LayoutProp | StyleLayoutProp]: Array<PropertyPathPart> } = {
  // TODO LAYOUT remove these once no place uses it
  LayoutSystem: ['layout', 'layoutSystem'],
  PinnedCenterX: ['layout', 'centerX'],
  PinnedCenterY: ['layout', 'centerY'],
  FlexCrossBasis: ['layout', 'crossBasis'],

  // TODO FIXME 'style' here should point to the inspector target selector's current target instead of always pointing to style
  FlexGap: ['style', 'gap'],
  flexBasis: ['style', 'flexBasis'],
  PinnedLeft: ['style', 'left'],
  PinnedTop: ['style', 'top'],
  Width: ['style', 'width'],
  Height: ['style', 'height'],
  PinnedRight: ['style', 'right'],
  PinnedBottom: ['style', 'bottom'],

  alignSelf: ['style', 'alignSelf'],
  flexWrap: ['style', 'wrap'],
  flexDirection: ['style', 'flexDirection'],
  flex: ['style', 'flex'],
  flexGrow: ['style', 'flexGrow'],
  flexShrink: ['style', 'flexShrink'],
  alignItems: ['style', 'alignItems'],
  alignContent: ['style', 'alignContent'],
  justifyContent: ['style', 'justifyContent'],
  position: ['style', 'position'],
  left: ['style', 'left'],
  top: ['style', 'top'],
  right: ['style', 'right'],
  bottom: ['style', 'bottom'],
  minWidth: ['style', 'minWidth'],
  maxWidth: ['style', 'maxWidth'],
  minHeight: ['style', 'minHeight'],
  maxHeight: ['style', 'maxHeight'],
  marginTop: ['style', 'marginTop'],
  marginRight: ['style', 'marginRight'],
  marginBottom: ['style', 'marginBottom'],
  marginLeft: ['style', 'marginLeft'],
  margin: ['style', 'margin'],
  padding: ['style', 'padding'],
  paddingTop: ['style', 'paddingTop'],
  paddingRight: ['style', 'paddingRight'],
  paddingBottom: ['style', 'paddingBottom'],
  paddingLeft: ['style', 'paddingLeft'],
  display: ['style', 'display'],
}

export interface LayoutPropertyTypes {
  LayoutSystem: LayoutSystem | undefined

  Width: CSSNumber | undefined
  Height: CSSNumber | undefined

  FlexGap: number
  flexBasis: CSSNumber | undefined
  FlexCrossBasis: CSSNumber | undefined

  PinnedLeft: CSSNumber | undefined
  PinnedTop: CSSNumber | undefined
  PinnedRight: CSSNumber | undefined
  PinnedBottom: CSSNumber | undefined
  PinnedCenterX: CSSNumber | undefined
  PinnedCenterY: CSSNumber | undefined
}

export interface LayoutPropertyTypesAndCSSPropertyTypes extends ParsedCSSProperties {
  LayoutSystem: LayoutSystem | undefined
  Width: FramePin | undefined
  Height: FramePin | undefined

  FlexGap: number
  flexBasis: FlexLength
  FlexCrossBasis: FlexLength

  PinnedLeft: FramePin | undefined
  PinnedTop: FramePin | undefined
  PinnedRight: FramePin | undefined
  PinnedBottom: FramePin | undefined
  PinnedCenterX: FramePin | undefined
  PinnedCenterY: FramePin | undefined
}

export function createLayoutPropertyPath(layoutProp: LayoutProp | StyleLayoutProp): PropertyPath {
  return PP.create(LayoutPathMap[layoutProp])
}

export function createLayoutPropertyPathString(layoutProp: LayoutProp | StyleLayoutProp): string {
  return PP.toString(createLayoutPropertyPath(layoutProp))
}

export function getObservedLayoutPixelValue(
  pin: LayoutPinnedProp,
  elementInstanceMetadata: ElementInstanceMetadata,
): number | null {
  if (elementInstanceMetadata.globalFrame == null || elementInstanceMetadata.localFrame == null) {
    return null
  }
  const parentFrame = elementInstanceMetadata.localFrame
  const elementFrame = elementInstanceMetadata.localFrame
  switch (pin) {
    case 'PinnedLeft':
      return elementFrame.x
    case 'PinnedTop':
      return elementFrame.y
    case 'Width':
      return elementFrame.width
    case 'Height':
      return elementFrame.height
    case 'PinnedRight':
      return parentFrame.width - (elementFrame.x + elementFrame.width)
    case 'PinnedBottom':
      return parentFrame.height - (elementFrame.y + elementFrame.height)
    case 'PinnedCenterX':
      return parentFrame.width / 2 - elementFrame.width / 2 + elementFrame.x
    case 'PinnedCenterY':
      return parentFrame.height / 2 - elementFrame.width / 2 + elementFrame.x
  }
}
