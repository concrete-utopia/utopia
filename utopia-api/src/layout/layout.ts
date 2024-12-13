import type {
  FlexParentProps,
  FlexElementProps,
  FlexPosition,
  FlexAlignment,
  FlexDirection,
  FlexJustifyContent,
  FlexWrap,
  Sides,
  FlexLength,
} from './flex'
import type { Frame, FramePin } from './frame'

export enum LayoutSystem {
  PinSystem = 'pinSystem',
  Group = 'group',
}

export interface LayoutProps extends Frame, FlexElementProps, FlexParentProps {
  layoutSystem?: LayoutSystem
}

export interface LayoutBaseProps {
  // pinSystem
  left?: FramePin
  right?: FramePin
  width?: FramePin
  top?: FramePin
  bottom?: FramePin
  height?: FramePin
  // flex element
  position?: FlexPosition
  minWidth?: number
  maxWidth?: number
  minHeight?: number
  maxHeight?: number
  margin?: Partial<Sides>
  alignSelf?: FlexAlignment
  flex?: number
  flexGrow?: number
  flexShrink?: number
  // flex parent
  flexDirection?: FlexDirection
  alignContent?: FlexJustifyContent
  alignItems?: FlexAlignment
  justifyContent?: FlexJustifyContent
  wrap?: FlexWrap
  padding?: Sides
}

export interface LayoutMagicProps {
  // pinSystem
  layoutSystem?: LayoutSystem
  centerX?: FramePin
  centerY?: FramePin
  // flex element
  flexBasis?: FlexLength // this is a magic prop because we might omit it from style if fixedWidth or fixedHeight is set
  crossBasis?: FlexLength
  // flex parent
  gapCross?: number
  gapMain?: number
}

type LayoutBasePropsJS = {
  [key in keyof Required<LayoutBaseProps>]: boolean
}
// this is a horrible Typescript hack so that we can enforce that AllLayoutBasePropsKeys is an exhaustive list
const AllLayoutBaseProps: LayoutBasePropsJS = {
  left: true,
  right: true,
  width: true,
  top: true,
  bottom: true,
  height: true,
  position: true,
  minWidth: true,
  maxWidth: true,
  minHeight: true,
  maxHeight: true,
  margin: true,
  alignSelf: true,
  flex: true,
  flexGrow: true,
  flexShrink: true,
  flexDirection: true,
  alignContent: true,
  alignItems: true,
  justifyContent: true,
  wrap: true,
  padding: true,
}

export const AllLayoutBasePropsKeys = Object.keys(AllLayoutBaseProps)
