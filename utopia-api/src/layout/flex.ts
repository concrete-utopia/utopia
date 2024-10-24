import { defaultIfNull } from '../utils'
import type { LayoutProps } from './layout'

export interface Sides {
  top?: number
  right?: number
  bottom?: number
  left?: number
}

export function sides(
  top: number | undefined,
  right: number | undefined,
  bottom: number | undefined,
  left: number | undefined,
): Sides {
  let sidesObject: Sides = {}
  if (top != null) {
    sidesObject.top = top
  }
  if (right != null) {
    sidesObject.right = right
  }
  if (bottom != null) {
    sidesObject.bottom = bottom
  }
  if (left != null) {
    sidesObject.left = left
  }
  return sidesObject
}

export type FlexLength = number | string | undefined

export interface FlexElementProps {
  position?: FlexPosition
  left?: number | string
  top?: number | string
  right?: number | string
  bottom?: number | string
  minWidth?: number | string
  maxWidth?: number | string
  minHeight?: number | string
  maxHeight?: number | string
  marginLeft?: number | string
  marginTop?: number | string
  marginRight?: number | string
  marginBottom?: number | string
  alignSelf?: FlexAlignment
  flexGrow?: number
  flexShrink?: number
  flexBasis?: FlexLength
  crossBasis?: FlexLength
}

// this is where we need to pass the right parent props
export function flexElementPropsToStyle(
  props: FlexElementProps,
  parentProps: FlexParentProps,
  index: number,
  siblingsCount: number,
): Partial<React.CSSProperties> {
  return {
    position: props.position,
    flexGrow: props.flexGrow,
    flexShrink: props.flexShrink,
    minWidth: props.minWidth,
    maxWidth: props.maxWidth,
    minHeight: props.minHeight,
    maxHeight: props.maxHeight,
    alignSelf: props.alignSelf,
    ...getFlexSize(props, parentProps),
    ...getTLBRProps(props),
    ...getMarginProps(props, parentProps, index, siblingsCount),
  }
}

type Axis = 'horizontal' | 'vertical'

export function getMainAxis(props: FlexParentProps): Axis {
  const { flexDirection } = props
  if (flexDirection === 'column' || flexDirection === 'column-reverse') {
    return 'vertical'
  } else {
    return 'horizontal'
  }
}

export function getCrossAxis(props: FlexParentProps): Axis {
  return getMainAxis(props) === 'horizontal' ? 'vertical' : 'horizontal'
}

function valueToUse(crossBasis: FlexLength, stretches: boolean, onCrossAxis: boolean): FlexLength {
  if (onCrossAxis && !stretches) {
    return crossBasis
  } else {
    return undefined
  }
}

export const flexWidthValueToUse = (crossBasis: FlexLength, stretches: boolean, crossAxis: Axis) =>
  valueToUse(crossBasis, stretches, crossAxis === 'horizontal')

export const flexHeightValueToUse = (crossBasis: FlexLength, stretches: boolean, crossAxis: Axis) =>
  valueToUse(crossBasis, stretches, crossAxis === 'vertical')

export function getUnstretchedWidthHeight(
  props: LayoutProps,
  parentProps: FlexParentProps,
): { width: FlexLength; height: FlexLength } {
  const axis = getMainAxis(parentProps)
  const { crossBasis, flexBasis, width, height } = props
  const widthBasis = axis === 'horizontal' ? flexBasis : crossBasis
  const heightBasis = axis === 'vertical' ? flexBasis : crossBasis
  return {
    width: widthBasis || width,
    height: heightBasis || height,
  }
}

export function getFlexSize(
  props: FlexElementProps,
  parentProps: FlexParentProps,
): Partial<React.CSSProperties> {
  // cross basis values are dropped when stretching
  const axis = getMainAxis(parentProps)
  const crossAxis = getCrossAxis(parentProps)
  const stretches = stretchesChild(parentProps, props)
  const { crossBasis, flexBasis } = props
  const width = flexWidthValueToUse(crossBasis, stretches, crossAxis)
  const height = flexHeightValueToUse(crossBasis, stretches, crossAxis)
  return {
    flexBasis: flexBasis,
    width: width,
    height: height,
  }
}

export function getTLBRProps(props: FlexElementProps): Partial<React.CSSProperties> {
  /**
    only copy left top right bottom to style if props.layout.position is defined!

    Because if our children is a layoutSystem: pins, it will give itself a style.position: relative,
    any offset such as left, top, right bottom that is provided to props.style will be active
    */

  return {
    left: props.position == null ? undefined : props.left,
    top: props.position == null ? undefined : props.top,
    right: props.position == null ? undefined : props.right,
    bottom: props.position == null ? undefined : props.bottom,
  }
}

export function getMarginProps(
  props: FlexElementProps,
  parentProps: FlexParentProps,
  index: number,
  siblingsCount: number,
): Partial<React.CSSProperties> {
  const elementGap = getElementGapFromParent(parentProps, index, siblingsCount)
  return {
    marginLeft: defaultIfNull<string | number | undefined>(elementGap.left, props.marginLeft),
    marginTop: defaultIfNull<string | number | undefined>(elementGap.top, props.marginTop),
    marginRight: defaultIfNull<string | number | undefined>(elementGap.right, props.marginRight),
    marginBottom: defaultIfNull<string | number | undefined>(elementGap.bottom, props.marginBottom),
  }
}

export interface FlexParentProps {
  flexDirection?: FlexDirection
  alignContent?: FlexJustifyContent
  alignItems?: FlexAlignment
  justifyContent?: FlexJustifyContent
  flexWrap?: FlexWrap
  gapMain?: number
  gapCross?: number
  paddingLeft?: string | number
  paddingTop?: string | number
  paddingRight?: string | number
  paddingBottom?: string | number
}

export function flexParentPropsToStyle(props: FlexParentProps): Partial<React.CSSProperties> {
  const styleToReturn: Partial<React.CSSProperties> = {
    flexDirection: props.flexDirection,
    alignContent: props.alignContent,
    alignItems: props.alignItems,
    justifyContent: props.justifyContent,
    flexWrap: props.flexWrap,
  }
  return styleToReturn
}

export enum FlexAlignment {
  Auto = 'auto',
  FlexStart = 'flex-start',
  Center = 'center',
  FlexEnd = 'flex-end',
  Stretch = 'stretch',
  Baseline = 'baseline',
  FirstBaseline = 'first baseline',
  LastBaseline = 'last baseline',
  SafeCenter = 'safe center',
  UnsafeCenter = 'unsafe center',
}

export const AllFlexAlignments: Array<FlexAlignment> = [
  FlexAlignment.Auto,
  FlexAlignment.FlexStart,
  FlexAlignment.Center,
  FlexAlignment.FlexEnd,
  FlexAlignment.Stretch,
  FlexAlignment.Baseline,
  FlexAlignment.FirstBaseline,
  FlexAlignment.LastBaseline,
  FlexAlignment.SafeCenter,
  FlexAlignment.UnsafeCenter,
]

export enum FlexJustifyContent {
  FlexStart = 'flex-start',
  Center = 'center',
  FlexEnd = 'flex-end',
  SpaceAround = 'space-around',
  SpaceBetween = 'space-between',
  SpaceEvenly = 'space-evenly',
  Stretch = 'stretch',
  Normal = 'normal',
  SafeCenter = 'safe center',
  UnsafeCenter = 'unsafe center',
}

export const AllFlexJustifyContents: Array<FlexJustifyContent> = [
  FlexJustifyContent.FlexStart,
  FlexJustifyContent.Center,
  FlexJustifyContent.FlexEnd,
  FlexJustifyContent.SpaceAround,
  FlexJustifyContent.SpaceBetween,
  FlexJustifyContent.SpaceEvenly,
  FlexJustifyContent.Stretch,
  FlexJustifyContent.Normal,
  FlexJustifyContent.SafeCenter,
  FlexJustifyContent.UnsafeCenter,
]

export enum FlexDirection {
  Column = 'column',
  ColumnReverse = 'column-reverse',
  Row = 'row',
  RowReverse = 'row-reverse',
}

export const AllFlexDirections: Array<FlexDirection> = [
  FlexDirection.Column,
  FlexDirection.ColumnReverse,
  FlexDirection.Row,
  FlexDirection.RowReverse,
]

export enum FlexWrap {
  NoWrap = 'nowrap',
  Wrap = 'wrap',
  WrapReverse = 'wrap-reverse',
}

export const AllFlexWraps: Array<FlexWrap> = [FlexWrap.NoWrap, FlexWrap.Wrap, FlexWrap.WrapReverse]

export type FlexPosition = 'absolute' | 'relative'

function getElementGapFromParent(
  parentProps: FlexParentProps,
  index: number,
  siblingsCount: number,
): Sides {
  if (parentProps.gapMain == null && parentProps.gapCross == null) {
    return {
      left: undefined,
      top: undefined,
      right: undefined,
      bottom: undefined,
    }
  }
  const gapMainAxis: number = defaultIfNull<number>(0, parentProps.gapMain)
  const gapCrossAxis: number = defaultIfNull<number>(0, parentProps.gapCross)
  const elementGapBaseMain = gapMainAxis / 2

  // first child, last child has gap only inside
  // reversed direction has gap on the other side
  const isFirstChild = index === 0
  const isLastChild = index === siblingsCount - 1

  const flexDirection = defaultIfNull(FlexDirection.Row, parentProps.flexDirection)

  switch (flexDirection) {
    case FlexDirection.Column:
      return {
        left: gapCrossAxis,
        top: isFirstChild ? 0 : elementGapBaseMain,
        right: gapCrossAxis,
        bottom: isLastChild ? 0 : elementGapBaseMain,
      }
    case FlexDirection.ColumnReverse:
      return {
        left: gapCrossAxis,
        top: isLastChild ? 0 : elementGapBaseMain,
        right: gapCrossAxis,
        bottom: isFirstChild ? 0 : elementGapBaseMain,
      }
    case FlexDirection.RowReverse:
      return {
        left: isLastChild ? 0 : elementGapBaseMain,
        top: gapCrossAxis,
        right: isFirstChild ? 0 : elementGapBaseMain,
        bottom: gapCrossAxis,
      }

    case FlexDirection.Row:
    default:
      return {
        left: isFirstChild ? 0 : elementGapBaseMain,
        top: gapCrossAxis,
        right: isLastChild ? 0 : elementGapBaseMain,
        bottom: gapCrossAxis,
      }
  }
}

export type FlexStretch = 'none' | 'horizontal' | 'vertical'

function stretchesChild(parent: FlexParentProps, child: FlexElementProps): boolean {
  const { alignSelf } = child
  const { alignItems } = parent
  const childrenStretchesSelf = alignSelf === 'stretch'
  const childrenAlignsSelfAuto = alignSelf === 'auto' || alignSelf == null
  const parentStretchesChildren = alignItems === 'stretch'
  return childrenStretchesSelf || (parentStretchesChildren && childrenAlignsSelfAuto)
}

export function getFlexStretchForChild(
  parent: FlexParentProps,
  child: FlexElementProps,
): FlexStretch {
  if (stretchesChild(parent, child)) {
    return getCrossAxis(parent)
  } else {
    return 'none'
  }
}
