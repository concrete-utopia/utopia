interface Size {
  width: number
  height: number
}

export enum FramePoint {
  Left = 'left',
  Right = 'right',
  CenterX = 'centerX',
  Width = 'width',
  Top = 'top',
  Bottom = 'bottom',
  CenterY = 'centerY',
  Height = 'height',
}

export function isFramePoint(s: string): s is FramePoint {
  return (
    s === FramePoint.Left ||
    s === FramePoint.Right ||
    s === FramePoint.CenterX ||
    s === FramePoint.Width ||
    s === FramePoint.Top ||
    s === FramePoint.Bottom ||
    s === FramePoint.CenterY ||
    s === FramePoint.Height
  )
}

export const HorizontalFramePoints = [
  FramePoint.Left,
  FramePoint.Right,
  FramePoint.CenterX,
  FramePoint.Width,
]
export const HorizontalFramePointsExceptSize = [
  FramePoint.Left,
  FramePoint.Right,
  FramePoint.CenterX,
]
export const VerticalFramePoints = [
  FramePoint.Top,
  FramePoint.Bottom,
  FramePoint.CenterY,
  FramePoint.Height,
]
export const VerticalFramePointsExceptSize = [FramePoint.Top, FramePoint.Bottom, FramePoint.CenterY]
export const AllFramePoints = [
  FramePoint.Left,
  FramePoint.Top,
  FramePoint.Right,
  FramePoint.Bottom,
  FramePoint.CenterX,
  FramePoint.CenterY,
  FramePoint.Width,
  FramePoint.Height,
]

export const AllFramePointsExceptSize = [
  FramePoint.Left,
  FramePoint.Top,
  FramePoint.Right,
  FramePoint.Bottom,
  FramePoint.CenterX,
  FramePoint.CenterY,
]

export type FramePoints = { [framePoint: string]: number }

export type FramePin = string | number

export function isPercentPin(pin: FramePin): boolean {
  if (typeof pin === 'string') {
    const parts = pin.split('%')
    return parts.length > 1
  } else {
    return false
  }
}

export function numberPartOfPin(pin: FramePin): number {
  if (typeof pin === 'string') {
    const parts = pin.split('%')
    return parseFloat(parts[0])
  } else {
    return pin
  }
}

function valueForStringPin(pin: string, referenceSize: number): number {
  const parts = pin.split('%')
  const numericPart = parseFloat(parts[0])
  if (parts.length === 1) {
    return numericPart
  } else {
    return (numericPart / 100) * referenceSize
  }
}

function numericValueForPin(
  operation: (value: number, refSize: number) => number,
): (pin: FramePin, referenceSize: number) => number {
  return (pin: FramePin, referenceSize: number) => {
    const value = typeof pin === 'number' ? pin : valueForStringPin(pin, referenceSize)
    return operation(value, referenceSize)
  }
}

const minValueForPin = numericValueForPin((v: number) => v)
const midValueForPin = numericValueForPin((v: number, r: number) => r / 2 + v)
const maxValueForPin = numericValueForPin((v: number, r: number) => r - v)
const sizeValueForPin = minValueForPin

export interface Frame {
  left?: FramePin
  right?: FramePin
  centerX?: FramePin
  width?: FramePin
  top?: FramePin
  bottom?: FramePin
  centerY?: FramePin
  height?: FramePin
}

export interface PinFrameProps {
  left?: number
  right?: number
  centerX?: number
  width?: number
  top?: number
  bottom?: number
  centerY?: number
  height?: number
}

export interface NormalisedFrame {
  left: number
  top: number
  width: number
  height: number
}

export function toNormalisedFrame(frame: Frame, parentFrame: NormalisedFrame): NormalisedFrame {
  const { left, right, centerX, width, top, bottom, centerY, height } = frame

  return {
    left: normalisedMin(parentFrame.width, left, centerX, right, width),
    top: normalisedMin(parentFrame.height, top, centerY, bottom, height),
    width: normalisedSize(parentFrame.width, left, centerX, right, width),
    height: normalisedSize(parentFrame.height, top, centerY, bottom, height),
  }
}

export function toAbsoluteFrame(
  normalisedFrame: NormalisedFrame,
  parentFrame: NormalisedFrame,
): NormalisedFrame {
  return {
    left: parentFrame.left + normalisedFrame.left,
    top: parentFrame.top + normalisedFrame.top,
    width: normalisedFrame.width,
    height: normalisedFrame.height,
  }
}

function normalisedMin(
  referenceSize: number,
  min?: FramePin,
  mid?: FramePin,
  max?: FramePin,
  size?: FramePin,
): number {
  if (min != null) {
    return minValueForPin(min, referenceSize)
  } else if (max != null) {
    const absoluteMax = maxValueForPin(max, referenceSize)
    if (mid != null) {
      const absoluteMid = midValueForPin(mid, referenceSize)
      const difference = absoluteMax - absoluteMid
      return absoluteMax - 2 * difference
    } else {
      if (size == null) {
        return 0
      }
      const absoluteSize = sizeValueForPin(size, referenceSize)
      return absoluteMax - absoluteSize
    }
  } else {
    if (mid == null || size == null) {
      return 0
    }
    const absoluteMid = midValueForPin(mid, referenceSize)
    const absoluteSize = sizeValueForPin(size, referenceSize)
    return absoluteMid - absoluteSize / 2
  }
}

export function zeroIfNegative(n: number): number {
  if (n < 0) {
    return 0
  } else {
    return n
  }
}

function normalisedSize(
  referenceSize: number,
  min?: FramePin,
  mid?: FramePin,
  max?: FramePin,
  size?: FramePin,
): number {
  if (size != null) {
    return zeroIfNegative(sizeValueForPin(size, referenceSize))
  } else if (min != null) {
    if (mid != null) {
      const absoluteMid = midValueForPin(mid, referenceSize)
      const absoluteMin = minValueForPin(min, referenceSize)
      return zeroIfNegative((absoluteMid - absoluteMin) * 2)
    } else {
      if (max == null) {
        return 0
      }
      const absoluteMax = maxValueForPin(max, referenceSize)
      const absoluteMin = minValueForPin(min, referenceSize)
      return zeroIfNegative(absoluteMax - absoluteMin)
    }
  } else {
    if (mid == null || max == null) {
      return 0
    }
    const absoluteMid = midValueForPin(mid, referenceSize)
    const absoluteMax = maxValueForPin(max, referenceSize)
    return zeroIfNegative((absoluteMax - absoluteMid) * 2)
  }
}

export function referenceParentValueForProp(prop: FramePoint, parentSize: Size): number {
  switch (prop) {
    case FramePoint.Left:
      return 0
    case FramePoint.CenterX:
      return parentSize.width / 2
    case FramePoint.Right:
      return parentSize.width
    case FramePoint.Width:
      return 0
    case FramePoint.Top:
      return 0
    case FramePoint.CenterY:
      return parentSize.height / 2
    case FramePoint.Bottom:
      return parentSize.height
    case FramePoint.Height:
      return 0
    default:
      const _exhaustiveCheck: never = prop
      throw new Error(`Unknown frame point ${JSON.stringify(prop)}`)
  }
}

export function isHorizontalPoint(point: FramePoint) {
  return HorizontalFramePoints.includes(point)
}

export function valueToUseForPin(
  prop: FramePoint,
  absoluteValue: number,
  pinIsPercentPin: boolean,
  parentRect: Size,
): string | number {
  const referenceSize = isHorizontalPoint(prop) ? parentRect.width : parentRect.height
  const referenceValue = referenceParentValueForProp(prop, parentRect)
  const shouldInvertOffset = prop === FramePoint.Right || prop === FramePoint.Bottom
  const actualOffsetValue = absoluteValue - referenceValue
  const offsetValue = shouldInvertOffset ? -actualOffsetValue : actualOffsetValue
  if (pinIsPercentPin) {
    const percentValue = (offsetValue / referenceSize) * 100
    return `${percentValue}%`
  } else {
    return offsetValue
  }
}
