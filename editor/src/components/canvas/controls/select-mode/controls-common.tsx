import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import { roundTo, size, zeroRectIfNullOrInfinity } from '../../../../core/shared/math-utils'
import { Modifiers } from '../../../../utils/modifiers'
import { ProjectContentTreeRoot } from '../../../assets'
import { colorTheme } from '../../../../uuiui'
import { CSSNumber, CSSNumberUnit, printCSSNumber } from '../../../inspector/common/css-utils'
import { elementHasOnlyTextChildren } from '../../canvas-utils'

export const Emdash: string = '\u2014'

export interface CSSNumberWithRenderedValue {
  value: CSSNumber
  renderedValuePx: number
}

export const unitlessCSSNumberWithRenderedValue = (
  renderedValuePx: number,
): CSSNumberWithRenderedValue => ({
  value: { value: renderedValuePx, unit: null },
  renderedValuePx,
})

export const cssNumberWithRenderedValue = (
  value: CSSNumber,
  renderedValuePx: number,
): CSSNumberWithRenderedValue => ({ value, renderedValuePx })

export type AdjustPrecision = 'precise' | 'coarse'

export function precisionFromModifiers(modifiers: Modifiers): AdjustPrecision {
  return modifiers.shift ? 'coarse' : 'precise'
}

export function valueWithUnitAppropriatePrecision(
  unit: CSSNumberUnit | null,
  value: number,
  precision: AdjustPrecision,
): number {
  const baseMultiplicator = unit === 'em' || unit === 'cm' ? 1 : 0
  const multiplicator = precision === 'coarse' ? baseMultiplicator - 1 : baseMultiplicator
  return roundTo(value, multiplicator)
}

export const offsetMeasurementByDelta = (
  measurement: CSSNumberWithRenderedValue,
  delta: number,
  precision: AdjustPrecision,
): CSSNumberWithRenderedValue =>
  measurementBasedOnOtherMeasurement(measurement, measurement.renderedValuePx + delta, precision)

export function measurementBasedOnOtherMeasurement(
  base: CSSNumberWithRenderedValue,
  desiredRenderedValue: number,
  precision: AdjustPrecision,
): CSSNumberWithRenderedValue {
  const desiredRenderedValueWithPrecision = valueWithUnitAppropriatePrecision(
    'px',
    desiredRenderedValue,
    precision,
  )

  if (base.renderedValuePx === 0) {
    return {
      renderedValuePx: desiredRenderedValueWithPrecision,
      value: { value: desiredRenderedValueWithPrecision, unit: null },
    }
  }

  const pixelsPerUnit = base.value.value / base.renderedValuePx
  const desiredValueInUnits = valueWithUnitAppropriatePrecision(
    base.value.unit,
    desiredRenderedValue * pixelsPerUnit,
    precision,
  )

  return {
    renderedValuePx: desiredRenderedValueWithPrecision,
    value: {
      unit: base.value.unit,
      value: desiredValueInUnits,
    },
  }
}

const FontSize = 11
const PaddingV = 0
const PaddingH = 2

const BorderRadius = 2

interface CanvasLabelProps {
  scale: number
  color: string
  textColor: string
  value: string | number
}

export const CanvasLabel = React.memo((props: CanvasLabelProps): JSX.Element => {
  const { scale, color, value, textColor } = props
  const fontSize = FontSize / scale
  const paddingV = PaddingV / scale
  const paddingH = PaddingH / scale
  const borderRadius = BorderRadius / scale
  return (
    <div
      style={{
        fontSize: fontSize,
        paddingLeft: paddingH,
        paddingRight: paddingH,
        paddingTop: paddingV,
        paddingBottom: paddingV,
        backgroundColor: color,
        color: textColor,
        borderRadius: borderRadius,
      }}
    >
      {value}
    </div>
  )
})

interface PillHandleProps {
  width: number
  height: number
  pillColor: string
  borderWidth: number
}

export const PillHandle = React.memo((props: PillHandleProps): JSX.Element => {
  const { width, height, pillColor, borderWidth } = props
  return (
    <div
      style={{
        width: width,
        height: height,
        backgroundColor: pillColor,
        borderRadius: 1,
        border: `${borderWidth}px solid ${colorTheme.white.value}`,
      }}
    />
  )
})

export type Timeout = ReturnType<typeof setTimeout>

export function useHoverWithDelay(
  delay: number,
  update: (hovered: boolean) => void,
): [React.MouseEventHandler, React.MouseEventHandler] {
  const fadeInTimeout = React.useRef<Timeout | null>(null)

  const onHoverEnd = () => {
    if (fadeInTimeout.current != null) {
      clearTimeout(fadeInTimeout.current)
    }
    fadeInTimeout.current = null
    update(false)
  }

  const onHoverStart = () => {
    fadeInTimeout.current = setTimeout(() => update(true), delay)
  }

  return [onHoverStart, onHoverEnd]
}

export function indicatorMessage(
  isOverThreshold: boolean,
  value: CSSNumberWithRenderedValue,
): string | number {
  if (isOverThreshold) {
    return printCSSNumber(value.value, value.value.unit)
  }

  return Emdash // emdash
}

export function cssNumberEqual(left: CSSNumber, right: CSSNumber): boolean {
  return left.unit === right.unit && left.value === right.value
}

type CanvasPropControl = 'padding' | 'borderRadius' | 'gap'

const CONTROL_CROWDING_UPPER_THRESHOLD = 80
const CONTROL_CROWDING_LOWER_THRESHOLD = 40

export function canShowCanvasPropControl(
  projectContents: ProjectContentTreeRoot,
  element: ElementInstanceMetadata,
  scale: number,
): Set<CanvasPropControl> {
  const frame = zeroRectIfNullOrInfinity(element.globalFrame)

  const { width, height } = size((frame.width ?? 0) * scale, (frame.height ?? 0) * scale)

  if (width > CONTROL_CROWDING_UPPER_THRESHOLD && height > CONTROL_CROWDING_UPPER_THRESHOLD) {
    return new Set<CanvasPropControl>(['borderRadius', 'padding', 'gap'])
  }

  if (Math.min(width, height) < CONTROL_CROWDING_LOWER_THRESHOLD) {
    return new Set<CanvasPropControl>([])
  }

  if (elementHasOnlyTextChildren(element)) {
    return new Set<CanvasPropControl>(['padding'])
  }

  if (!MetadataUtils.targetElementSupportsChildren(projectContents, element)) {
    return new Set<CanvasPropControl>(['borderRadius', 'gap'])
  }

  return new Set<CanvasPropControl>(['padding', 'gap'])
}

export function shouldShowControls(
  propAvailableFromStyle: boolean,
  measurementsNonZero: boolean,
): boolean {
  if (propAvailableFromStyle) {
    return true
  }

  if (!propAvailableFromStyle && !measurementsNonZero) {
    return true
  }

  if (!propAvailableFromStyle && measurementsNonZero) {
    return false
  }

  return true
}
