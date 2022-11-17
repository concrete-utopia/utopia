import React from 'react'
import { StyleLayoutProp } from '../../../../core/layout/layout-helpers-new'
import { defaultEither, Either } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import { roundTo } from '../../../../core/shared/math-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { Modifiers } from '../../../../utils/modifiers'
import { AllElementProps } from '../../../editor/store/editor-state'
import {
  CSSNumber,
  CSSNumberUnit,
  cssParsers,
  ParsedCSSProperties,
  printCSSNumber,
} from '../../../inspector/common/css-utils'

export const Emdash: string = '\u2014'

export const DisabledColor = 'rgba(150, 150, 150, 0.5)'

export interface CSSNumberWithRenderedValue {
  value: CSSNumber
  renderedValuePx: number
}

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

const FontSize = 12
const Padding = 4

interface CanvasLabelProps {
  scale: number
  color: string
  value: string | number
}

export const CanvasLabel = React.memo((props: CanvasLabelProps): JSX.Element => {
  const { scale, color, value } = props
  const fontSize = FontSize / scale
  const padding = Padding / scale
  return (
    <div
      style={{
        fontSize: fontSize,
        padding: padding,
        backgroundColor: color,
        color: 'white',
        borderRadius: 2,
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
        border: `${borderWidth}px solid rgba(255, 255, 255)`,
      }}
    />
  )
})

export const StripeOpacity: number = 30

export const StripedBackgroundCSS = (
  stripeColor: string,
  scale: number,
): { backgroundImage: string; backgroundSize: string } => ({
  backgroundImage: `linear-gradient(135deg, ${stripeColor} 24.5%, rgba(255,255,255,0) 24.5%, rgba(255,255,255,0) 50%, ${stripeColor} 50%, ${stripeColor} 74%, rgba(255,255,255,0) 74%, rgba(255,255,255,0) 100%)`,
  backgroundSize: `${4 / scale}px ${4 / scale}px`,
})

export type Timeout = ReturnType<typeof setTimeout>

export function useHoverWithDelay(
  delay: number,
  update: (hovered: boolean) => void,
): [React.MouseEventHandler, React.MouseEventHandler] {
  const fadeInTimeout = React.useRef<Timeout | null>(null)

  const onHoverEnd = () => {
    if (fadeInTimeout.current) {
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
    return printCSSNumber(value.value, null)
  }

  return Emdash
}

export function getPropertyFromStyle<P extends StyleLayoutProp, T = ParsedCSSProperties[P]>(
  allElementProps: AllElementProps,
  elementPath: ElementPath,
  prop: P,
): T | null {
  const parser = cssParsers[prop] as (value: unknown) => Either<string, T>
  return optionalMap(
    (v) => defaultEither(null, parser(v)),
    allElementProps[EP.toString(elementPath)]?.['style']?.[prop],
  )
}
