import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import type { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import { roundTo, size, zeroRectIfNullOrInfinity } from '../../../../core/shared/math-utils'
import type { Modifiers } from '../../../../utils/modifiers'
import type { ProjectContentTreeRoot } from '../../../assets'
import { colorTheme } from '../../../../uuiui'
import type { CSSNumber, CSSNumberUnit } from '../../../inspector/common/css-utils'
import { cssNumber, printCSSNumber } from '../../../inspector/common/css-utils'
import { elementHasOnlyTextChildren } from '../../canvas-utils'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { treatElementAsGroupLikeFromMetadata } from '../../canvas-strategies/strategies/group-helpers'
import { assertNever } from '../../../../core/shared/utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import type { PropertyControlsInfo } from '../../../custom-code/code-file'

export const Emdash: string = '\u2014'

export interface CSSNumberWithRenderedValue {
  value: CSSNumber
  renderedValuePx: number
}

export const unitlessCSSNumberWithRenderedValue = (
  renderedValuePx: number,
): CSSNumberWithRenderedValue => ({
  value: { value: renderedValuePx, unit: null },
  renderedValuePx: renderedValuePx,
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

  const baseValue = fallbackEmptyValue(base)

  if (base.renderedValuePx === 0) {
    return {
      renderedValuePx: desiredRenderedValueWithPrecision,
      value: { value: desiredRenderedValueWithPrecision, unit: null },
    }
  }

  const pixelsPerUnit = baseValue.value / base.renderedValuePx
  const desiredValueInUnits = valueWithUnitAppropriatePrecision(
    baseValue.unit,
    desiredRenderedValue * pixelsPerUnit,
    precision,
  )

  return {
    renderedValuePx: desiredRenderedValueWithPrecision,
    value: {
      unit: baseValue.unit,
      value: desiredValueInUnits,
    },
  }
}

const FontSize = 11
const PaddingV = 0
const PaddingH = 2
const ExplicitHeightHacked = 20
const BorderRadius = 2

interface CanvasLabelProps {
  scale: number
  color: string
  textColor: string
  value: string | number
  onMouseDown?: React.MouseEventHandler
  testId?: string
}

export const CanvasLabel = React.memo((props: CanvasLabelProps): JSX.Element => {
  const { scale, color, value, textColor } = props
  const fontSize = FontSize / scale
  const paddingV = PaddingV / scale
  const paddingH = PaddingH / scale
  const borderRadius = BorderRadius / scale
  return (
    <div
      data-testid={props.testId}
      style={{
        display: 'flex',
        alignItems: 'center',
        fontSize: fontSize,
        paddingLeft: paddingH,
        paddingRight: paddingH,
        paddingTop: paddingV,
        paddingBottom: paddingV,
        backgroundColor: color,
        color: textColor,
        borderRadius: borderRadius,
        height: ExplicitHeightHacked / scale,
      }}
      onMouseDown={props.onMouseDown}
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

const SHOW_ALL_CONTROLS_THRESHOLD = 80
const SHOW_NO_CONTROLS_THRESHOLD = 60

export function canShowCanvasPropControl(
  projectContents: ProjectContentTreeRoot,
  path: ElementPath,
  scale: number,
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  propertyControlsInfo: PropertyControlsInfo,
): Set<CanvasPropControl> {
  function getControls(element: ElementInstanceMetadata): CanvasPropControl[] {
    const frame = zeroRectIfNullOrInfinity(element.globalFrame)

    const { width, height } = size((frame.width ?? 0) * scale, (frame.height ?? 0) * scale)
    if (width > SHOW_ALL_CONTROLS_THRESHOLD && height > SHOW_ALL_CONTROLS_THRESHOLD) {
      return ['borderRadius', 'padding', 'gap']
    }

    if (width < SHOW_NO_CONTROLS_THRESHOLD || height < SHOW_NO_CONTROLS_THRESHOLD) {
      return []
    }

    if (elementHasOnlyTextChildren(element)) {
      return ['padding']
    }

    if (
      !MetadataUtils.targetElementSupportsChildren(
        projectContents,
        element.elementPath,
        metadata,
        elementPathTree,
        propertyControlsInfo,
      )
    ) {
      return ['borderRadius', 'gap']
    }

    return ['padding', 'gap']
  }

  const element = MetadataUtils.findElementByElementPath(metadata, path)
  if (element == null) {
    return new Set<CanvasPropControl>([])
  }
  return onlyAcceptableCanvasPropControls(element, getControls(element))
}

// return a set from the given controls, dropping all the ones that are not acceptable for the given element.
function onlyAcceptableCanvasPropControls(
  element: ElementInstanceMetadata,
  controls: CanvasPropControl[],
): Set<CanvasPropControl> {
  return new Set<CanvasPropControl>(
    mapDropNulls((control) => {
      switch (control) {
        case 'borderRadius':
          const isGroup = treatElementAsGroupLikeFromMetadata(element)
          const hasOverflowHidden =
            element.computedStyle != null && element.computedStyle['overflow'] === 'hidden'
          if (isGroup && !hasOverflowHidden) {
            return null
          }
          return control

        case 'gap':
        case 'padding':
          return control

        default:
          assertNever(control)
      }
    }, controls),
  )
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

export function fallbackEmptyValue(numberWithRenderedValue: CSSNumberWithRenderedValue): CSSNumber {
  if (!numberWithRenderedValue.value.emptyValue) {
    return numberWithRenderedValue.value
  }
  return cssNumber(numberWithRenderedValue.renderedValuePx, 'px')
}
