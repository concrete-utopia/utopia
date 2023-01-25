import React from 'react'
import { wrapValue } from '../../../../../core/shared/math-utils'
import { ElementPath } from '../../../../../core/shared/project-file-types'
import { assertNever } from '../../../../../core/shared/utils'
import { when } from '../../../../../utils/react-conditionals'
import {
  ChainedNumberInput,
  InspectorSectionIcons,
  NumberInputProps,
  SquareButton,
  Tooltip,
  wrappedEmptyOrUnknownOnSubmitValue,
} from '../../../../../uuiui'
import { ControlStatus, PropertyStatus } from '../../../common/control-status'
import { CSSNumber, isCSSNumber, UnknownOrEmptyInput } from '../../../common/css-utils'

export type ControlMode =
  | 'one-value' // a single value that applies to all sides
  | 'per-direction' // two values that group per direction (vertical / horizontal)
  | 'per-side' // one distinct value per side (TLBR)

const controlModeOrder: ControlMode[] = ['one-value', 'per-direction', 'per-side']

interface ControlCSSNumber {
  controlStatus: ControlStatus
  propertyStatus: PropertyStatus
  value: CSSNumber
  onSubmitValue: (newValue: CSSNumber, transient?: boolean) => void
  onTransientSubmitValue: (newValue: CSSNumber) => void
  onUnsetValues: () => void
}

function isControlStatusActive(status: ControlStatus): boolean {
  switch (status) {
    case 'off':
    case 'unset':
    case 'disabled':
    case 'detected':
    case 'multiselect-disabled':
    case 'trivial-default':
      return false
    default:
      return true
  }
}

// compare the given values and if they're equal return their shared value, or null otherwise
function getSharedValueIfEqualSides(values: ControlCSSNumber[]): CSSNumber | null {
  const areEqual = values.every((v) => {
    return (
      isControlStatusActive(v.controlStatus) &&
      v.value.value === values[0].value.value &&
      v.value.unit === values[0].value.unit
    )
  })
  if (!areEqual) {
    return null
  }
  return values[0].value
}

function areAllSidesSet(values: ControlCSSNumber[]): boolean {
  return values.every((v) => isControlStatusActive(v.controlStatus))
}

function cssNumberValueOrNull(values: ControlCSSNumber[]): number | null {
  const result = getSharedValueIfEqualSides(values)
  if (result == null) {
    return null
  }
  return result.value
}

export interface SplitChainedNumberInputProps {
  name: string
  defaultMode?: ControlMode
  top: ControlCSSNumber
  left: ControlCSSNumber
  bottom: ControlCSSNumber
  right: ControlCSSNumber
  selectedViews: ElementPath[]
}

function getInitialMode(
  aggOne: number | null,
  aggHorizontal: number | null,
  aggVertical: number | null,
  allSidesSet: boolean,
  defaultMode: ControlMode,
): ControlMode {
  if (aggOne != null) {
    return 'one-value'
  }
  if (aggHorizontal != null && aggVertical != null) {
    return 'per-direction'
  }
  if (allSidesSet) {
    return 'per-side'
  }
  if (aggHorizontal != null || aggVertical != null) {
    return 'per-direction'
  }
  return defaultMode
}

function onSubmitValue(old: ControlCSSNumber) {
  return wrappedEmptyOrUnknownOnSubmitValue(old.onSubmitValue, old.onUnsetValues)
}

function onTransientSubmitValue(old: ControlCSSNumber) {
  return wrappedEmptyOrUnknownOnSubmitValue(old.onTransientSubmitValue, old.onUnsetValues)
}

const aggOnSubmitValue =
  (update: (v: number) => void, sides: ControlCSSNumber[]) =>
  (input: UnknownOrEmptyInput<CSSNumber>) => {
    if (isCSSNumber(input)) {
      update(input.value)
    }
    sides.forEach((side) => onSubmitValue(side)(input))
  }

const aggTransientOnSubmitValue =
  (update: (v: number) => void, sides: ControlCSSNumber[]) =>
  (input: UnknownOrEmptyInput<CSSNumber>) => {
    if (isCSSNumber(input)) {
      update(input.value)
    }
    sides.forEach((side) => onTransientSubmitValue(side)(input))
  }

function cssValueOrNull(v: number | null): CSSNumber | null {
  return v != null ? { value: v, unit: 'px' } : null
}

export const SplitChainedNumberInput = React.memo((props: SplitChainedNumberInputProps) => {
  const { name, top, left, bottom, right } = props

  const [aggOneValue, setAggOneValue] = React.useState<number | null>(null)
  const [aggHorizontal, setAggHorizontal] = React.useState<number | null>(null)
  const [aggVertical, setAggVertical] = React.useState<number | null>(null)
  const [mode, setMode] = React.useState<ControlMode | null>(null)

  const allSides = React.useMemo(() => [top, left, bottom, right], [top, left, bottom, right])
  const horizontalSides = React.useMemo(() => [left, right], [left, right])
  const verticalSides = React.useMemo(() => [top, bottom], [top, bottom])

  React.useEffect(() => {
    if (mode != null) {
      return
    }

    const newAggOneValue = cssNumberValueOrNull(allSides)
    setAggOneValue(newAggOneValue)
    const newAggHorizontal = cssNumberValueOrNull(horizontalSides)
    setAggHorizontal(newAggHorizontal)
    const newAggVertical = cssNumberValueOrNull(verticalSides)
    setAggVertical(newAggVertical)

    const newMode = getInitialMode(
      newAggOneValue,
      newAggHorizontal,
      newAggVertical,
      areAllSidesSet(allSides),
      props.defaultMode ?? 'per-side',
    )
    setMode(newMode)
  }, [props.selectedViews, props.defaultMode, mode, allSides, horizontalSides, verticalSides])

  React.useEffect(() => {
    return function () {
      setMode(null)
    }
  }, [props.selectedViews])

  const cycleToNextMode = React.useCallback(() => {
    if (mode == null) {
      return
    }
    const index = controlModeOrder.indexOf(mode) + 1
    setMode(controlModeOrder[wrapValue(index, 0, controlModeOrder.length - 1)])
  }, [mode])

  const chainedPropsToRender: Array<Omit<NumberInputProps, 'chained' | 'id'>> = []
  switch (mode) {
    case 'one-value':
      chainedPropsToRender.push({
        style: { width: '100%' },
        value: cssValueOrNull(aggOneValue),
        DEPRECATED_labelBelow: '↔',
        minimum: 0,
        onSubmitValue: aggOnSubmitValue(setAggOneValue, allSides),
        onTransientSubmitValue: aggTransientOnSubmitValue(setAggOneValue, allSides),
        numberType: 'Px',
        defaultUnitToHide: 'px',
        controlStatus: allSides[0].controlStatus,
        testId: `${name}-one`,
      })
      break
    case 'per-direction':
      chainedPropsToRender.push(
        {
          value: cssValueOrNull(aggHorizontal),
          DEPRECATED_labelBelow: 'H',
          minimum: 0,
          onSubmitValue: aggOnSubmitValue(setAggHorizontal, horizontalSides),
          onTransientSubmitValue: aggTransientOnSubmitValue(setAggHorizontal, horizontalSides),
          numberType: 'Px',
          controlStatus: horizontalSides[0].controlStatus,
          defaultUnitToHide: 'px',
          testId: `${name}-H`,
        },
        {
          value: cssValueOrNull(aggVertical),
          DEPRECATED_labelBelow: 'V',
          minimum: 0,
          onSubmitValue: aggOnSubmitValue(setAggVertical, verticalSides),
          onTransientSubmitValue: aggTransientOnSubmitValue(setAggVertical, verticalSides),
          numberType: 'Px',
          controlStatus: verticalSides[0].controlStatus,
          defaultUnitToHide: 'px',
          testId: `${name}-V`,
        },
      )
      break
    case 'per-side':
      chainedPropsToRender.push(
        {
          value: top.value,
          DEPRECATED_labelBelow: 'T',
          minimum: 0,
          onSubmitValue: onSubmitValue(top),
          onTransientSubmitValue: onTransientSubmitValue(top),
          controlStatus: top.controlStatus,
          numberType: 'LengthPercent',
          defaultUnitToHide: 'px',
          testId: `${name}-T`,
        },
        {
          value: right.value,
          DEPRECATED_labelBelow: 'R',
          minimum: 0,
          onSubmitValue: onSubmitValue(right),
          onTransientSubmitValue: onTransientSubmitValue(right),
          controlStatus: right.controlStatus,
          numberType: 'LengthPercent',
          defaultUnitToHide: 'px',
          testId: `${name}-R`,
        },
        {
          value: bottom.value,
          DEPRECATED_labelBelow: 'B',
          minimum: 0,
          onSubmitValue: onSubmitValue(bottom),
          onTransientSubmitValue: onTransientSubmitValue(bottom),
          controlStatus: bottom.controlStatus,
          numberType: 'LengthPercent',
          defaultUnitToHide: 'px',
          testId: `${name}-B`,
        },
        {
          value: left.value,
          DEPRECATED_labelBelow: 'L',
          minimum: 0,
          onSubmitValue: onSubmitValue(left),
          onTransientSubmitValue: onTransientSubmitValue(left),
          controlStatus: left.controlStatus,
          numberType: 'LengthPercent',
          defaultUnitToHide: 'px',
          testId: `${name}-L`,
        },
      )
      break
    case null:
      break
    default:
      assertNever(mode)
  }

  return (
    <div style={{ display: 'flex', flexDirection: 'row', gap: 4 }}>
      <Tooltip
        title={
          mode === 'one-value'
            ? 'Padding'
            : mode === 'per-direction'
            ? 'Padding per direction'
            : 'Padding per side'
        }
      >
        <SquareButton onClick={cycleToNextMode}>
          <>
            {when(mode === 'one-value', <InspectorSectionIcons.SplitFull />)}
            {when(mode === 'per-direction', <InspectorSectionIcons.SplitHalf />)}
            {when(mode === 'per-side', <InspectorSectionIcons.SplitQuarter />)}
          </>
        </SquareButton>
      </Tooltip>
      <ChainedNumberInput
        idPrefix={name}
        style={{ flex: 1, gap: 4 }}
        propsArray={chainedPropsToRender}
      />
    </div>
  )
})
