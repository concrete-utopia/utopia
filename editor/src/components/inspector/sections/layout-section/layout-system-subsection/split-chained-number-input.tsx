import React from 'react'
import { wrapValue } from '../../../../../core/shared/math-utils'
import { ElementPath } from '../../../../../core/shared/project-file-types'
import { assertNever } from '../../../../../core/shared/utils'
import { when } from '../../../../../utils/react-conditionals'
import {
  ChainedNumberInput,
  Icons,
  NumberInputProps,
  SquareButton,
  Tooltip,
  wrappedEmptyOrUnknownOnSubmitValue,
} from '../../../../../uuiui'
import { ControlStatus } from '../../../common/control-status'
import { CSSNumber, isCSSNumber, UnknownOrEmptyInput } from '../../../common/css-utils'

export type ControlMode =
  | 'one-value' // a single value that applies to all sides
  | 'per-direction' // two values that group per direction (vertical / horizontal)
  | 'per-side' // one distinct value per side (TLBR)

const controlModeOrder: ControlMode[] = ['one-value', 'per-direction', 'per-side']

interface ControlCSSNumber {
  controlStatus: ControlStatus
  value: CSSNumber
  onSubmitValue: (newValue: CSSNumber, transient?: boolean) => void
  onTransientSubmitValue: (newValue: CSSNumber) => void
  onUnsetValues: () => void
}

// compare the given values and if they're equal return their shared value, or null otherwise
function getSharedValueIfEqualSides(values: ControlCSSNumber[]): CSSNumber | null {
  const areEqual = values.every(
    (v) =>
      v.controlStatus === 'simple' &&
      v.value.value === values[0].value.value &&
      v.value.unit === values[0].value.unit,
  )
  if (!areEqual) {
    return null
  }
  return values[0].value
}

function areAllSidesUnset(values: ControlCSSNumber[]): boolean {
  return values.every((v) => v.controlStatus !== 'simple')
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

export const SplitChainedNumberInput = React.memo((props: SplitChainedNumberInputProps) => {
  const { name, top, left, bottom, right } = props

  const allSides = React.useMemo(() => [top, left, bottom, right], [top, left, bottom, right])
  const horizontalSides = React.useMemo(() => [left, right], [left, right])
  const verticalSides = React.useMemo(() => [top, bottom], [top, bottom])
  const allSidesUnset = React.useMemo(() => areAllSidesUnset(allSides), [allSides])

  const [aggregateSingleValue, setAggregateSingleValue] = React.useState(
    cssNumberValueOrNull(allSides),
  )
  const [aggregatePerDirectionHorizontal, setAggregatePerDirectionHorizontal] = React.useState(
    cssNumberValueOrNull(horizontalSides),
  )
  const [aggregatePerDirectionVertical, setAggregatePerDirectionVertical] = React.useState(
    cssNumberValueOrNull(verticalSides),
  )

  const getInitialMode = React.useCallback(() => {
    return aggregateSingleValue != null
      ? 'one-value'
      : aggregatePerDirectionHorizontal != null && aggregatePerDirectionVertical != null
      ? 'per-direction'
      : allSidesUnset
      ? props.defaultMode ?? 'per-side'
      : 'per-side'
  }, [
    aggregateSingleValue,
    aggregatePerDirectionHorizontal,
    aggregatePerDirectionVertical,
    allSidesUnset,
    props.defaultMode,
  ])

  const [mode, setMode] = React.useState<ControlMode | null>(null)

  React.useEffect(() => {
    if (mode != null) {
      return
    }
    setMode(getInitialMode())
  }, [props.selectedViews, getInitialMode, mode])

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

  React.useEffect(() => {
    setAggregateSingleValue(cssNumberValueOrNull(allSides))
    setAggregatePerDirectionHorizontal(cssNumberValueOrNull(horizontalSides))
    setAggregatePerDirectionVertical(cssNumberValueOrNull(verticalSides))
  }, [allSides, horizontalSides, verticalSides])

  const onSubmitValue = (old: ControlCSSNumber) =>
    wrappedEmptyOrUnknownOnSubmitValue(old.onSubmitValue, old.onUnsetValues)

  const onTransientSubmitValue = (old: ControlCSSNumber) =>
    wrappedEmptyOrUnknownOnSubmitValue(old.onTransientSubmitValue, old.onUnsetValues)

  const aggregateOnSubmitValue =
    (update: (v: number) => void, sides: ControlCSSNumber[]) =>
    (input: UnknownOrEmptyInput<CSSNumber>) => {
      if (isCSSNumber(input)) {
        update(input.value)
      }
      sides.forEach((side) => onSubmitValue(side)(input))
    }

  const aggregateTransientOnSubmitValue =
    (update: (v: number) => void, sides: ControlCSSNumber[]) =>
    (input: UnknownOrEmptyInput<CSSNumber>) => {
      if (isCSSNumber(input)) {
        update(input.value)
      }
      sides.forEach((side) => onTransientSubmitValue(side)(input))
    }

  const cssValueOrNull = (v: number | null): CSSNumber | null => {
    return v != null ? { value: v, unit: 'px' } : null
  }

  const chainedPropsToRender: Array<Omit<NumberInputProps, 'chained' | 'id'>> = []
  switch (mode) {
    case 'one-value':
      chainedPropsToRender.push({
        style: { width: '100%' },
        value: cssValueOrNull(aggregateSingleValue),
        DEPRECATED_labelBelow: '↔',
        minimum: 0,
        onSubmitValue: aggregateOnSubmitValue(setAggregateSingleValue, allSides),
        onTransientSubmitValue: aggregateTransientOnSubmitValue(setAggregateSingleValue, allSides),
        numberType: 'Px',
        defaultUnitToHide: 'px',
        testId: `${name}-one-value`,
      })
      break
    case 'per-direction':
      chainedPropsToRender.push(
        {
          value: cssValueOrNull(aggregatePerDirectionHorizontal),
          DEPRECATED_labelBelow: 'H',
          minimum: 0,
          onSubmitValue: aggregateOnSubmitValue(
            setAggregatePerDirectionHorizontal,
            horizontalSides,
          ),
          onTransientSubmitValue: aggregateTransientOnSubmitValue(
            setAggregatePerDirectionHorizontal,
            horizontalSides,
          ),
          numberType: 'Px',
          defaultUnitToHide: 'px',
          testId: `${name}-horizontal`,
        },
        {
          value: cssValueOrNull(aggregatePerDirectionVertical),
          DEPRECATED_labelBelow: 'V',
          minimum: 0,
          onSubmitValue: aggregateOnSubmitValue(setAggregatePerDirectionVertical, verticalSides),
          onTransientSubmitValue: aggregateTransientOnSubmitValue(
            setAggregatePerDirectionVertical,
            verticalSides,
          ),
          numberType: 'Px',
          defaultUnitToHide: 'px',
          testId: `${name}-vertical`,
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
      <ChainedNumberInput
        idPrefix={name}
        style={{ flex: 1, gap: 4 }}
        propsArray={chainedPropsToRender}
      />
      <Tooltip
        title={
          mode === 'one-value'
            ? 'Padding'
            : mode === 'per-direction'
            ? 'Padding per direction'
            : 'Padding per side'
        }
      >
        <SquareButton highlight onClick={cycleToNextMode}>
          <>
            {when(mode === 'one-value', <Icons.Dot />)}
            {when(mode === 'per-direction', <Icons.TwoDots />)}
            {when(mode === 'per-side', <Icons.FourDots />)}
          </>
        </SquareButton>
      </Tooltip>
    </div>
  )
})
