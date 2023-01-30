import React from 'react'
import { wrapValue } from '../../../../../core/shared/math-utils'
import { ElementPath } from '../../../../../core/shared/project-file-types'
import { assertNever } from '../../../../../core/shared/utils'
import {
  ChainedNumberInput,
  InspectorSectionIcons,
  NumberInputProps,
  SquareButton,
  Tooltip,
  wrappedEmptyOrUnknownOnSubmitValue,
} from '../../../../../uuiui'
import { edgePosition } from '../../../../canvas/canvas-types'
import { Substores, useEditorState } from '../../../../editor/store/store-hook'
import { ControlStatus, PropertyStatus } from '../../../common/control-status'
import { CSSNumber, isCSSNumber, UnknownOrEmptyInput } from '../../../common/css-utils'
import { InspectorInfo } from '../../../common/property-path-hooks'

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
  const normalUnit = (unit: CSSNumber['unit'] | null) => {
    return unit == null ? 'px' : unit
  }
  const areEqual = values.every((v) => {
    return (
      isControlStatusActive(v.controlStatus) &&
      v.value.value === values[0].value.value &&
      normalUnit(v.value.unit) === normalUnit(values[0].value.unit)
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

export interface Sides {
  top: CSSNumber
  bottom: CSSNumber
  left: CSSNumber
  right: CSSNumber
}

type UpdateShorthand = (sides: Sides, transient?: boolean) => void

export interface SplitChainedNumberInputProps<T> {
  name: string
  defaultMode?: ControlMode
  top: ControlCSSNumber
  left: ControlCSSNumber
  bottom: ControlCSSNumber
  right: ControlCSSNumber
  shorthand: InspectorInfo<T>
  updateShorthand: UpdateShorthand
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

const onSubmitValueShorthand =
  (
    update: (v: number) => void,
    updateShorthand: UpdateShorthand | null,
    sides: Array<ControlCSSNumber>,
    keepSides: Partial<Sides>,
    transient?: boolean,
  ) =>
  (input: UnknownOrEmptyInput<CSSNumber>) => {
    if (!isCSSNumber(input)) {
      return
    }
    update(input.value)
    if (updateShorthand != null) {
      updateShorthand(
        {
          top: keepSides.top ?? input,
          left: keepSides.left ?? input,
          bottom: keepSides.bottom ?? input,
          right: keepSides.right ?? input,
        },
        transient,
      )
    } else {
      sides.forEach((side) => side.onSubmitValue(input, transient))
    }
  }

const onTransientSubmitValueShorthand = (
  update: (v: number) => void,
  updateShorthand: UpdateShorthand | null,
  sides: Array<ControlCSSNumber>,
  keepSides: Partial<Sides>,
) => onSubmitValueShorthand(update, updateShorthand, sides, keepSides, true)

function cssValueOrNull(v: number | null): CSSNumber | null {
  return v != null ? { value: v, unit: 'px' } : null
}

export const SplitChainedNumberInput = React.memo((props: SplitChainedNumberInputProps<any>) => {
  const { name, top, left, bottom, right } = props

  const [oneValue, setOneValue] = React.useState<number | null>(null)
  const [horizontal, setHorizontal] = React.useState<number | null>(null)
  const [vertical, setVertical] = React.useState<number | null>(null)
  const [mode, setMode] = React.useState<ControlMode | null>(null)

  const allSides = React.useMemo(() => [top, left, bottom, right], [top, left, bottom, right])
  const sidesHorizontal = React.useMemo(() => [left, right], [left, right])
  const sidesVertical = React.useMemo(() => [top, bottom], [top, bottom])
  const excludeHorizontal = React.useMemo(
    () => ({ top: top.value, bottom: bottom.value }),
    [top, bottom],
  )
  const excludeVertical = React.useMemo(
    () => ({ left: left.value, right: right.value }),
    [left, right],
  )

  const isCmdPressed = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.keysPressed.cmd === true,
    'SplitChainedNumberInput isCmdPressed',
  )

  const isCurrentModeApplicable = React.useCallback(() => {
    if (mode === 'one-value' && oneValue == null) {
      return false
    }
    if (mode === 'per-direction' && horizontal == null && vertical == null) {
      return false
    }
    return true
  }, [mode, oneValue, horizontal, vertical])

  const updateAggregates = React.useCallback(() => {
    const newOneValue = cssNumberValueOrNull(allSides)
    setOneValue(newOneValue)
    const newHorizontal = cssNumberValueOrNull(sidesHorizontal)
    setHorizontal(newHorizontal)
    const newVertical = cssNumberValueOrNull(sidesVertical)
    setVertical(newVertical)
    return { oneValue: newOneValue, horizontal: newHorizontal, vertical: newVertical }
  }, [allSides, sidesHorizontal, sidesVertical])

  const updateMode = React.useCallback(() => {
    if (mode != null) {
      return
    }

    const aggregates = updateAggregates()
    const newMode = getInitialMode(
      aggregates.oneValue,
      aggregates.horizontal,
      aggregates.vertical,
      areAllSidesSet(allSides),
      props.defaultMode ?? 'per-side',
    )
    setMode(newMode)
  }, [props.defaultMode, mode, allSides, updateAggregates])

  React.useEffect(() => {
    updateMode()
  }, [props.selectedViews, updateMode, props.shorthand])

  React.useEffect(() => {
    updateAggregates()
  }, [updateAggregates])

  React.useEffect(() => {
    return function () {
      setMode(null)
    }
  }, [props.selectedViews])

  React.useEffect(() => {
    if (!isCurrentModeApplicable()) {
      updateMode()
    }
  }, [isCurrentModeApplicable, updateMode])

  const cycleToNextMode = React.useCallback(() => {
    if (mode == null) {
      return
    }
    const delta = isCmdPressed ? -1 : 1
    const index = controlModeOrder.indexOf(mode) + delta
    setMode(controlModeOrder[wrapValue(index, 0, controlModeOrder.length - 1)])
  }, [isCmdPressed, mode])

  const updateShorthandIfUsed = React.useMemo(() => {
    return props.shorthand.controlStatus === 'simple' ? props.updateShorthand : null
  }, [props.shorthand, props.updateShorthand])

  const chainedPropsToRender: Array<Omit<NumberInputProps, 'chained' | 'id'>> =
    React.useMemo(() => {
      switch (mode) {
        case 'one-value':
          return [
            {
              style: { width: '100%' },
              value: cssValueOrNull(oneValue),
              DEPRECATED_labelBelow: '↔',
              minimum: 0,
              onSubmitValue: onSubmitValueShorthand(
                setOneValue,
                updateShorthandIfUsed,
                allSides,
                {},
              ),
              onTransientSubmitValue: onTransientSubmitValueShorthand(
                setOneValue,
                updateShorthandIfUsed,
                allSides,
                {},
              ),
              numberType: 'Px',
              defaultUnitToHide: 'px',
              controlStatus: allSides[0].controlStatus,
              testId: `${name}-one`,
            },
          ]
        case 'per-direction':
          return [
            {
              value: cssValueOrNull(horizontal),
              DEPRECATED_labelBelow: 'H',
              minimum: 0,
              onSubmitValue: onSubmitValueShorthand(
                setHorizontal,
                updateShorthandIfUsed,
                sidesHorizontal,
                excludeHorizontal,
              ),
              onTransientSubmitValue: onTransientSubmitValueShorthand(
                setHorizontal,
                updateShorthandIfUsed,
                sidesHorizontal,
                excludeHorizontal,
              ),
              numberType: 'Px',
              controlStatus: sidesHorizontal[0].controlStatus,
              defaultUnitToHide: 'px',
              testId: `${name}-H`,
            },
            {
              value: cssValueOrNull(vertical),
              DEPRECATED_labelBelow: 'V',
              minimum: 0,
              onSubmitValue: onSubmitValueShorthand(
                setVertical,
                updateShorthandIfUsed,
                sidesVertical,
                excludeVertical,
              ),
              onTransientSubmitValue: onTransientSubmitValueShorthand(
                setVertical,
                updateShorthandIfUsed,
                sidesVertical,
                excludeVertical,
              ),
              numberType: 'Px',
              controlStatus: sidesVertical[0].controlStatus,
              defaultUnitToHide: 'px',
              testId: `${name}-V`,
            },
          ]
        case 'per-side':
          return [
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
          ]
        case null:
          return []
        default:
          assertNever(mode)
      }
    }, [
      mode,
      allSides,
      excludeHorizontal,
      excludeVertical,
      oneValue,
      horizontal,
      vertical,
      top,
      bottom,
      left,
      right,
      name,
      sidesHorizontal,
      sidesVertical,
      updateShorthandIfUsed,
    ])

  const tooltipTitle = React.useMemo(() => {
    switch (mode) {
      case 'one-value':
        return 'Padding'
      case 'per-direction':
        return 'Padding per direction'
      case 'per-side':
        return 'Padding per side'
      case null:
        return ''
      default:
        assertNever(mode)
    }
  }, [mode])

  const modeIcon = React.useMemo(() => {
    switch (mode) {
      case 'one-value':
        return <InspectorSectionIcons.SplitFull />
      case 'per-direction':
        return <InspectorSectionIcons.SplitHalf />
      case 'per-side':
        return <InspectorSectionIcons.SplitQuarter />
      case null:
        return null
      default:
        assertNever(mode)
    }
  }, [mode])

  return (
    <div style={{ display: 'flex', flexDirection: 'row', gap: 4 }}>
      <Tooltip title={tooltipTitle}>
        <SquareButton onClick={cycleToNextMode}>{modeIcon}</SquareButton>
      </Tooltip>
      <ChainedNumberInput
        idPrefix={name}
        style={{ flex: 1, gap: 4 }}
        propsArray={chainedPropsToRender}
      />
    </div>
  )
})
