import { useSetAtom } from 'jotai'
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
import { useRefEditorState } from '../../../../editor/store/store-hook'
import { ControlStatus, PropertyStatus } from '../../../common/control-status'
import {
  CSSNumber,
  CSSNumberType,
  isCSSNumber,
  UnknownOrEmptyInput,
} from '../../../common/css-utils'
import {
  CanvasControlWithProps,
  InspectorFocusedCanvasControls,
  InspectorHoveredCanvasControls,
} from '../../../common/inspector-atoms'
import { InspectorInfo } from '../../../common/property-path-hooks'

export type ControlMode =
  | 'one-value' // a single value that applies to all sides
  | 'per-direction' // two values that group per direction (vertical / horizontal)
  | 'per-side' // one distinct value per side (TLBR)

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

export interface Sides {
  top: CSSNumber
  bottom: CSSNumber
  left: CSSNumber
  right: CSSNumber
}

type UpdateShorthand = (sides: Sides, transient?: boolean) => void

interface CanvasControls {
  onHover?: CanvasControlWithProps<any>
  onFocus?: CanvasControlWithProps<any>
}

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
  controlModeOrder: ControlMode[]
  labels?: {
    top?: string
    bottom?: string
    left?: string
    right?: string
    horizontal?: string
    vertical?: string
    oneValue?: string
  }
  tooltips?: {
    oneValue?: string
    perDirection?: string
    perSide?: string
  }
  canvasControls?: {
    top?: CanvasControls
    left?: CanvasControls
    bottom?: CanvasControls
    right?: CanvasControls
  }
  numberType: CSSNumberType
}

function getInitialMode(
  aggOne: CSSNumber | null,
  aggHorizontal: CSSNumber | null,
  aggVertical: CSSNumber | null,
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
    update: (v: CSSNumber) => void,
    updateShorthand: UpdateShorthand | null,
    sides: Array<ControlCSSNumber>,
    keepSides: Partial<Sides>,
    transient?: boolean,
  ) =>
  (input: UnknownOrEmptyInput<CSSNumber>) => {
    if (!isCSSNumber(input)) {
      return
    }
    update(input)
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

export const SplitChainedNumberInput = React.memo((props: SplitChainedNumberInputProps<any>) => {
  const { name, top, left, bottom, right, controlModeOrder, canvasControls, numberType } = props

  const [oneValue, setOneValue] = React.useState<CSSNumber | null>(null)
  const [horizontal, setHorizontal] = React.useState<CSSNumber | null>(null)
  const [vertical, setVertical] = React.useState<CSSNumber | null>(null)
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

  const isCmdPressedRef = useRefEditorState((store) => store.editor.keysPressed.cmd === true)

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
    const newOneValue = getSharedValueIfEqualSides(allSides)
    setOneValue(newOneValue)
    const newHorizontal = getSharedValueIfEqualSides(sidesHorizontal)
    setHorizontal(newHorizontal)
    const newVertical = getSharedValueIfEqualSides(sidesVertical)
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
    const delta = isCmdPressedRef.current ? -1 : 1
    const index = controlModeOrder.indexOf(mode) + delta
    setMode(controlModeOrder[wrapValue(index, 0, controlModeOrder.length - 1)])
  }, [isCmdPressedRef, mode, controlModeOrder])

  const updateShorthandIfUsed = React.useMemo(() => {
    const allUnset =
      top.controlStatus === 'trivial-default' &&
      bottom.controlStatus === 'trivial-default' &&
      left.controlStatus === 'trivial-default' &&
      right.controlStatus === 'trivial-default'

    const useShorthand = props.shorthand.controlStatus === 'simple' || allUnset

    return useShorthand ? props.updateShorthand : null
  }, [props.shorthand, props.updateShorthand, top, bottom, left, right])

  const onSubmitValueOne = React.useCallback(
    (transient: boolean) => () => {
      return onSubmitValueShorthand(setOneValue, updateShorthandIfUsed, allSides, {}, transient)
    },
    [updateShorthandIfUsed, allSides],
  )

  const onSubmitValueHorizontal = React.useCallback(
    (transient: boolean) => () => {
      return onSubmitValueShorthand(
        setHorizontal,
        updateShorthandIfUsed,
        sidesHorizontal,
        excludeHorizontal,
        transient,
      )
    },
    [updateShorthandIfUsed, sidesHorizontal, excludeHorizontal],
  )

  const onSubmitValueVertical = React.useCallback(
    (transient: boolean) => () => {
      return onSubmitValueShorthand(
        setVertical,
        updateShorthandIfUsed,
        sidesVertical,
        excludeVertical,
        transient,
      )
    },
    [updateShorthandIfUsed, sidesVertical, excludeVertical],
  )

  const setHoveredCanvasControls = useSetAtom(InspectorHoveredCanvasControls)
  const setFocusedCanvasControls = useSetAtom(InspectorFocusedCanvasControls)

  const chainedPropsToRender: Array<Omit<NumberInputProps, 'chained' | 'id'>> =
    React.useMemo(() => {
      const {
        top: topCanvasControls,
        right: rightCanvasControls,
        bottom: bottomCanvasControls,
        left: leftCanvasControls,
      } = canvasControls ?? {}

      const onMouseEnterForControls =
        (controls: Array<CanvasControlWithProps<any> | undefined>) => () =>
          setHoveredCanvasControls(
            controls.filter((c) => c != undefined) as Array<CanvasControlWithProps<any>>,
          )
      const onFocusForControls = (controls: Array<CanvasControlWithProps<any> | undefined>) => () =>
        setFocusedCanvasControls(
          controls.filter((c) => c != undefined) as Array<CanvasControlWithProps<any>>,
        )

      const onMouseLeave = () => setHoveredCanvasControls([])
      const onBlur = () => setFocusedCanvasControls([])

      switch (mode) {
        case 'one-value':
          return [
            {
              style: { width: '100%' },
              value: oneValue,
              DEPRECATED_labelBelow: props.labels?.oneValue ?? '↔',
              minimum: 0,
              onSubmitValue: onSubmitValueOne(false)(),
              onTransientSubmitValue: onSubmitValueOne(true)(),
              numberType: numberType,
              defaultUnitToHide: 'px',
              controlStatus: allSides[0].controlStatus,
              onMouseEnter: onMouseEnterForControls([
                topCanvasControls?.onHover,
                rightCanvasControls?.onHover,
                bottomCanvasControls?.onHover,
                leftCanvasControls?.onHover,
              ]),
              onMouseLeave: onMouseLeave,
              inputProps: {
                onFocus: onFocusForControls([
                  topCanvasControls?.onFocus,
                  rightCanvasControls?.onFocus,
                  bottomCanvasControls?.onFocus,
                  leftCanvasControls?.onFocus,
                ]),
                onBlur: onBlur,
              },
              testId: `${name}-one`,
            },
          ]
        case 'per-direction':
          return [
            {
              value: horizontal,
              DEPRECATED_labelBelow: props.labels?.horizontal ?? 'H',
              minimum: 0,
              onSubmitValue: onSubmitValueHorizontal(false)(),
              onTransientSubmitValue: onSubmitValueHorizontal(true)(),
              numberType: numberType,
              controlStatus: sidesHorizontal[0].controlStatus,
              defaultUnitToHide: 'px',
              onMouseEnter: onMouseEnterForControls([
                rightCanvasControls?.onHover,
                leftCanvasControls?.onHover,
              ]),
              onMouseLeave: onMouseLeave,
              inputProps: {
                onFocus: onFocusForControls([
                  rightCanvasControls?.onFocus,
                  leftCanvasControls?.onFocus,
                ]),
                onBlur: onBlur,
              },
              testId: `${name}-H`,
            },
            {
              value: vertical,
              DEPRECATED_labelBelow: props.labels?.vertical ?? 'V',
              minimum: 0,
              onSubmitValue: onSubmitValueVertical(false)(),
              onTransientSubmitValue: onSubmitValueVertical(true)(),
              numberType: numberType,
              controlStatus: sidesVertical[0].controlStatus,
              defaultUnitToHide: 'px',
              onMouseEnter: onMouseEnterForControls([
                topCanvasControls?.onHover,
                bottomCanvasControls?.onHover,
              ]),
              onMouseLeave: onMouseLeave,
              inputProps: {
                onFocus: onFocusForControls([
                  topCanvasControls?.onFocus,
                  bottomCanvasControls?.onFocus,
                ]),
                onBlur: onBlur,
              },
              testId: `${name}-V`,
            },
          ]
        case 'per-side':
          return [
            {
              value: top.value,
              DEPRECATED_labelBelow: props.labels?.top ?? 'T',
              minimum: 0,
              onSubmitValue: onSubmitValue(top),
              onTransientSubmitValue: onTransientSubmitValue(top),
              controlStatus: top.controlStatus,
              numberType: numberType,
              defaultUnitToHide: 'px',
              onMouseEnter: onMouseEnterForControls([topCanvasControls?.onHover]),
              onMouseLeave: onMouseLeave,
              inputProps: {
                onFocus: onFocusForControls([topCanvasControls?.onFocus]),
                onBlur: onBlur,
              },
              testId: `${name}-T`,
            },
            {
              value: right.value,
              DEPRECATED_labelBelow: props.labels?.right ?? 'R',
              minimum: 0,
              onSubmitValue: onSubmitValue(right),
              onTransientSubmitValue: onTransientSubmitValue(right),
              controlStatus: right.controlStatus,
              numberType: numberType,
              defaultUnitToHide: 'px',
              onMouseEnter: onMouseEnterForControls([rightCanvasControls?.onHover]),
              onMouseLeave: onMouseLeave,
              inputProps: {
                onFocus: onFocusForControls([rightCanvasControls?.onFocus]),
                onBlur: onBlur,
              },
              testId: `${name}-R`,
            },
            {
              value: bottom.value,
              DEPRECATED_labelBelow: props.labels?.bottom ?? 'B',
              minimum: 0,
              onSubmitValue: onSubmitValue(bottom),
              onTransientSubmitValue: onTransientSubmitValue(bottom),
              controlStatus: bottom.controlStatus,
              numberType: numberType,
              defaultUnitToHide: 'px',
              onMouseEnter: onMouseEnterForControls([bottomCanvasControls?.onHover]),
              onMouseLeave: onMouseLeave,
              inputProps: {
                onFocus: onFocusForControls([bottomCanvasControls?.onFocus]),
                onBlur: onBlur,
              },
              testId: `${name}-B`,
            },
            {
              value: left.value,
              DEPRECATED_labelBelow: props.labels?.left ?? 'L',
              minimum: 0,
              onSubmitValue: onSubmitValue(left),
              onTransientSubmitValue: onTransientSubmitValue(left),
              controlStatus: left.controlStatus,
              numberType: numberType,
              defaultUnitToHide: 'px',
              onMouseEnter: onMouseEnterForControls([leftCanvasControls?.onHover]),
              onMouseLeave: onMouseLeave,
              inputProps: {
                onFocus: onFocusForControls([leftCanvasControls?.onFocus]),
                onBlur: onBlur,
              },
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
      props.labels,
      onSubmitValueOne,
      onSubmitValueHorizontal,
      onSubmitValueVertical,
      canvasControls,
      setHoveredCanvasControls,
      setFocusedCanvasControls,
      numberType,
    ])

  const tooltipTitle = React.useMemo(() => {
    switch (mode) {
      case 'one-value':
        return props.tooltips?.oneValue ?? mode
      case 'per-direction':
        return props.tooltips?.perDirection ?? mode
      case 'per-side':
        return props.tooltips?.perSide ?? mode
      case null:
        return ''
      default:
        assertNever(mode)
    }
  }, [mode, props.tooltips])

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
        <SquareButton data-testid={`${name}-cycle-mode`} onClick={cycleToNextMode}>
          {modeIcon}
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
