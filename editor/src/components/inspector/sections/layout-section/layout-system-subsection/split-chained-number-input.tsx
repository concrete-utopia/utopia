import { useAtom } from 'jotai'
import React from 'react'
import { mapDropNulls } from '../../../../../core/shared/array-utils'
import { emptyComments, jsxAttributeValue } from '../../../../../core/shared/element-template'
import { wrapValue } from '../../../../../core/shared/math-utils'
import { ElementPath, PropertyPath } from '../../../../../core/shared/project-file-types'
import * as PP from '../../../../../core/shared/property-path'
import { assertNever } from '../../../../../core/shared/utils'
import {
  ChainedNumberInput,
  InspectorSectionIcons,
  NumberInputProps,
  SquareButton,
  Tooltip,
} from '../../../../../uuiui'
import { EditorAction, EditorDispatch } from '../../../../editor/action-types'
import { setProp_UNSAFE, unsetProperty } from '../../../../editor/actions/action-creators'
import { useRefEditorState } from '../../../../editor/store/store-hook'
import { ControlStatus, PropertyStatus } from '../../../common/control-status'
import {
  CSSNumber,
  CSSNumberType,
  isCSSNumber,
  printCSSNumber,
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
  | 'per-side' // one distinct value per side (TRBL)

interface ControlCSSNumber {
  controlStatus: ControlStatus
  propertyStatus: PropertyStatus
  value: CSSNumber
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
  eventHandler: SplitChainedNumberInputEventHandler
}

type SplitChainedNumberInputEventHandler = (
  e: SplitChainedEvent,
  aggregates: SplitControlValues,
  useShorthand: boolean,
) => void

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

export type FourValue =
  | { type: 'T'; value: CSSNumber }
  | { type: 'R'; value: CSSNumber }
  | { type: 'B'; value: CSSNumber }
  | { type: 'L'; value: CSSNumber }

export type TwoValue = { type: 'V'; value: CSSNumber } | { type: 'H'; value: CSSNumber }

export type SplitChainedEvent =
  | { type: 'one-value'; value: CSSNumber }
  | { type: 'two-value'; value: TwoValue }
  | { type: 'four-value'; value: FourValue }

export type SplitControlValues = {
  oneValue: CSSNumber | null
  horizontal: CSSNumber | null
  vertical: CSSNumber | null
  top: CSSNumber
  right: CSSNumber
  bottom: CSSNumber
  left: CSSNumber
}

const handleSplitChainedEvent =
  (
    e: SplitChainedEvent,
    dispatch: EditorDispatch,
    element: ElementPath,
    shorthand: PropertyPath,
    longhand: {
      T: PropertyPath
      R: PropertyPath
      B: PropertyPath
      L: PropertyPath
    },
  ) =>
  (useShorthand: boolean, aggregates: SplitControlValues): void => {
    const setProp = (path: PropertyPath, values: (CSSNumber | null)[]): EditorAction => {
      return setProp_UNSAFE(
        element,
        path,
        jsxAttributeValue(
          mapDropNulls((v) => v, values)
            .map((v) => printCSSNumber(v, null))
            .join(' '),
          emptyComments,
        ),
      )
    }

    const horizontal = aggregates.horizontal ?? { value: 0, unit: 'px' }
    const vertical = aggregates.vertical ?? { value: 0, unit: 'px' }

    const unsetAllIndividual = [
      unsetProperty(element, longhand.T),
      unsetProperty(element, longhand.R),
      unsetProperty(element, longhand.B),
      unsetProperty(element, longhand.L),
    ]

    const getActions = (): Array<EditorAction> => {
      switch (e.type) {
        case 'one-value':
          return useShorthand
            ? [...unsetAllIndividual, setProp(shorthand, [e.value])]
            : [
                setProp(longhand.T, [e.value]),
                setProp(longhand.R, [e.value]),
                setProp(longhand.B, [e.value]),
                setProp(longhand.L, [e.value]),
              ]
        case 'two-value':
          return useShorthand
            ? [
                ...unsetAllIndividual,
                ...(e.value.type === 'V' ? [setProp(shorthand, [e.value.value, horizontal])] : []),
                ...(e.value.type === 'H' ? [setProp(shorthand, [vertical, e.value.value])] : []),
              ]
            : [
                unsetProperty(element, shorthand),
                ...(e.value.type === 'V'
                  ? [setProp(longhand.T, [e.value.value]), setProp(longhand.B, [e.value.value])]
                  : []),
                ...(e.value.type === 'H'
                  ? [setProp(longhand.L, [e.value.value]), setProp(longhand.R, [e.value.value])]
                  : []),
              ]
        case 'four-value':
          return useShorthand
            ? [
                ...unsetAllIndividual,
                setProp(shorthand, [
                  // order here is important! TRBL
                  e.value.type === 'T' ? e.value.value : aggregates.top,
                  e.value.type === 'R' ? e.value.value : aggregates.right,
                  e.value.type === 'B' ? e.value.value : aggregates.bottom,
                  e.value.type === 'L' ? e.value.value : aggregates.left,
                ]),
              ]
            : [
                // order here is important! TRBL
                unsetProperty(element, shorthand),
                ...(e.value.type === 'T' ? [setProp(longhand.T, [e.value.value])] : []),
                ...(e.value.type === 'R' ? [setProp(longhand.R, [e.value.value])] : []),
                ...(e.value.type === 'B' ? [setProp(longhand.B, [e.value.value])] : []),
                ...(e.value.type === 'L' ? [setProp(longhand.L, [e.value.value])] : []),
              ]
        default:
          assertNever(e)
      }
    }

    dispatch(getActions())
  }

export const longhandShorthandEventHandler = (
  shorthand: string,
  longhands: {
    T: string
    R: string
    B: string
    L: string
  },
  elementPath: ElementPath,
  dispatch: EditorDispatch,
): SplitChainedNumberInputEventHandler => {
  return (e: SplitChainedEvent, aggregates: SplitControlValues, useShorthand: boolean) => {
    handleSplitChainedEvent(e, dispatch, elementPath, PP.create('style', shorthand), {
      T: PP.create('style', longhands.T),
      R: PP.create('style', longhands.R),
      B: PP.create('style', longhands.B),
      L: PP.create('style', longhands.L),
    })(useShorthand, aggregates)
  }
}

const whenCSSNumber = (fn: (v: CSSNumber) => any) => (v: UnknownOrEmptyInput<CSSNumber>) => {
  if (!isCSSNumber(v)) {
    return
  }
  fn(v)
}

export const SplitChainedNumberInput = React.memo((props: SplitChainedNumberInputProps<any>) => {
  const {
    name,
    top,
    left,
    bottom,
    right,
    controlModeOrder,
    canvasControls,
    numberType,
    eventHandler,
    labels,
  } = props

  const [oneValue, setOneValue] = React.useState<CSSNumber | null>(null)
  const [horizontal, setHorizontal] = React.useState<CSSNumber | null>(null)
  const [vertical, setVertical] = React.useState<CSSNumber | null>(null)
  const [mode, setMode] = React.useState<ControlMode | null>(null)

  const allSides = React.useMemo(() => [top, left, bottom, right], [top, left, bottom, right])
  const sidesHorizontal = React.useMemo(() => [left, right], [left, right])
  const sidesVertical = React.useMemo(() => [top, bottom], [top, bottom])

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

  const useShorthand = React.useMemo(() => {
    const allUnset =
      top.controlStatus === 'trivial-default' &&
      bottom.controlStatus === 'trivial-default' &&
      left.controlStatus === 'trivial-default' &&
      right.controlStatus === 'trivial-default'

    return props.shorthand.controlStatus === 'simple' || allUnset
  }, [top, left, bottom, right, props.shorthand])

  const [, setHoveredCanvasControls] = useAtom(InspectorHoveredCanvasControls)
  const [, setFocusedCanvasControls] = useAtom(InspectorFocusedCanvasControls)

  const aggregates: SplitControlValues = React.useMemo(() => {
    return {
      oneValue,
      horizontal,
      vertical,
      top: top.value,
      right: right.value,
      bottom: bottom.value,
      left: left.value,
    }
  }, [oneValue, horizontal, vertical, top, left, bottom, right])

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

      const onSubmitValueOne = whenCSSNumber((v) =>
        eventHandler({ type: 'one-value', value: v }, aggregates, useShorthand),
      )

      const onSubmitValueHorizontal = whenCSSNumber((v) =>
        eventHandler(
          { type: 'two-value', value: { type: 'H', value: v } },
          aggregates,
          useShorthand,
        ),
      )

      const onSubmitValueVertical = whenCSSNumber((v) =>
        eventHandler(
          { type: 'two-value', value: { type: 'V', value: v } },
          aggregates,
          useShorthand,
        ),
      )

      const onSubmitValueTop = whenCSSNumber((v) =>
        eventHandler(
          { type: 'four-value', value: { type: 'T', value: v } },
          aggregates,
          useShorthand,
        ),
      )

      const onSubmitValueRight = whenCSSNumber((v) =>
        eventHandler(
          { type: 'four-value', value: { type: 'R', value: v } },
          aggregates,
          useShorthand,
        ),
      )

      const onSubmitValueBottom = whenCSSNumber((v) =>
        eventHandler(
          { type: 'four-value', value: { type: 'B', value: v } },
          aggregates,
          useShorthand,
        ),
      )

      const onSubmitValueLeft = whenCSSNumber((v) =>
        eventHandler(
          { type: 'four-value', value: { type: 'L', value: v } },
          aggregates,
          useShorthand,
        ),
      )

      switch (mode) {
        case 'one-value':
          return [
            {
              style: { width: '100%' },
              value: oneValue,
              DEPRECATED_labelBelow: labels?.oneValue ?? '↔',
              minimum: 0,
              onSubmitValue: onSubmitValueOne,
              onTransientSubmitValue: onSubmitValueOne,
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
              DEPRECATED_labelBelow: labels?.horizontal ?? 'H',
              minimum: 0,
              onSubmitValue: onSubmitValueHorizontal,
              onTransientSubmitValue: onSubmitValueHorizontal,
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
              DEPRECATED_labelBelow: labels?.vertical ?? 'V',
              minimum: 0,
              onSubmitValue: onSubmitValueVertical,
              onTransientSubmitValue: onSubmitValueVertical,
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
              DEPRECATED_labelBelow: labels?.top ?? 'T',
              minimum: 0,
              onSubmitValue: onSubmitValueTop,
              onTransientSubmitValue: onSubmitValueTop,
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
              DEPRECATED_labelBelow: labels?.right ?? 'R',
              minimum: 0,
              onSubmitValue: onSubmitValueRight,
              onTransientSubmitValue: onSubmitValueRight,
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
              DEPRECATED_labelBelow: labels?.bottom ?? 'B',
              minimum: 0,
              onSubmitValue: onSubmitValueBottom,
              onTransientSubmitValue: onSubmitValueBottom,
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
              DEPRECATED_labelBelow: labels?.left ?? 'L',
              minimum: 0,
              onSubmitValue: onSubmitValueLeft,
              onTransientSubmitValue: onSubmitValueLeft,
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
      labels,
      canvasControls,
      setHoveredCanvasControls,
      setFocusedCanvasControls,
      numberType,
      eventHandler,
      useShorthand,
      aggregates,
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
