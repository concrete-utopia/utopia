import { useSetAtom } from 'jotai'
import React from 'react'
import { mapDropNulls } from '../../../../../core/shared/array-utils'
import { emptyComments, jsExpressionValue } from '../../../../../core/shared/element-template'
import type { ElementPath, PropertyPath } from '../../../../../core/shared/project-file-types'
import * as PP from '../../../../../core/shared/property-path'
import { assertNever } from '../../../../../core/shared/utils'
import type { NumberInputProps } from '../../../../../uuiui'
import {
  ChainedNumberInput,
  InspectorSectionIcons,
  SquareButton,
  Tooltip,
} from '../../../../../uuiui'
import type { EditorAction, EditorDispatch } from '../../../../editor/action-types'
import {
  setProp_UNSAFE,
  transientActions,
  unsetProperty,
} from '../../../../editor/actions/action-creators'
import { Substores, useEditorState, useRefEditorState } from '../../../../editor/store/store-hook'
import type { ControlStatus, PropertyStatus } from '../../../common/control-status'
import type { CSSNumber, CSSNumberType, UnknownOrEmptyInput } from '../../../common/css-utils'
import { isCSSNumber, isEmptyInputValue, printCSSNumber } from '../../../common/css-utils'
import type { CanvasControlWithProps } from '../../../common/inspector-atoms'
import {
  InspectorFocusedCanvasControls,
  InspectorHoveredCanvasControls,
} from '../../../common/inspector-atoms'
import { when } from '../../../../../utils/react-conditionals'

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
function getSharedValueIfEqualSides(values: ControlCSSNumber[]): ControlCSSNumber | null {
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
  return values[0]
}

export function areAllSidesSet(values: ControlCSSNumber[]): boolean {
  return values.every((v) => isControlStatusActive(v.controlStatus))
}

export interface SidesAbstract<T> {
  top: T
  bottom: T
  left: T
  right: T
}

export type Sides = SidesAbstract<CSSNumber>
export type SidesCSSNumber = SidesAbstract<ControlCSSNumber>

interface CanvasControls {
  onHover?: CanvasControlWithProps<any>
  onFocus?: CanvasControlWithProps<any>
}

export interface SplitChainedNumberInputValues {
  oneValue: ControlCSSNumber | null
  twoValue: { horizontal: ControlCSSNumber | null; vertical: ControlCSSNumber | null }
  fourValue: {
    top: ControlCSSNumber | null
    right: ControlCSSNumber | null
    bottom: ControlCSSNumber | null
    left: ControlCSSNumber | null
  }
}

export interface SplitChainedNumberInputProps<T> {
  name: string
  mode: ControlMode | null
  onCycleMode: () => void
  labels?: {
    top?: React.ReactChild
    bottom?: React.ReactChild
    left?: React.ReactChild
    right?: React.ReactChild
    horizontal?: React.ReactChild
    vertical?: React.ReactChild
    oneValue?: React.ReactChild
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
  values: SplitChainedNumberInputValues
  numberType: CSSNumberType
  eventHandler: SplitChainedNumberInputEventHandler
  fourSidesOrder?: Array<SplitFourValueSide>
}

export type SplitChainedNumberInputEventHandler = (
  e: SplitChainedEvent,
  isTransient: boolean,
) => void

export function getInitialMode(
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

type CSSNumberOrNull = CSSNumber | null

export type SplitFourValueSide = 'T' | 'R' | 'B' | 'L'

export type FourValue =
  | { type: 'T'; value: CSSNumberOrNull }
  | { type: 'R'; value: CSSNumberOrNull }
  | { type: 'B'; value: CSSNumberOrNull }
  | { type: 'L'; value: CSSNumberOrNull }

export type TwoValue = { type: 'V'; value: CSSNumberOrNull } | { type: 'H'; value: CSSNumberOrNull }

export type SplitChainedEvent =
  | { type: 'one-value'; value: CSSNumberOrNull }
  | { type: 'two-value'; value: TwoValue }
  | { type: 'four-value'; value: FourValue }

export function splitChainedEventValueForProp(
  prop: SplitFourValueSide,
  e: SplitChainedEvent,
): CSSNumber | null {
  switch (e.type) {
    case 'one-value':
      return e.value
    case 'two-value':
      if (e.value.type === 'V') {
        if (prop === 'T' || prop === 'B') {
          return e.value.value
        }
      } else if (e.value.type === 'H') {
        if (prop === 'L' || prop === 'R') {
          return e.value.value
        }
      }

      return null
    case 'four-value':
      if (prop === e.value.type) {
        return e.value.value
      }

      return null
    default:
      assertNever(e)
  }
}

export type SplitControlValues = {
  oneValue: CSSNumberOrNull
  horizontal: CSSNumberOrNull
  vertical: CSSNumberOrNull
  top: CSSNumber
  right: CSSNumber
  bottom: CSSNumber
  left: CSSNumber
}

const emptyCSSNumber: CSSNumber = { value: 0, unit: 'px' }
const actionsForSplitChainedEvent =
  (
    e: SplitChainedEvent,
    element: ElementPath,
    shorthand: PropertyPath,
    longhand: {
      T: PropertyPath
      R: PropertyPath
      B: PropertyPath
      L: PropertyPath
    },
  ) =>
  (useShorthand: boolean, aggregates: SplitControlValues): Array<EditorAction> => {
    function setProp(path: PropertyPath, values: (CSSNumber | null)[]): EditorAction {
      const normalizedValues = values.map((v) => (useShorthand && v == null ? emptyCSSNumber : v))
      return setProp_UNSAFE(
        element,
        path,
        jsExpressionValue(
          normalizedValues.length === 1 && normalizedValues[0] != null
            ? printCSSNumber(normalizedValues[0], 'px')
            : mapDropNulls((v) => {
                if (v == null) {
                  return null
                }
                return {
                  ...v,
                  unit: useShorthand && v.unit == null ? 'px' : v.unit,
                }
              }, normalizedValues)
                .map((v) => printCSSNumber(v, null))
                .join(' '),
          emptyComments,
        ),
      )
    }

    const horizontal = aggregates.horizontal ?? emptyCSSNumber
    const vertical = aggregates.vertical ?? emptyCSSNumber

    const unsetAllIndividual = [
      unsetProperty(element, longhand.T),
      unsetProperty(element, longhand.R),
      unsetProperty(element, longhand.B),
      unsetProperty(element, longhand.L),
    ]

    function actionsOrUnset(
      actions: EditorAction[],
      unset: boolean,
      unsetPaths: PropertyPath[],
    ): EditorAction[] {
      if (unset) {
        return unsetPaths.map((path) => unsetProperty(element, path))
      }
      return actions
    }

    function shouldUnset(): boolean {
      switch (e.type) {
        case 'one-value':
          return e.value == null

        case 'two-value':
          if (e.value.value != null) {
            return false
          }

          if (useShorthand) {
            if (e.value.type === 'V') {
              return aggregates.horizontal === null || aggregates.horizontal.value === 0
            } else {
              return aggregates.vertical === null || aggregates.vertical.value === 0
            }
          } else {
            return true
          }

        case 'four-value':
          if (e.value.value != null) {
            return false
          }

          let otherSides: CSSNumber[] = []
          if (e.value.type !== 'T') {
            otherSides.push(aggregates.top)
          }
          if (e.value.type !== 'R') {
            otherSides.push(aggregates.right)
          }
          if (e.value.type !== 'B') {
            otherSides.push(aggregates.bottom)
          }
          if (e.value.type !== 'L') {
            otherSides.push(aggregates.left)
          }

          return !useShorthand || otherSides.every((o) => o == null)

        default:
          assertNever(e)
      }
    }

    function getUnsetPaths(): PropertyPath[] {
      if (useShorthand) {
        return [shorthand]
      }

      switch (e.type) {
        case 'one-value':
          return [longhand.T, longhand.R, longhand.B, longhand.L]

        case 'two-value':
          if (e.value.type === 'V') {
            return [longhand.T, longhand.B]
          }
          return [longhand.R, longhand.L]

        case 'four-value':
          if (e.value.type === 'T') {
            return [longhand.T]
          } else if (e.value.type === 'R') {
            return [longhand.R]
          } else if (e.value.type === 'B') {
            return [longhand.B]
          } else {
            return [longhand.L]
          }

        default:
          assertNever(e)
      }
    }

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
                ...(e.value.type === 'V'
                  ? [setProp(shorthand, [e.value.value, horizontal])]
                  : [setProp(shorthand, [vertical, e.value.value])]),
              ]
            : [
                unsetProperty(element, shorthand),
                ...(e.value.type === 'V'
                  ? [setProp(longhand.T, [e.value.value]), setProp(longhand.B, [e.value.value])]
                  : [setProp(longhand.L, [e.value.value]), setProp(longhand.R, [e.value.value])]),
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

    return actionsOrUnset(getActions(), shouldUnset(), getUnsetPaths())
  }

export const longhandShorthandEventHandler = (
  shorthand: string,
  longhands: {
    T: string
    R: string
    B: string
    L: string
  },
  selectedViewsRef: { current: Array<ElementPath> },
  useShorthand: boolean,
  aggregates: SplitControlValues,
  allUnset: boolean,
  dispatch: EditorDispatch,
  extraActionHandling?: (
    e: SplitChainedEvent,
    aggregates: SplitControlValues,
  ) => Array<EditorAction>,
): SplitChainedNumberInputEventHandler => {
  return (e: SplitChainedEvent, isTransient: boolean) => {
    const shouldUseShorthandWith4Value =
      e.type === 'four-value' ? useShorthand && !allUnset : useShorthand
    let actions = actionsForSplitChainedEvent(
      e,
      selectedViewsRef.current[0],
      PP.create('style', shorthand),
      {
        T: PP.create('style', longhands.T),
        R: PP.create('style', longhands.R),
        B: PP.create('style', longhands.B),
        L: PP.create('style', longhands.L),
      },
    )(shouldUseShorthandWith4Value, aggregates)

    if (extraActionHandling != null) {
      actions.push(...extraActionHandling(e, aggregates))
    }

    if (isTransient) {
      dispatch([transientActions(actions)])
    } else {
      dispatch(actions)
    }
  }
}

const whenCSSNumber = (fn: (v: CSSNumber | null) => any) => (v: UnknownOrEmptyInput<CSSNumber>) => {
  if (isEmptyInputValue(v)) {
    fn(null)
  } else if (isCSSNumber(v)) {
    fn(v)
  }
}

export const SplitChainedNumberInput = React.memo((props: SplitChainedNumberInputProps<any>) => {
  const {
    name,
    canvasControls,
    numberType,
    eventHandler,
    labels,
    values,
    fourSidesOrder = ['T', 'R', 'L', 'B'],
  } = props

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

      const onSubmitValueOne = (isTransient: boolean) =>
        whenCSSNumber((v) => eventHandler({ type: 'one-value', value: v }, isTransient))

      const onSubmitValueHorizontal = (isTransient: boolean) =>
        whenCSSNumber((v) =>
          eventHandler({ type: 'two-value', value: { type: 'H', value: v } }, isTransient),
        )

      const onSubmitValueVertical = (isTransient: boolean) =>
        whenCSSNumber((v) =>
          eventHandler({ type: 'two-value', value: { type: 'V', value: v } }, isTransient),
        )

      const onSubmitValueTop = (isTransient: boolean) =>
        whenCSSNumber((v) =>
          eventHandler({ type: 'four-value', value: { type: 'T', value: v } }, isTransient),
        )

      const onSubmitValueRight = (isTransient: boolean) =>
        whenCSSNumber((v) =>
          eventHandler({ type: 'four-value', value: { type: 'R', value: v } }, isTransient),
        )

      const onSubmitValueBottom = (isTransient: boolean) =>
        whenCSSNumber((v) =>
          eventHandler({ type: 'four-value', value: { type: 'B', value: v } }, isTransient),
        )

      const onSubmitValueLeft = (isTransient: boolean) =>
        whenCSSNumber((v) =>
          eventHandler({ type: 'four-value', value: { type: 'L', value: v } }, isTransient),
        )

      switch (props.mode) {
        case 'one-value':
          return [
            {
              style: { width: '50%' },
              value: values.oneValue?.value,
              scrubbable_innerlabel: labels?.oneValue ?? '↔',
              minimum: 0,
              onSubmitValue: onSubmitValueOne(false),
              onTransientSubmitValue: onSubmitValueOne(true),
              numberType: numberType,
              defaultUnitToHide: 'px',
              controlStatus: values.oneValue?.controlStatus,
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
              style: { width: '48%' },
              value: values.twoValue.vertical?.value,
              scrubbable_innerlabel: labels?.vertical ?? 'V',
              minimum: 0,
              onSubmitValue: onSubmitValueVertical(false),
              onTransientSubmitValue: onSubmitValueVertical(true),
              numberType: numberType,
              controlStatus: values.twoValue.vertical?.controlStatus,
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
            {
              style: { width: '48%' },
              value: values.twoValue.horizontal?.value,
              scrubbable_innerlabel: labels?.horizontal ?? 'H',
              minimum: 0,
              onSubmitValue: onSubmitValueHorizontal(false),
              onTransientSubmitValue: onSubmitValueHorizontal(true),
              numberType: numberType,
              controlStatus: values.twoValue.horizontal?.controlStatus,
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
          ]
        case 'per-side':
          const sides: Record<SplitFourValueSide, Omit<NumberInputProps, 'chained' | 'id'>> = {
            T: {
              style: { width: '48%' },
              value: values.fourValue.top?.value,
              scrubbable_innerlabel: labels?.top ?? 'T',
              minimum: 0,
              onSubmitValue: onSubmitValueTop(false),
              onTransientSubmitValue: onSubmitValueTop(true),
              controlStatus: values.fourValue.top?.controlStatus,
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
            R: {
              style: { width: '48%' },
              value: values.fourValue.right?.value,
              scrubbable_innerlabel: labels?.right ?? 'R',
              minimum: 0,
              onSubmitValue: onSubmitValueRight(false),
              onTransientSubmitValue: onSubmitValueRight(true),
              controlStatus: values.fourValue.right?.controlStatus,
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
            L: {
              style: { width: '48%' },
              value: values.fourValue.left?.value,
              scrubbable_innerlabel: labels?.left ?? 'L',
              minimum: 0,
              onSubmitValue: onSubmitValueLeft(false),
              onTransientSubmitValue: onSubmitValueLeft(true),
              controlStatus: values.fourValue.left?.controlStatus,
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
            B: {
              style: { width: '48%' },
              value: values.fourValue.bottom?.value,
              scrubbable_innerlabel: labels?.bottom ?? 'B',
              minimum: 0,
              onSubmitValue: onSubmitValueBottom(false),
              onTransientSubmitValue: onSubmitValueBottom(true),
              controlStatus: values.fourValue.bottom?.controlStatus,
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
          }
          return fourSidesOrder.map((side) => sides[side])
        case null:
          return []
        default:
          assertNever(props.mode)
      }
    }, [
      name,
      labels,
      canvasControls,
      setHoveredCanvasControls,
      setFocusedCanvasControls,
      numberType,
      eventHandler,
      props.mode,
      values,
      fourSidesOrder,
    ])

  const tooltipTitle = React.useMemo(() => {
    switch (props.mode) {
      case 'one-value':
        return props.tooltips?.oneValue ?? props.mode
      case 'per-direction':
        return props.tooltips?.perDirection ?? props.mode
      case 'per-side':
        return props.tooltips?.perSide ?? props.mode
      case null:
        return ''
      default:
        assertNever(props.mode)
    }
  }, [props.mode, props.tooltips])

  const modeIcon = React.useMemo(() => {
    switch (props.mode) {
      case 'one-value':
        return <InspectorSectionIcons.SplitFull />
      case 'per-direction':
        return <InspectorSectionIcons.SplitHalf />
      case 'per-side':
        return <InspectorSectionIcons.SplitQuarter />
      case null:
        return null
      default:
        assertNever(props.mode)
    }
  }, [props.mode])

  const cycleModeControl = React.useMemo(() => {
    return (
      <Tooltip title={tooltipTitle}>
        <SquareButton data-testid={`${name}-cycle-mode`} onClick={props.onCycleMode} highlight>
          {modeIcon}
        </SquareButton>
      </Tooltip>
    )
  }, [modeIcon, name, props.onCycleMode, tooltipTitle])

  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'row',
        gap: 10,
        justifyContent: 'space-between',
        flex: 1,
      }}
    >
      <ChainedNumberInput
        idPrefix={name}
        style={{ flex: 1, gap: 8 }}
        propsArray={chainedPropsToRender}
        wrap={true}
      />
      {cycleModeControl}
    </div>
  )
})

export function isControlModeApplicable(
  overriddenMode: ControlMode,
  oneValue: CSSNumberOrNull,
  horizontal: CSSNumberOrNull,
  vertical: CSSNumberOrNull,
): boolean {
  if (overriddenMode === 'one-value' && oneValue == null) {
    return false
  }
  if (overriddenMode === 'per-direction' && horizontal == null && vertical == null) {
    return false
  }
  return true
}

interface SplitControlAggregates {
  allSides: Array<ControlCSSNumber>
  horizontal: ControlCSSNumber | null
  vertical: ControlCSSNumber | null
  oneValue: ControlCSSNumber | null
}

export function aggregateGroups(sides: SidesAbstract<ControlCSSNumber>): SplitControlAggregates {
  const { top, left, bottom, right } = sides

  return {
    allSides: [top, left, bottom, right],
    horizontal: getSharedValueIfEqualSides([left, right]),
    vertical: getSharedValueIfEqualSides([top, bottom]),
    oneValue: getSharedValueIfEqualSides([top, left, bottom, right]),
  }
}

export function getSplitChainedNumberInputValues(
  aggregates: SplitControlAggregates,
  sides: SidesAbstract<ControlCSSNumber>,
): SplitChainedNumberInputValues {
  return {
    oneValue: aggregates.oneValue,
    twoValue: { horizontal: aggregates.horizontal, vertical: aggregates.vertical },
    fourValue: { top: sides.top, right: sides.right, bottom: sides.bottom, left: sides.left },
  }
}

export function getSplitControlValues(
  aggregates: SplitControlAggregates,
  sides: SidesAbstract<ControlCSSNumber>,
): SplitControlValues {
  return {
    oneValue: aggregates.oneValue?.value ?? null,
    horizontal: aggregates.horizontal?.value ?? null,
    vertical: aggregates.vertical?.value ?? null,
    top: sides.top.value,
    right: sides.right.value,
    bottom: sides.bottom.value,
    left: sides.left.value,
  }
}
