/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { createSelector } from 'reselect'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { optionalMap } from '../../core/shared/optional-utils'
import { ElementPath } from '../../core/shared/project-file-types'
import { intersection } from '../../core/shared/set-utils'
import { assertNever, NO_OP } from '../../core/shared/utils'
import { NumberInput, PopupList, SimpleCSSNumberInput } from '../../uuiui'
import {
  ControlStatus,
  getControlStyles,
  InspectorRowHoverCSS,
  SelectOption,
} from '../../uuiui-deps'
import { useDispatch } from '../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import {
  CSSNumber,
  cssNumber,
  CSSNumberType,
  EmptyInputValue,
  UnknownOrEmptyInput,
} from './common/css-utils'
import { metadataSelector, selectedViewsSelector, useComputedSizeRef } from './inpector-selectors'
import {
  Axis,
  detectFillHugFixedState,
  FixedHugFill,
  FixedHugFillMode,
  getFixedFillHugOptionsForElement,
} from './inspector-common'
import {
  setPropFillStrategies,
  setPropFixedStrategies,
  setPropHugStrategies,
} from './inspector-strategies/inspector-strategies'
import {
  executeFirstApplicableStrategy,
  InspectorStrategy,
} from './inspector-strategies/inspector-strategy'

export const FillFixedHugControlId = (segment: 'width' | 'height'): string =>
  `hug-fixed-fill-${segment}`

function isFixedHugFillEqual(a: FixedHugFill | undefined, b: FixedHugFill | undefined): boolean {
  if (a === undefined && b === undefined) {
    return true
  }

  if (a?.type !== b?.type) {
    return false
  }

  if ((a?.type === 'fixed' && b?.type === 'fixed') || (a?.type === 'fill' && b?.type === 'fill')) {
    return a.value.value === b.value.value && a.value.unit === b.value.unit
  }

  return true
}

export const FillContainerLabel = 'Fill container' as const
export const FixedLabel = 'Fixed' as const
export const HugContentsLabel = 'Hug contents' as const

export function selectOptionLabel(mode: FixedHugFillMode): string {
  switch (mode) {
    case 'fill':
      return FillContainerLabel
    case 'fixed':
      return FixedLabel
    case 'hug':
      return HugContentsLabel
    default:
      assertNever(mode)
  }
}

function selectOption(mode: FixedHugFillMode): SelectOption {
  return {
    value: mode,
    label: selectOptionLabel(mode),
  }
}

interface FillHugFixedControlProps {}

const optionsSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  (metadata, selectedViews) => {
    const applicableOptions: Array<FixedHugFillMode> = [
      ...intersection(
        selectedViews.map((selectedView) =>
          getFixedFillHugOptionsForElement(metadata, selectedView),
        ),
      ),
    ]

    return applicableOptions.map(selectOption)
  },
)

export const FillHugFixedControl = React.memo<FillHugFixedControlProps>((props) => {
  const options = useEditorState(Substores.metadata, optionsSelector, 'FillHugFixedControl options')

  const dispatch = useDispatch()
  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)

  const widthCurrentValue = useEditorState(
    Substores.metadata,
    (store) =>
      detectFillHugFixedState(
        'horizontal',
        metadataSelector(store),
        selectedViewsSelector(store).at(0) ?? null,
      ) ?? undefined,
    'FillHugFixedControl widthCurrentValue',
    isFixedHugFillEqual,
  )
  const widthControlStatus = React.useMemo(
    () => (widthCurrentValue == null ? 'unset' : widthCurrentValue.status),
    [widthCurrentValue],
  )
  const widthControlStyles = React.useMemo(
    () => getControlStyles(widthControlStatus),
    [widthControlStatus],
  )
  const widthInputControlStatus = React.useMemo(
    () => (isNumberInputEnabled(widthCurrentValue) ? widthControlStatus : 'disabled'),
    [widthCurrentValue, widthControlStatus],
  )

  const fillsContainerHorizontallyRef = useRefEditorState(
    (store) =>
      detectFillHugFixedState(
        'horizontal',
        metadataSelector(store),
        selectedViewsSelector(store).at(0) ?? null,
      )?.type === 'fill',
  )

  const widthComputedValueRef = useComputedSizeRef('width')

  const heightCurrentValue = useEditorState(
    Substores.metadata,
    (store) =>
      detectFillHugFixedState(
        'vertical',
        metadataSelector(store),
        selectedViewsSelector(store).at(0) ?? null,
      ) ?? undefined,
    'FillHugFixedControl heightCurrentValue',
    isFixedHugFillEqual,
  )
  const heightControlStatus = React.useMemo(
    () => (heightCurrentValue == null ? 'unset' : heightCurrentValue.status),
    [heightCurrentValue],
  )
  const heightControlStyles = React.useMemo(
    () => getControlStyles(heightControlStatus),
    [heightControlStatus],
  )
  const heightInputControlStatus = React.useMemo(
    () => (isNumberInputEnabled(heightCurrentValue) ? heightControlStatus : 'disabled'),
    [heightCurrentValue, heightControlStatus],
  )

  const fillsContainerVerticallyRef = useRefEditorState(
    (store) =>
      detectFillHugFixedState(
        'vertical',
        metadataSelector(store),
        selectedViewsSelector(store).at(0) ?? null,
      )?.type === 'fill',
  )

  const heightComputedValueRef = useComputedSizeRef('height')

  const onSubmitHeight = React.useCallback(
    ({ value: anyValue }: SelectOption) => {
      const value = anyValue as FixedHugFillMode
      const strategy = strategyForMode(
        heightComputedValueRef.current ?? 0,
        'vertical',
        value,
        fillsContainerHorizontallyRef.current,
      )
      executeFirstApplicableStrategy(
        dispatch,
        metadataRef.current,
        selectedViewsRef.current,
        strategy,
      )
    },
    [
      dispatch,
      fillsContainerHorizontallyRef,
      heightComputedValueRef,
      metadataRef,
      selectedViewsRef,
    ],
  )

  const onAdjustHeight = React.useCallback(
    (value: UnknownOrEmptyInput<CSSNumber>) => {
      if (
        'type' in value &&
        (value.type === 'EMPTY_INPUT_VALUE' || value.type === 'UNKNOWN_INPUT')
      ) {
        return
      }
      if (heightCurrentValue?.type === 'fill') {
        if (value.unit != null && value.unit !== '%') {
          // fill mode only accepts percentage or valueless numbers
          return
        }
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          setPropFillStrategies('vertical', value.value, false),
        )
      }
      if (heightCurrentValue?.type === 'fixed') {
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          setPropFixedStrategies('always', 'vertical', value),
        )
      }
    },
    [dispatch, heightCurrentValue?.type, metadataRef, selectedViewsRef],
  )

  const onAdjustWidth = React.useCallback(
    (value: UnknownOrEmptyInput<CSSNumber>) => {
      if (
        'type' in value &&
        (value.type === 'EMPTY_INPUT_VALUE' || value.type === 'UNKNOWN_INPUT')
      ) {
        return
      }
      if (widthCurrentValue?.type === 'fill') {
        if (value.unit != null && value.unit !== '%') {
          // fill mode only accepts percentage or valueless numbers
          return
        }
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          setPropFillStrategies('horizontal', value.value, false),
        )
      }
      if (widthCurrentValue?.type === 'fixed') {
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          setPropFixedStrategies('always', 'horizontal', value),
        )
      }
    },
    [dispatch, metadataRef, selectedViewsRef, widthCurrentValue?.type],
  )

  const onSubmitWidth = React.useCallback(
    ({ value: anyValue }: SelectOption) => {
      const value = anyValue as FixedHugFillMode
      const strategy = strategyForMode(
        widthComputedValueRef.current ?? 0,
        'horizontal',
        value,
        fillsContainerVerticallyRef.current,
      )
      executeFirstApplicableStrategy(
        dispatch,
        metadataRef.current,
        selectedViewsRef.current,
        strategy,
      )
    },
    [dispatch, fillsContainerVerticallyRef, metadataRef, selectedViewsRef, widthComputedValueRef],
  )

  if (options == null) {
    return null
  }

  const widthValue = optionalMap(pickFixedValue, widthCurrentValue) ?? undefined
  const heightValue = optionalMap(pickFixedValue, heightCurrentValue) ?? undefined

  return (
    <div
      style={{
        display: 'grid',
        gridTemplateRows: '1fr 1fr',
        gridTemplateColumns: '1fr',
      }}
    >
      <div
        style={{
          display: 'grid',
          gridTemplateColumns: '1fr 1fr',
          gap: 4,
          padding: 4,
        }}
        css={InspectorRowHoverCSS}
      >
        <PopupList
          value={optionalMap(selectOption, widthCurrentValue?.type) ?? undefined}
          options={options}
          onSubmitValue={onSubmitWidth}
          controlStyles={widthControlStyles}
        />
        <NumberInput
          id={FillFixedHugControlId('width')}
          testId={FillFixedHugControlId('width')}
          value={widthValue}
          onSubmitValue={onAdjustWidth}
          onTransientSubmitValue={NO_OP}
          onForcedSubmitValue={NO_OP}
          controlStatus={widthInputControlStatus}
          numberType={pickNumberType(widthCurrentValue)}
          incrementControls={true}
          stepSize={1}
          minimum={0}
          maximum={Infinity}
          labelInner={'W'}
          defaultUnitToHide={null}
          focusOnMount={false}
        />
      </div>
      <div
        style={{
          display: 'grid',
          gridTemplateColumns: '1fr 1fr',
          gap: 4,
          padding: 4,
        }}
        css={InspectorRowHoverCSS}
      >
        <PopupList
          value={optionalMap(selectOption, heightCurrentValue?.type) ?? undefined}
          options={options}
          onSubmitValue={onSubmitHeight}
          controlStyles={heightControlStyles}
        />
        <NumberInput
          id={FillFixedHugControlId('height')}
          testId={FillFixedHugControlId('height')}
          value={heightValue}
          onSubmitValue={onAdjustHeight}
          onTransientSubmitValue={NO_OP}
          onForcedSubmitValue={NO_OP}
          controlStatus={heightInputControlStatus}
          numberType={pickNumberType(heightCurrentValue)}
          incrementControls={true}
          stepSize={1}
          minimum={0}
          maximum={Infinity}
          labelInner={'H'}
          defaultUnitToHide={null}
          focusOnMount={false}
        />
      </div>
    </div>
  )
})

function strategyForMode(
  fixedValue: number,
  axis: Axis,
  mode: FixedHugFillMode,
  otherAxisSetToFill: boolean,
): Array<InspectorStrategy> {
  switch (mode) {
    case 'fill':
      return setPropFillStrategies(axis, 'default', otherAxisSetToFill)
    case 'hug':
      return setPropHugStrategies(axis)
    case 'fixed':
      return setPropFixedStrategies('always', axis, cssNumber(fixedValue, null))
    default:
      assertNever(mode)
  }
}

function pickFixedValue(value: FixedHugFill): CSSNumber | undefined {
  if (value.type === 'fixed') {
    return value.value
  }
  if (value.type === 'fill') {
    return value.value
  }
  return undefined
}

function pickNumberType(value: FixedHugFill | undefined): CSSNumberType {
  if (value?.type === 'fixed') {
    return 'AnyValid'
  }
  if (value?.type === 'fill') {
    return value.value.unit === '%' ? 'Percent' : 'Unitless'
  }
  return 'Unitless'
}

function isNumberInputEnabled(value: FixedHugFill | undefined): boolean {
  return value?.type === 'fixed' || value?.type === 'fill'
}
