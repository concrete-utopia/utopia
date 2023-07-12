/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { createSelector } from 'reselect'
import { optionalMap } from '../../core/shared/optional-utils'
import { intersection } from '../../core/shared/set-utils'
import { assertNever, NO_OP } from '../../core/shared/utils'
import { NumberInput, PopupList } from '../../uuiui'
import type { SelectOption } from '../../uuiui-deps'
import { getControlStyles, InspectorRowHoverCSS } from '../../uuiui-deps'
import { useDispatch } from '../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import type { CSSNumber, CSSNumberType, UnknownOrEmptyInput } from './common/css-utils'
import { cssNumber } from './common/css-utils'
import {
  metadataSelector,
  pathTreesSelector,
  selectedViewsSelector,
  useComputedSizeRef,
} from './inpector-selectors'
import type { Axis, FixedHugFill, FixedHugFillMode } from './inspector-common'
import {
  detectFillHugFixedStateMultiselect,
  getFixedFillHugOptionsForElement,
  isFixedHugFillEqual,
} from './inspector-common'
import {
  setPropFillStrategies,
  setPropFixedStrategies,
  setPropHugStrategies,
} from './inspector-strategies/inspector-strategies'
import type { InspectorStrategy } from './inspector-strategies/inspector-strategy'
import { executeFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'
import type { MetadataSubstate } from '../editor/store/store-hook-substore-types'
import type { ElementPath } from '../../core/shared/project-file-types'
import { treatElementAsGroupLike } from '../canvas/canvas-strategies/strategies/group-helpers'
import * as EP from '../../core/shared/element-path'

export const FillFixedHugControlId = (segment: 'width' | 'height'): string =>
  `hug-fixed-fill-${segment}`

export const FillContainerLabel = 'Fill container' as const
export const FixedLabel = 'Fixed' as const
export const HugContentsLabel = 'Hug contents' as const
export const ComputedLabel = 'Computed' as const
export const DetectedLabel = 'Detected' as const

export function selectOptionLabel(mode: FixedHugFillMode): string {
  switch (mode) {
    case 'fill':
      return FillContainerLabel
    case 'fixed':
      return FixedLabel
    case 'hug':
      return HugContentsLabel
    case 'computed':
      return ComputedLabel
    case 'detected':
      return DetectedLabel
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
  pathTreesSelector,
  selectedViewsSelector,
  (metadata, pathTrees, selectedViews) => {
    const applicableOptions: Array<FixedHugFillMode> = [
      ...intersection(
        selectedViews.map((selectedView) =>
          getFixedFillHugOptionsForElement(metadata, pathTrees, selectedView),
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
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

  const elementOrParentGroupRef = useRefEditorState(anySelectedElementGroupOrChildOfGroup)

  const widthCurrentValue = useEditorState(
    Substores.metadata,
    (store) =>
      detectFillHugFixedStateMultiselect(
        'horizontal',
        metadataSelector(store),
        selectedViewsSelector(store),
      ),
    'FillHugFixedControl widthCurrentValue',
    isFixedHugFillEqual,
  )

  const widthControlStyles = React.useMemo(
    () => getControlStyles(widthCurrentValue.controlStatus),
    [widthCurrentValue],
  )
  const widthInputControlStatus = React.useMemo(
    () =>
      isNumberInputEnabled(widthCurrentValue.fixedHugFill)
        ? widthCurrentValue.controlStatus
        : 'disabled',
    [widthCurrentValue],
  )

  const fillsContainerHorizontallyRef = useRefEditorState(
    (store) =>
      detectFillHugFixedStateMultiselect(
        'horizontal',
        metadataSelector(store),
        selectedViewsSelector(store),
      ).fixedHugFill?.type === 'fill',
  )

  const widthComputedValueRef = useComputedSizeRef('width')

  const heightCurrentValue = useEditorState(
    Substores.metadata,
    (store) =>
      detectFillHugFixedStateMultiselect(
        'vertical',
        metadataSelector(store),
        selectedViewsSelector(store),
      ),
    'FillHugFixedControl heightCurrentValue',
    isFixedHugFillEqual,
  )

  const heightControlStyles = React.useMemo(
    () => getControlStyles(heightCurrentValue.controlStatus),
    [heightCurrentValue],
  )
  const heightInputControlStatus = React.useMemo(
    () =>
      isNumberInputEnabled(heightCurrentValue.fixedHugFill)
        ? heightCurrentValue.controlStatus
        : 'disabled',
    [heightCurrentValue],
  )

  const fillsContainerVerticallyRef = useRefEditorState(
    (store) =>
      detectFillHugFixedStateMultiselect(
        'vertical',
        metadataSelector(store),
        selectedViewsSelector(store),
      ).fixedHugFill?.type === 'fill',
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
        elementPathTreeRef.current,
        allElementPropsRef.current,
        strategy,
      )
    },
    [
      allElementPropsRef,
      dispatch,
      fillsContainerHorizontallyRef,
      heightComputedValueRef,
      metadataRef,
      elementPathTreeRef,
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
      if (elementOrParentGroupRef.current) {
        if (value.unit != null && value.unit !== 'px') {
          // if the element or its parent is a group, we only allow setting the size to Fixed pixels to avoid inconsistent behavior
          return
        }
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
          setPropFixedStrategies('always', 'vertical', value),
        )
        return
      }
      if (heightCurrentValue.fixedHugFill?.type === 'fill') {
        if (value.unit != null && value.unit !== '%') {
          // fill mode only accepts percentage or valueless numbers
          return
        }
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
          setPropFillStrategies('vertical', value.value, false),
        )
      }
      if (
        heightCurrentValue.fixedHugFill?.type === 'fixed' ||
        heightCurrentValue.fixedHugFill?.type === 'computed'
      ) {
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
          setPropFixedStrategies('always', 'vertical', value),
        )
      }
    },
    [
      allElementPropsRef,
      dispatch,
      heightCurrentValue.fixedHugFill?.type,
      metadataRef,
      elementPathTreeRef,
      selectedViewsRef,
      elementOrParentGroupRef,
    ],
  )

  const onAdjustWidth = React.useCallback(
    (value: UnknownOrEmptyInput<CSSNumber>) => {
      if (
        'type' in value &&
        (value.type === 'EMPTY_INPUT_VALUE' || value.type === 'UNKNOWN_INPUT')
      ) {
        return
      }
      if (elementOrParentGroupRef.current) {
        if (value.unit != null && value.unit !== 'px') {
          // if the element or its parent is a group, we only allow setting the size to Fixed pixels to avoid inconsistent behavior
          return
        }
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
          setPropFixedStrategies('always', 'horizontal', value),
        )
        return
      }
      if (widthCurrentValue.fixedHugFill?.type === 'fill') {
        if (value.unit != null && value.unit !== '%') {
          // fill mode only accepts percentage or valueless numbers
          return
        }
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
          setPropFillStrategies('horizontal', value.value, false),
        )
      }
      if (
        widthCurrentValue.fixedHugFill?.type === 'fixed' ||
        widthCurrentValue.fixedHugFill?.type === 'computed'
      ) {
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
          setPropFixedStrategies('always', 'horizontal', value),
        )
      }
    },
    [
      allElementPropsRef,
      dispatch,
      metadataRef,
      selectedViewsRef,
      elementPathTreeRef,
      widthCurrentValue.fixedHugFill?.type,
      elementOrParentGroupRef,
    ],
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
        elementPathTreeRef.current,
        allElementPropsRef.current,
        strategy,
      )
    },
    [
      allElementPropsRef,
      dispatch,
      fillsContainerVerticallyRef,
      metadataRef,
      selectedViewsRef,
      elementPathTreeRef,
      widthComputedValueRef,
    ],
  )

  if (options == null) {
    return null
  }

  const widthValue = optionalMap(pickFixedValue, widthCurrentValue.fixedHugFill) ?? null
  const heightValue = optionalMap(pickFixedValue, heightCurrentValue.fixedHugFill) ?? null

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
          value={optionalMap(selectOption, widthCurrentValue.fixedHugFill?.type) ?? undefined}
          options={options}
          onSubmitValue={onSubmitWidth}
          controlStyles={widthControlStyles}
        />
        <NumberInput
          id={FillFixedHugControlId('width')}
          testId={FillFixedHugControlId('width')}
          value={widthValue}
          onSubmitValue={onAdjustWidth}
          onTransientSubmitValue={onAdjustWidth}
          onForcedSubmitValue={onAdjustWidth}
          controlStatus={widthInputControlStatus}
          numberType={pickNumberType(widthCurrentValue.fixedHugFill)}
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
          value={optionalMap(selectOption, heightCurrentValue.fixedHugFill?.type) ?? undefined}
          options={options}
          onSubmitValue={onSubmitHeight}
          controlStyles={heightControlStyles}
        />
        <NumberInput
          id={FillFixedHugControlId('height')}
          testId={FillFixedHugControlId('height')}
          value={heightValue}
          onSubmitValue={onAdjustHeight}
          onTransientSubmitValue={onAdjustHeight}
          onForcedSubmitValue={onAdjustHeight}
          controlStatus={heightInputControlStatus}
          numberType={pickNumberType(heightCurrentValue.fixedHugFill)}
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
    case 'detected':
    case 'computed':
      return setPropFixedStrategies('always', axis, cssNumber(fixedValue, null))
    default:
      assertNever(mode)
  }
}

function pickFixedValue(value: FixedHugFill): CSSNumber | undefined {
  switch (value.type) {
    case 'computed':
    case 'detected':
    case 'fixed':
    case 'fill':
      return value.value
    case 'hug':
      return undefined
    default:
      assertNever(value)
  }
}

function pickNumberType(value: FixedHugFill | null): CSSNumberType {
  if (value?.type === 'fixed') {
    return 'AnyValid'
  }
  if (value?.type === 'fill') {
    return value.value.unit === '%' ? 'Percent' : 'Unitless'
  }
  return 'Unitless'
}

function isNumberInputEnabled(value: FixedHugFill | null): boolean {
  return value?.type === 'fixed' || value?.type === 'fill'
}

const anySelectedElementGroupOrChildOfGroup = createSelector(
  metadataSelector,
  (store: MetadataSubstate) => store.editor.elementPathTree,
  selectedViewsSelector,
  (metadata, pathTrees, selectedViews): boolean => {
    function elementOrAnyChildGroup(path: ElementPath) {
      return (
        // is the element a Group
        treatElementAsGroupLike(metadata, pathTrees, path) ||
        // or is the parent a group
        treatElementAsGroupLike(metadata, pathTrees, EP.parentPath(path))
      )
    }
    return selectedViews.some(elementOrAnyChildGroup)
  },
)
