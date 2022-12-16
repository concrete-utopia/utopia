import React from 'react'
import { getLayoutProperty } from '../../core/layout/getLayoutProperty'
import { getSimpleAttributeAtPath, MetadataUtils } from '../../core/model/element-metadata-utils'
import { stripNulls } from '../../core/shared/array-utils'
import { defaultEither, isLeft, right } from '../../core/shared/either'
import { ElementInstanceMetadataMap, isJSXElement } from '../../core/shared/element-template'
import { optionalMap } from '../../core/shared/optional-utils'
import { ElementPath } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import { assertNever, NO_OP } from '../../core/shared/utils'
import { PopupList, SimpleNumberInput } from '../../uuiui'
import { getControlStyles, SelectOption } from '../../uuiui-deps'
import { useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { CSSNumber, cssNumber, EmptyInputValue, parseCSSLengthPercent } from './common/css-utils'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import { fillContainerApplicable, hugContentsApplicable } from './inspector-common'
import {
  InspectorStrategy,
  runStrategies,
  setPropFillStrategies,
  setPropFixedStrategies,
  setPropHugStrategies,
} from './inspector-strategies'

const controlId = (segment: 'width' | 'height') => `hug-fixed-fill-${segment}`

type FixedHugFill = { type: 'fixed'; amount: CSSNumber } | { type: 'hug' } | { type: 'fill' }
type FixedHugFillMode = FixedHugFill['type']

function isFixedHugFillEqual(a: FixedHugFill | undefined, b: FixedHugFill | undefined): boolean {
  if (a === undefined && b === undefined) {
    return true
  }

  if (a?.type !== b?.type) {
    return false
  }

  if (a?.type === 'fixed' && b?.type === 'fixed') {
    return a.amount.value === b.amount.value && a.amount.unit === b.amount.unit
  }

  return true
}

function selectOption(value: FixedHugFillMode): SelectOption {
  switch (value) {
    case 'fill':
      return {
        value: 'fill',
        label: 'Fill container',
      }
    case 'fixed':
      return {
        value: 'fixed',
        label: 'Fixed',
      }
    case 'hug':
      return {
        value: 'hug',
        label: 'Hug contents',
      }
    default:
      assertNever(value)
  }
}

const FillHugFixedControlOptions = ({
  hugAvailable,
  fillAvailable,
}: {
  hugAvailable: boolean
  fillAvailable: boolean
}): Array<SelectOption> =>
  stripNulls([
    selectOption('fixed'),
    hugAvailable ? selectOption('hug') : null,
    fillAvailable ? selectOption('fill') : null,
  ])

function detectFillHugFixedState(
  property: 'width' | 'height',
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath | null,
): FixedHugFill | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
    return null
  }

  const prop = defaultEither(
    null,
    getSimpleAttributeAtPath(right(element.element.value.props), PP.create(['style', property])),
  )

  if (prop === 'min-content') {
    return { type: 'hug' }
  }

  if (prop === '100%') {
    return { type: 'fill' }
  }

  const parsed = defaultEither(null, parseCSSLengthPercent(prop))
  if (parsed != null) {
    return { type: 'fixed', amount: parsed }
  }

  return null
}

function elementComputedDimension(
  prop: 'width' | 'height',
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath | null,
): CSSNumber | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null) {
    return null
  }

  return defaultEither(null, parseCSSLengthPercent(element.computedStyle?.[prop]))
}

interface FillHugFixedControlProps {}

export const FillHugFixedControl = React.memo<FillHugFixedControlProps>((props) => {
  const optionsRef = useRefEditorState((store) => {
    const selectedView = selectedViewsSelector(store).at(0)
    if (selectedView == null) {
      return null
    }
    const metadata = metadataSelector(store)
    return FillHugFixedControlOptions({
      hugAvailable: hugContentsApplicable(metadata, selectedView),
      fillAvailable: fillContainerApplicable(selectedView),
    })
  })

  const dispatch = useEditorState((store) => store.dispatch, 'FillHugFixedControl dispatch')
  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)

  const widthCurrentValue = useEditorState(
    (store) =>
      detectFillHugFixedState(
        'width',
        metadataSelector(store),
        selectedViewsSelector(store).at(0) ?? null,
      ) ?? undefined,
    'FillHugFixedControl widthCurrentValue',
    isFixedHugFillEqual,
  )

  const widthComputedValueRef = useRefEditorState(
    (store) =>
      elementComputedDimension(
        'width',
        metadataSelector(store),
        selectedViewsSelector(store).at(0) ?? null,
      ) ?? cssNumber(0, null),
  )

  const heightCurrentValue = useEditorState(
    (store) =>
      detectFillHugFixedState(
        'height',
        metadataSelector(store),
        selectedViewsSelector(store).at(0) ?? null,
      ) ?? undefined,
    'FillHugFixedControl heightCurrentValue',
    isFixedHugFillEqual,
  )

  const heightComputedValueRef = useRefEditorState(
    (store) =>
      elementComputedDimension(
        'height',
        metadataSelector(store),
        selectedViewsSelector(store).at(0) ?? null,
      ) ?? cssNumber(0, null),
  )

  const onSubmitHeight = React.useCallback(
    ({ value: anyValue }: SelectOption) => {
      const value = anyValue as FixedHugFillMode
      const strategy = strategyForMode(heightComputedValueRef.current, 'height', value)
      runStrategies(dispatch, metadataRef.current, selectedViewsRef.current, strategy)
    },
    [dispatch, heightComputedValueRef, metadataRef, selectedViewsRef],
  )

  const onAdjustHeight = React.useCallback(
    (value: number | EmptyInputValue) => {
      if (typeof value === 'number') {
        runStrategies(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          setPropFixedStrategies(
            'height',
            cssNumber(value, heightComputedValueRef.current?.unit ?? null),
          ),
        )
      }
    },
    [dispatch, heightComputedValueRef, metadataRef, selectedViewsRef],
  )

  const onAdjustWidth = React.useCallback(
    (value: number | EmptyInputValue) => {
      if (typeof value === 'number') {
        runStrategies(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          setPropFixedStrategies(
            'width',
            cssNumber(value, widthComputedValueRef.current?.unit ?? null),
          ),
        )
      }
    },
    [dispatch, metadataRef, selectedViewsRef, widthComputedValueRef],
  )

  const onSubmitWidth = React.useCallback(
    ({ value: anyValue }: SelectOption) => {
      const value = anyValue as FixedHugFillMode
      const strategy = strategyForMode(widthComputedValueRef.current, 'width', value)
      runStrategies(dispatch, metadataRef.current, selectedViewsRef.current, strategy)
    },
    [dispatch, metadataRef, selectedViewsRef, widthComputedValueRef],
  )

  const controlStylesRef = React.useRef(getControlStyles('simple'))

  if (optionsRef.current == null) {
    return null
  }

  const widthValue = optionalMap(pickFixedValue, widthCurrentValue) ?? undefined
  const heightValue = optionalMap(pickFixedValue, heightCurrentValue) ?? undefined

  return (
    <div
      style={{
        display: 'grid',
        gridTemplateRows: '1fr 1fr',
        gridTemplateColumns: '1fr 1fr',
        gap: 4,
        padding: 4,
      }}
    >
      <PopupList
        value={optionalMap(selectOption, widthCurrentValue?.type) ?? undefined}
        options={optionsRef.current}
        onSubmitValue={onSubmitWidth}
        controlStyles={controlStylesRef.current}
        containerMode='showBorderOnHover'
      />
      <SimpleNumberInput
        id={controlId('width')}
        testId={controlId('width')}
        value={widthValue?.value}
        onSubmitValue={onAdjustWidth}
        onTransientSubmitValue={NO_OP}
        onForcedSubmitValue={NO_OP}
        controlStatus={isNumberInputEnabled(widthCurrentValue) ? undefined : 'disabled'}
        incrementControls={true}
        stepSize={1}
        minimum={0}
        maximum={Infinity}
        labelInner={'W'}
        defaultUnitToHide={'px'}
        focusOnMount={false}
      />
      <PopupList
        value={optionalMap(selectOption, heightCurrentValue?.type) ?? undefined}
        options={optionsRef.current}
        onSubmitValue={onSubmitHeight}
        controlStyles={controlStylesRef.current}
        containerMode='showBorderOnHover'
      />
      <SimpleNumberInput
        id={controlId('height')}
        testId={controlId('height')}
        value={heightValue?.value}
        onSubmitValue={onAdjustHeight}
        onTransientSubmitValue={NO_OP}
        onForcedSubmitValue={NO_OP}
        controlStatus={isNumberInputEnabled(heightCurrentValue) ? undefined : 'disabled'}
        incrementControls={true}
        stepSize={1}
        minimum={0}
        maximum={Infinity}
        labelInner={'H'}
        defaultUnitToHide={null}
        focusOnMount={false}
      />
    </div>
  )
})

function strategyForMode(
  fixedValue: CSSNumber,
  prop: 'width' | 'height',
  mode: FixedHugFillMode,
): Array<InspectorStrategy> {
  switch (mode) {
    case 'fill':
      return setPropFillStrategies(prop)
    case 'hug':
      return setPropHugStrategies(prop)
    case 'fixed':
      return setPropFixedStrategies(prop, fixedValue)
    default:
      assertNever(mode)
  }
}

function pickFixedValue(value: FixedHugFill): CSSNumber | undefined {
  if (value.type === 'fixed') {
    return value.amount
  }
  return undefined
}

function isNumberInputEnabled(value: FixedHugFill | undefined): boolean {
  return value?.type === 'fixed'
}
