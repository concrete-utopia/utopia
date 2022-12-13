import React from 'react'
import { getLayoutProperty } from '../../core/layout/getLayoutProperty'
import { getSimpleAttributeAtPath, MetadataUtils } from '../../core/model/element-metadata-utils'
import { stripNulls } from '../../core/shared/array-utils'
import { defaultEither, foldEither, isLeft, isRight, right } from '../../core/shared/either'
import { ElementInstanceMetadataMap, isJSXElement } from '../../core/shared/element-template'
import { optionalMap } from '../../core/shared/optional-utils'
import { ElementPath } from '../../core/shared/project-file-types'
import { create } from '../../core/shared/property-path'
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
        label: 'Fixed width',
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
    getSimpleAttributeAtPath(right(element.element.value.props), create(['style', property])),
  )

  if (prop == null || prop === 'min-content') {
    return { type: 'hug' }
  }

  if (prop === '100%') {
    return { type: 'fill' }
  }

  const parsed = parseCSSLengthPercent(prop)
  if (isRight(parsed)) {
    return { type: 'fixed', amount: parsed.value }
  }

  return null
}

interface FillHugFixedControlProps {}

export const FillHugFixedControl = React.memo<FillHugFixedControlProps>((props) => {
  // TODO: come up with better memo
  const options = useEditorState((store) => {
    const selectedView = selectedViewsSelector(store).at(0)
    if (selectedView == null) {
      return null
    }
    const metadata = metadataSelector(store)
    return FillHugFixedControlOptions({
      hugAvailable: hugContentsApplicable(metadata, selectedView),
      fillAvailable: fillContainerApplicable(selectedView),
    })
  }, 'FillHugFixedControl options')

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
    'FillHugFixedControl currentValue',
  )

  const heightCurrentValue = useEditorState(
    (store) =>
      detectFillHugFixedState(
        'height',
        metadataSelector(store),
        selectedViewsSelector(store).at(0) ?? null,
      ) ?? undefined,
    'FillHugFixedControl currentValue',
  )

  const onSubmitHeight = React.useCallback(
    ({ value: anyValue }: SelectOption) => {
      const value = anyValue as FixedHugFillMode
      const strategy = strategyForMode(cssNumber(420, 'px'), 'height', value)
      runStrategies(dispatch, metadataRef.current, selectedViewsRef.current, strategy)
    },
    [dispatch, metadataRef, selectedViewsRef],
  )

  const onAdjustHeight = React.useCallback(
    (value: number | EmptyInputValue) => {
      if (typeof value === 'number') {
        runStrategies(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          setPropFixedStrategies('height', cssNumber(value, 'px')),
        )
      }
    },
    [dispatch, metadataRef, selectedViewsRef],
  )

  const onAdjustWidth = React.useCallback(
    (value: number | EmptyInputValue) => {
      if (typeof value === 'number') {
        runStrategies(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          setPropFixedStrategies('width', cssNumber(value, 'px')),
        )
      }
    },
    [dispatch, metadataRef, selectedViewsRef],
  )

  const onSubmitWidth = React.useCallback(
    ({ value: anyValue }: SelectOption) => {
      const value = anyValue as FixedHugFillMode
      const strategy = strategyForMode(cssNumber(420, 'px'), 'width', value)
      runStrategies(dispatch, metadataRef.current, selectedViewsRef.current, strategy)
    },
    [dispatch, metadataRef, selectedViewsRef],
  )

  const controlStylesRef = React.useRef(getControlStyles('simple'))

  if (options == null) {
    return null
  }

  const widthValue = optionalMap(pickAmount, widthCurrentValue) ?? undefined
  const heightValue = optionalMap(pickAmount, heightCurrentValue) ?? undefined

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
        options={options}
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
        options={options}
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

function pickAmount(value: FixedHugFill): CSSNumber | undefined {
  if (value.type === 'fixed') {
    return value.amount
  }
  return undefined
}

function isNumberInputEnabled(value: FixedHugFill | undefined): boolean {
  return value?.type === 'fixed'
}
