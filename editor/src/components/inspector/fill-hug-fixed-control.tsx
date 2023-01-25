import React from 'react'
import { getSimpleAttributeAtPath, MetadataUtils } from '../../core/model/element-metadata-utils'
import { stripNulls } from '../../core/shared/array-utils'
import { defaultEither, foldEither, isLeft, right } from '../../core/shared/either'
import { parentPath } from '../../core/shared/element-path'
import { ElementInstanceMetadataMap, isJSXElement } from '../../core/shared/element-template'
import { optionalMap } from '../../core/shared/optional-utils'
import { ElementPath } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import { assertNever, NO_OP } from '../../core/shared/utils'
import { PopupList, SimpleNumberInput } from '../../uuiui'
import { getControlStyles, SelectOption } from '../../uuiui-deps'
import { useDispatch } from '../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import {
  CSSNumber,
  cssNumber,
  CSSNumberType,
  EmptyInputValue,
  parseCSSLengthPercent,
  parseCSSNumber,
} from './common/css-utils'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import {
  Axis,
  detectFlexDirectionOne,
  fillContainerApplicable,
  hugContentsApplicableForContainer,
  hugContentsApplicableForText,
  MaxContent,
  widthHeightFromAxis,
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

export const controlId = (segment: 'width' | 'height'): string => `hug-fixed-fill-${segment}`

type FixedHugFill =
  | { type: 'fixed'; value: CSSNumber }
  | { type: 'hug' }
  | { type: 'fill'; value: CSSNumber }
type FixedHugFillMode = FixedHugFill['type']

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

function selectOption(value: FixedHugFillMode): SelectOption {
  switch (value) {
    case 'fill':
      return {
        value: 'fill',
        label: FillContainerLabel,
      }
    case 'fixed':
      return {
        value: 'fixed',
        label: FixedLabel,
      }
    case 'hug':
      return {
        value: 'hug',
        label: HugContentsLabel,
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
  axis: Axis,
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath | null,
): FixedHugFill | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
    return null
  }

  const flexGrow = foldEither(
    () => null,
    (value) => defaultEither(null, parseCSSNumber(value, 'Unitless')),
    getSimpleAttributeAtPath(right(element.element.value.props), PP.create(['style', 'flexGrow'])),
  )

  if (flexGrow != null) {
    const flexDirection = optionalMap(
      (e) => detectFlexDirectionOne(metadata, parentPath(e)),
      elementPath,
    )

    const isFlexDirectionHorizontal = flexDirection === 'row' || flexDirection === 'row-reverse'
    if (axis === 'horizontal' && isFlexDirectionHorizontal) {
      return { type: 'fill', value: flexGrow }
    }

    const isFlexDirectionVertical = flexDirection === 'column' || flexDirection === 'column-reverse'
    if (axis === 'vertical' && isFlexDirectionVertical) {
      return { type: 'fill', value: flexGrow }
    }
  }

  const property = widthHeightFromAxis(axis)

  const prop = defaultEither(
    null,
    getSimpleAttributeAtPath(right(element.element.value.props), PP.create(['style', property])),
  )

  if (prop === MaxContent) {
    return { type: 'hug' }
  }

  if (prop === '100%') {
    return { type: 'fill', value: cssNumber(100, '%') }
  }

  const parsed = defaultEither(null, parseCSSLengthPercent(prop))
  if (parsed != null) {
    return { type: 'fixed', value: parsed }
  }

  return null
}

function elementComputedDimension(
  prop: 'width' | 'height',
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath | null,
): number | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null) {
    return null
  }

  return element.localFrame?.[prop] ?? null
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
      hugAvailable:
        hugContentsApplicableForText(metadata, selectedView) ||
        hugContentsApplicableForContainer(metadata, selectedView),
      fillAvailable: fillContainerApplicable(selectedView),
    })
  })

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

  const widthComputedValueRef = useRefEditorState(
    (store) =>
      elementComputedDimension(
        'width',
        metadataSelector(store),
        selectedViewsSelector(store).at(0) ?? null,
      ) ?? 0,
  )

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

  const heightComputedValueRef = useRefEditorState(
    (store) =>
      elementComputedDimension(
        'height',
        metadataSelector(store),
        selectedViewsSelector(store).at(0) ?? null,
      ) ?? 0,
  )

  const onSubmitHeight = React.useCallback(
    ({ value: anyValue }: SelectOption) => {
      const value = anyValue as FixedHugFillMode
      const strategy = strategyForMode(heightComputedValueRef.current, 'vertical', value)
      executeFirstApplicableStrategy(
        dispatch,
        metadataRef.current,
        selectedViewsRef.current,
        strategy,
      )
    },
    [dispatch, heightComputedValueRef, metadataRef, selectedViewsRef],
  )

  const onAdjustHeight = React.useCallback(
    (value: number | EmptyInputValue) => {
      if (typeof value !== 'number') {
        return
      }
      if (heightCurrentValue?.type === 'fill') {
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          setPropFillStrategies('vertical', value),
        )
      }
      if (heightCurrentValue?.type === 'fixed') {
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          setPropFixedStrategies('always', 'vertical', cssNumber(value, null)),
        )
      }
    },
    [dispatch, heightCurrentValue?.type, metadataRef, selectedViewsRef],
  )

  const onAdjustWidth = React.useCallback(
    (value: number | EmptyInputValue) => {
      if (typeof value !== 'number') {
        return
      }
      if (widthCurrentValue?.type === 'fill') {
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          setPropFillStrategies('horizontal', value),
        )
      }
      if (widthCurrentValue?.type === 'fixed') {
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          setPropFixedStrategies('always', 'horizontal', cssNumber(value, null)),
        )
      }
    },
    [dispatch, metadataRef, selectedViewsRef, widthCurrentValue?.type],
  )

  const onSubmitWidth = React.useCallback(
    ({ value: anyValue }: SelectOption) => {
      const value = anyValue as FixedHugFillMode
      const strategy = strategyForMode(widthComputedValueRef.current, 'horizontal', value)
      executeFirstApplicableStrategy(
        dispatch,
        metadataRef.current,
        selectedViewsRef.current,
        strategy,
      )
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
        numberType={pickNumberType(widthCurrentValue)}
        incrementControls={true}
        stepSize={1}
        minimum={0}
        maximum={Infinity}
        labelInner={'W'}
        defaultUnitToHide={'%'}
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
  )
})

function strategyForMode(
  fixedValue: number,
  axis: Axis,
  mode: FixedHugFillMode,
): Array<InspectorStrategy> {
  switch (mode) {
    case 'fill':
      return setPropFillStrategies(axis, 'default')
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
    return 'Px'
  }
  if (value?.type === 'fill') {
    return value.value.unit === '%' ? 'Percent' : 'Unitless'
  }
  return 'Unitless'
}

function isNumberInputEnabled(value: FixedHugFill | undefined): boolean {
  return value?.type === 'fixed' || value?.type === 'fill'
}
