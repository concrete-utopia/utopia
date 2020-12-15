/** @jsx jsx */
import { jsx } from '@emotion/react'
import { OptionsType } from 'react-select'
import { FramePin } from 'utopia-api'
import {
  ChainedNumberInput,
  NumberInput,
  PopupList,
  useWrappedEmptyOrUnknownOnSubmitValue,
  useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue,
} from 'uuiui'
import { betterReactMemo, InspectorContextMenuItems } from 'uuiui-deps'
import { isLeft, isRight, left, right } from '../../../../../core/shared/either'
import utils from '../../../../../utils/utils'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { SelectOption } from '../../../controls/select-control'
import { SliderControl } from '../../../controls/slider-control'
import {
  CSSBorderRadius,
  CSSBorderRadiusIndividual,
  CSSNumber,
  cssNumber,
  defaultBorderRadiusIndividual,
  EmptyInputValue,
  fallbackOnEmptyInputValueToCSSEmptyValue,
  framePinToCSSNumber,
  getCSSNumberValue,
} from '../../../common/css-utils'
import { useInspectorLayoutInfo, useInspectorStyleInfo } from '../../../common/property-path-hooks'
import { GridRow } from '../../../widgets/grid-row'

function updateRadiusType(
  newRadiusTypeValue: SelectOption,
  borderRadius: CSSBorderRadius,
): CSSBorderRadius {
  if (newRadiusTypeValue.value === 'individual') {
    if (isLeft(borderRadius)) {
      return right({
        tl: borderRadius.value,
        tr: borderRadius.value,
        br: borderRadius.value,
        bl: borderRadius.value,
      })
    } else {
      return borderRadius
    }
  } else {
    if (isRight(borderRadius)) {
      return left(borderRadius.value.tl)
    } else {
      return borderRadius
    }
  }
}

function updateBorderRadiusAll(
  newBorderRadiusAllValue: number,
  borderRadius: CSSBorderRadius,
): CSSBorderRadius {
  if (isLeft(borderRadius)) {
    return left({
      ...borderRadius.value,
      value: newBorderRadiusAllValue,
    })
  } else {
    return left({
      ...borderRadius.value.tr,
      value: newBorderRadiusAllValue,
    })
  }
}

function updateBorderRadiusAllCSSNumber(newBorderRadiusAllValue: CSSNumber): CSSBorderRadius {
  return left(newBorderRadiusAllValue)
}

function updateBorderRadiusTL(
  newBorderRadiusTLValue: CSSNumber | EmptyInputValue,
  borderRadius: CSSBorderRadius,
): CSSBorderRadius {
  const safeNewValue = fallbackOnEmptyInputValueToCSSEmptyValue(
    cssNumber(0),
    newBorderRadiusTLValue,
  )
  if (isRight(borderRadius)) {
    const newBorderRadius: CSSBorderRadiusIndividual = {
      ...borderRadius.value,
      tl: safeNewValue,
    }
    return right(newBorderRadius)
  } else {
    return right({ ...defaultBorderRadiusIndividual, tl: safeNewValue })
  }
}

function updateBorderRadiusTR(
  newBorderRadiusTRValue: CSSNumber | EmptyInputValue,
  borderRadius: CSSBorderRadius,
): CSSBorderRadius {
  const safeNewValue = fallbackOnEmptyInputValueToCSSEmptyValue(
    cssNumber(0),
    newBorderRadiusTRValue,
  )
  if (isRight(borderRadius)) {
    const newBorderRadius: CSSBorderRadiusIndividual = {
      ...borderRadius.value,
      tr: safeNewValue,
    }
    return right(newBorderRadius)
  } else {
    return right({ ...defaultBorderRadiusIndividual, tr: safeNewValue })
  }
}

function updateBorderRadiusBL(
  newBorderRadiusBLValue: CSSNumber | EmptyInputValue,
  borderRadius: CSSBorderRadius,
): CSSBorderRadius {
  const safeNewValue = fallbackOnEmptyInputValueToCSSEmptyValue(
    cssNumber(0),
    newBorderRadiusBLValue,
  )
  if (isRight(borderRadius)) {
    const newBorderRadius: CSSBorderRadiusIndividual = {
      ...borderRadius.value,
      bl: safeNewValue,
    }
    return right(newBorderRadius)
  } else {
    return right({ ...defaultBorderRadiusIndividual, bl: safeNewValue })
  }
}

function updateBorderRadiusBR(
  newBorderRadiusBRValue: CSSNumber | EmptyInputValue,
  borderRadius: CSSBorderRadius,
): CSSBorderRadius {
  const safeNewValue = fallbackOnEmptyInputValueToCSSEmptyValue(
    cssNumber(0),
    newBorderRadiusBRValue,
  )
  if (isRight(borderRadius)) {
    const newBorderRadius: CSSBorderRadiusIndividual = {
      ...borderRadius.value,
      br: safeNewValue,
    }
    return right(newBorderRadius)
  } else {
    return right({ ...defaultBorderRadiusIndividual, br: safeNewValue })
  }
}

const radiusTypeOptions: OptionsType<SelectOption> = [
  {
    value: 'all',
    label: 'Radius',
  },
  {
    value: 'individual',
    label: 'Corners',
  },
]

function getSliderMax(widthPin: FramePin | undefined, heightPin: FramePin | undefined): number {
  const defaultMax = 100
  const parsedWidth = framePinToCSSNumber(widthPin)
  const parsedHeight = framePinToCSSNumber(heightPin)
  const width = utils.defaultIfNull(defaultMax, getCSSNumberValue(parsedWidth))
  const height = utils.defaultIfNull(defaultMax, getCSSNumberValue(parsedHeight))
  return Math.min(width, height)
}

export const RadiusRow = betterReactMemo('RadiusControls', () => {
  const {
    value: borderRadiusValue,
    controlStatus,
    controlStyles,
    useSubmitValueFactory,
    onUnsetValues,
  } = useInspectorStyleInfo('borderRadius')
  const valueWidth = useInspectorLayoutInfo('Width')
  const valueHeight = useInspectorLayoutInfo('Height')
  const sliderMax = getSliderMax(valueWidth.value, valueHeight.value)

  const [onBorderRadiusTypeSubmitValue] = useSubmitValueFactory(updateRadiusType)
  const [
    onBorderRadiusAllSubmitCSSNumberValue,
    onBorderRadiusAllTransientSubmitCSSNumberValue,
  ] = useSubmitValueFactory(updateBorderRadiusAllCSSNumber)
  const [
    onBorderRadiusAllSubmitValue,
    onBorderRadiusAllTransientSubmitValue,
  ] = useSubmitValueFactory(updateBorderRadiusAll)
  const [
    onBorderRadiusTLSubmitValue,
    onBorderRadiusTLTransientSubmitValue,
  ] = useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
    useSubmitValueFactory(updateBorderRadiusTL),
  )
  const [
    onBorderRadiusTRSubmitValue,
    onBorderRadiusTRTransientSubmitValue,
  ] = useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
    useSubmitValueFactory(updateBorderRadiusTR),
  )
  const [
    onBorderRadiusBLSubmitValue,
    onBorderRadiusBLTransientSubmitValue,
  ] = useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
    useSubmitValueFactory(updateBorderRadiusBL),
  )
  const [
    onBorderRadiusBRSubmitValue,
    onBorderRadiusBRTransientSubmitValue,
  ] = useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
    useSubmitValueFactory(updateBorderRadiusBR),
  )

  const borderRadiusContextMenuItems = InspectorContextMenuItems.optionalAddOnUnsetValues(
    borderRadiusValue != null,
    ['borderRadius'],
    onUnsetValues,
  )

  const radiusTypeValue = isLeft(borderRadiusValue) ? radiusTypeOptions[0] : radiusTypeOptions[1]

  const wrappedOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    onBorderRadiusAllSubmitCSSNumberValue,
    onUnsetValues,
  )
  const wrappedOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    onBorderRadiusAllTransientSubmitCSSNumberValue,
    onUnsetValues,
  )

  return (
    <InspectorContextMenuWrapper
      id='borderRadius-subsection-context-menu'
      items={borderRadiusContextMenuItems}
      data={null}
    >
      <GridRow
        padded={true}
        type='<---1fr--->|------172px-------|'
        style={{ paddingLeft: 0, maxWidth: '100%' }}
      >
        <PopupList
          value={radiusTypeValue}
          options={radiusTypeOptions}
          onSubmitValue={onBorderRadiusTypeSubmitValue}
          containerMode='showBorderOnHover'
          controlStyles={controlStyles}
          style={{
            maxWidth: '100%',
            overflow: 'hidden',
          }}
        />
        {isRight(borderRadiusValue) ? (
          <div>
            <ChainedNumberInput
              idPrefix='borderRadius'
              propsArray={[
                {
                  numberType: 'LengthPercent',
                  value: borderRadiusValue.value.tl,
                  onSubmitValue: onBorderRadiusTLSubmitValue,
                  onTransientSubmitValue: onBorderRadiusTLTransientSubmitValue,
                  controlStatus: controlStatus,
                },
                {
                  numberType: 'LengthPercent',
                  value: borderRadiusValue.value.tr,
                  onSubmitValue: onBorderRadiusTRSubmitValue,
                  onTransientSubmitValue: onBorderRadiusTRTransientSubmitValue,
                  controlStatus: controlStatus,
                },
                {
                  numberType: 'LengthPercent',
                  value: borderRadiusValue.value.bl,
                  onSubmitValue: onBorderRadiusBLSubmitValue,
                  onTransientSubmitValue: onBorderRadiusBLTransientSubmitValue,
                  controlStatus: controlStatus,
                },
                {
                  numberType: 'LengthPercent',
                  value: borderRadiusValue.value.br,
                  onSubmitValue: onBorderRadiusBRSubmitValue,
                  onTransientSubmitValue: onBorderRadiusBRTransientSubmitValue,
                  controlStatus: controlStatus,
                },
              ]}
            />
          </div>
        ) : (
          <GridRow padded={false} type='<--------auto-------->|--45px--|'>
            <SliderControl
              id='radius-all-slider'
              key='radius-all-slider'
              value={borderRadiusValue.value.value}
              DEPRECATED_controlOptions={{
                minimum: 0,
                maximum: sliderMax / 2,
                stepSize: 0.5,
              }}
              onSubmitValue={onBorderRadiusAllSubmitValue}
              onTransientSubmitValue={onBorderRadiusAllTransientSubmitValue}
              controlStatus={controlStatus}
              controlStyles={controlStyles}
            />
            <NumberInput
              value={borderRadiusValue.value}
              id='radius-all-number-input'
              onSubmitValue={wrappedOnSubmitValue}
              onTransientSubmitValue={wrappedOnTransientSubmitValue}
              controlStatus={controlStatus}
              minimum={0}
              numberType='UnitlessPercent'
            />
          </GridRow>
        )}
      </GridRow>
    </InspectorContextMenuWrapper>
  )
})
