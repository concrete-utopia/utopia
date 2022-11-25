/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { OptionsType } from 'react-select'
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
import { UIGridRow } from '../../../widgets/ui-grid-row'
import {
  useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue,
  useWrappedEmptyOrUnknownOnSubmitValue,
  PopupList,
  ChainedNumberInput,
  NumberInput,
} from '../../../../../uuiui'
import { InspectorContextMenuItems } from '../../../../../uuiui-deps'
import { SliderNumberControl } from '../../../controls/slider-number-control'

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

function getSliderMax(widthPin: CSSNumber | undefined, heightPin: CSSNumber | undefined): number {
  const defaultMax = 100
  const width = utils.defaultIfNull(defaultMax, getCSSNumberValue(widthPin))
  const height = utils.defaultIfNull(defaultMax, getCSSNumberValue(heightPin))
  return Math.min(width, height)
}

export const RadiusRow = React.memo(() => {
  const {
    value: borderRadiusValue,
    controlStatus,
    controlStyles,
    useSubmitValueFactory,
    onUnsetValues,
  } = useInspectorStyleInfo('borderRadius')
  const valueWidth = useInspectorLayoutInfo('width')
  const valueHeight = useInspectorLayoutInfo('height')
  const sliderMax = getSliderMax(valueWidth.value, valueHeight.value)

  const [onBorderRadiusTypeSubmitValue] = useSubmitValueFactory(updateRadiusType)
  const [onBorderRadiusAllSubmitCSSNumberValue, onBorderRadiusAllTransientSubmitCSSNumberValue] =
    useSubmitValueFactory(updateBorderRadiusAllCSSNumber)
  const [onBorderRadiusAllSubmitValue, onBorderRadiusAllTransientSubmitValue] =
    useSubmitValueFactory(updateBorderRadiusAll)
  const [onBorderRadiusTLSubmitValue, onBorderRadiusTLTransientSubmitValue] =
    useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(useSubmitValueFactory(updateBorderRadiusTL))
  const [onBorderRadiusTRSubmitValue, onBorderRadiusTRTransientSubmitValue] =
    useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(useSubmitValueFactory(updateBorderRadiusTR))
  const [onBorderRadiusBLSubmitValue, onBorderRadiusBLTransientSubmitValue] =
    useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(useSubmitValueFactory(updateBorderRadiusBL))
  const [onBorderRadiusBRSubmitValue, onBorderRadiusBRTransientSubmitValue] =
    useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(useSubmitValueFactory(updateBorderRadiusBR))

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

  const transformBorderRadiusAllNumberToCSSNumber = React.useCallback<
    (newValue: number) => CSSNumber
  >(
    (newValue) => {
      const updatedValue = updateBorderRadiusAll(newValue, borderRadiusValue)
      if (isLeft(updatedValue)) {
        return updatedValue.value
      } else {
        return updatedValue.value.tr
      }
    },
    [borderRadiusValue],
  )

  return (
    <InspectorContextMenuWrapper
      id='borderRadius-subsection-context-menu'
      items={borderRadiusContextMenuItems}
      data={null}
    >
      <UIGridRow
        padded={true}
        variant='<---1fr--->|------172px-------|'
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
                  testId: 'border-radius-tl',
                  defaultUnitToHide: 'px',
                },
                {
                  numberType: 'LengthPercent',
                  value: borderRadiusValue.value.tr,
                  onSubmitValue: onBorderRadiusTRSubmitValue,
                  onTransientSubmitValue: onBorderRadiusTRTransientSubmitValue,
                  controlStatus: controlStatus,
                  testId: 'border-radius-tr',
                  defaultUnitToHide: 'px',
                },
                {
                  numberType: 'LengthPercent',
                  value: borderRadiusValue.value.bl,
                  onSubmitValue: onBorderRadiusBLSubmitValue,
                  onTransientSubmitValue: onBorderRadiusBLTransientSubmitValue,
                  controlStatus: controlStatus,
                  testId: 'border-radius-bl',
                  defaultUnitToHide: 'px',
                },
                {
                  numberType: 'LengthPercent',
                  value: borderRadiusValue.value.br,
                  onSubmitValue: onBorderRadiusBRSubmitValue,
                  onTransientSubmitValue: onBorderRadiusBRTransientSubmitValue,
                  controlStatus: controlStatus,
                  testId: 'border-radius-br',
                  defaultUnitToHide: 'px',
                },
              ]}
            />
          </div>
        ) : (
          <SliderNumberControl
            id='radius-all'
            key='radius-all'
            testId='radius-all'
            value={borderRadiusValue.value}
            DEPRECATED_controlOptions={{
              minimum: 0,
              maximum: sliderMax / 2,
              stepSize: 0.5,
            }}
            controlStatus={controlStatus}
            controlStyles={controlStyles}
            minimum={0}
            numberType='Length'
            defaultUnitToHide={'px'}
            onSubmitValue={wrappedOnSubmitValue}
            onTransientSubmitValue={wrappedOnTransientSubmitValue}
            onSliderSubmitValue={onBorderRadiusAllSubmitValue}
            onSliderTransientSubmitValue={onBorderRadiusAllTransientSubmitValue}
            transformSliderValueToCSSNumber={transformBorderRadiusAllNumberToCSSNumber}
          />
        )}
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})
