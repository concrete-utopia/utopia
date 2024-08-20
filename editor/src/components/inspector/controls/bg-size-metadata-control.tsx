import React from 'react'
import { PopupList, NumberInput, FlexRow } from '../../../uuiui'
import type { SelectOption } from '../../../uuiui-deps'
import type { ControlStatus } from '../common/control-status'
import type {
  CSSBackgroundLayers,
  CSSBGSize,
  CSSBGSizeValue,
  CSSGradientBackgroundLayer,
  CSSKeyword,
  CSSNumber,
  CSSURLFunctionBackgroundLayer,
  UnknownOrEmptyInput,
} from '../common/css-utils'
import {
  cssBGSize,
  cssDefault,
  cssKeyword,
  isCSSBackgroundLayerWithBGSize,
  isCSSKeyword,
  isCSSNumber,
  isCSSValidKeyword,
  isEmptyInputValue,
  isParsedCurlyBrace,
  isUnknownInputValue,
  parsedCurlyBrace,
  cssPixelLength,
} from '../common/css-utils'
import type { UseSubmitTransformedValuesFactory } from '../sections/style-section/background-subsection/background-layer-helpers'
import { MetadataControlsStyle } from '../sections/style-section/background-subsection/background-picker'
import { UIGridRow } from '../widgets/ui-grid-row'

interface BGSizeMetadataControlProps {
  index: number
  value: CSSGradientBackgroundLayer | CSSURLFunctionBackgroundLayer
  useSubmitValueFactory: UseSubmitTransformedValuesFactory
  controlStatus: ControlStatus
}

type BGSizeSelectOptionValue = 'auto' | 'contain' | 'cover' | 'percentage-length'
interface BGSizeSelectOption extends SelectOption {
  value: BGSizeSelectOptionValue
}
const autoSelectOption: BGSizeSelectOption = { value: 'auto', label: 'Auto' }
const containSelectOption: BGSizeSelectOption = { value: 'contain', label: 'Contain' }
const coverSelectOption: BGSizeSelectOption = { value: 'cover', label: 'Cover' }
const percentageLengthSelectOption: BGSizeSelectOption = {
  value: 'percentage-length',
  label: 'Width and Height',
}
const BGSizeKeywordValueSelectOptions = [
  autoSelectOption,
  containSelectOption,
  coverSelectOption,
  percentageLengthSelectOption,
]

function getIndexedUpdateBGSizePopupList(index: number) {
  return function updateBGSizePopupList(
    newValue: BGSizeSelectOption,
    oldValue: CSSBackgroundLayers,
  ): CSSBackgroundLayers {
    let newCSSBackgroundLayers = [...oldValue]
    const indexedLayer = newCSSBackgroundLayers[index]
    if (isCSSBackgroundLayerWithBGSize(indexedLayer)) {
      const bgSize = ((): CSSBGSize => {
        switch (newValue.value) {
          case 'auto': {
            return cssBGSize(cssDefault(parsedCurlyBrace([cssKeyword('auto')]), false))
          }
          case 'contain':
          case 'cover': {
            return cssBGSize(cssDefault(cssKeyword(newValue.value), false))
          }
          case 'percentage-length': {
            return cssBGSize(
              cssDefault(parsedCurlyBrace([cssPixelLength(100), cssPixelLength(100)]), false),
            )
          }
          default: {
            const _exhaustiveCheck: never = newValue.value
            throw new Error(`BGSize type ${JSON.stringify(newValue)} not handled`)
          }
        }
      })()
      newCSSBackgroundLayers[index] = { ...indexedLayer, bgSize }
    }
    return newCSSBackgroundLayers
  }
}

function getIndexedUpdateBGSizeWidthNumberValue(index: number) {
  return function updateBGSizeWidthNumberValue(
    newValue: UnknownOrEmptyInput<CSSNumber | CSSKeyword>,
    oldValue: CSSBackgroundLayers,
  ): CSSBackgroundLayers {
    let newCSSBackgroundLayers = [...oldValue]
    const indexedLayer = newCSSBackgroundLayers[index]
    if (isCSSBackgroundLayerWithBGSize(indexedLayer)) {
      const bgSizeValue = indexedLayer.bgSize.size.value
      if (isParsedCurlyBrace(bgSizeValue)) {
        if (isEmptyInputValue(newValue)) {
          if (isParsedCurlyBrace(bgSizeValue) && bgSizeValue.value[1] != null) {
            const bgSize = cssBGSize(
              cssDefault(parsedCurlyBrace([cssKeyword('auto'), bgSizeValue.value[1]]), false),
            )
            newCSSBackgroundLayers[index] = { ...indexedLayer, bgSize }
          } else {
            const bgSize = cssBGSize(cssDefault(parsedCurlyBrace([cssKeyword('auto')])))
            newCSSBackgroundLayers[index] = { ...indexedLayer, bgSize }
          }
        } else if (isUnknownInputValue(newValue)) {
          // TODO submitting unknown input value triggers error state
        } else if (isCSSValidKeyword(newValue, ['auto'])) {
          const bgSize = cssBGSize(
            cssDefault(parsedCurlyBrace([newValue, bgSizeValue.value[1]]), false),
          )
          newCSSBackgroundLayers[index] = { ...indexedLayer, bgSize }
        } else if (isCSSNumber(newValue)) {
          if (isParsedCurlyBrace(bgSizeValue) && bgSizeValue.value[1] != null) {
            const heightComponent = bgSizeValue.value[1]
            const bgSize = cssBGSize(
              cssDefault(parsedCurlyBrace([newValue, heightComponent]), false),
            )
            newCSSBackgroundLayers[index] = { ...indexedLayer, bgSize }
          } else {
            const bgSize = cssBGSize(
              cssDefault(parsedCurlyBrace([newValue, cssKeyword('auto')]), false),
            )
            newCSSBackgroundLayers[index] = { ...indexedLayer, bgSize }
          }
        }
      }
    }
    return newCSSBackgroundLayers
  }
}

function getIndexedUpdateBGSizeHeightNumberValue(index: number) {
  return function updateBGSizeHeightNumberValue(
    newValue: UnknownOrEmptyInput<CSSNumber | CSSKeyword>,
    oldValue: CSSBackgroundLayers,
  ): CSSBackgroundLayers {
    let newCSSBackgroundLayers = [...oldValue]
    const indexedLayer = newCSSBackgroundLayers[index]
    if (isCSSBackgroundLayerWithBGSize(indexedLayer)) {
      const bgSizeValue = indexedLayer.bgSize.size.value
      if (isParsedCurlyBrace(bgSizeValue)) {
        if (isEmptyInputValue(newValue)) {
          if (
            isParsedCurlyBrace(bgSizeValue) &&
            bgSizeValue.value[0] != null &&
            !isCSSKeyword(bgSizeValue.value[0])
          ) {
            const bgSize = cssBGSize(
              cssDefault(parsedCurlyBrace([bgSizeValue.value[0], cssKeyword('auto')]), false),
            )
            newCSSBackgroundLayers[index] = { ...indexedLayer, bgSize }
          } else {
            const bgSize = cssBGSize(cssDefault(parsedCurlyBrace([cssKeyword('auto')])))
            newCSSBackgroundLayers[index] = { ...indexedLayer, bgSize }
          }
        } else if (isUnknownInputValue(newValue)) {
          // TODO submitting unknown input value triggers error state
        } else if (isCSSValidKeyword(newValue, ['auto']) || isCSSNumber(newValue)) {
          const widthComponent = isParsedCurlyBrace(bgSizeValue)
            ? bgSizeValue.value[0]
            : cssKeyword('auto')
          const bgSize = cssBGSize(cssDefault(parsedCurlyBrace([widthComponent, newValue]), false))
          newCSSBackgroundLayers[index] = { ...indexedLayer, bgSize }
        }
      }
    }
    return newCSSBackgroundLayers
  }
}

function bgSizeValueToSelectOption(value: CSSBGSizeValue): SelectOption {
  if (isCSSKeyword(value)) {
    if (value.value === 'contain') {
      return containSelectOption
    } else {
      return coverSelectOption
    }
  } else {
    const first = value.value[0]
    const second = value.value[1] ?? first

    if (first.value === 'auto' && second.value === 'auto') {
      return autoSelectOption
    } else {
      return percentageLengthSelectOption
    }
  }
}

export const BGSizeMetadataControl: React.FunctionComponent<
  React.PropsWithChildren<BGSizeMetadataControlProps>
> = (props) => {
  const [onSubmitPopupListValue] = props.useSubmitValueFactory(
    getIndexedUpdateBGSizePopupList(props.index),
  )
  const [onSubmitWidthValue, onTransientSubmitWidthValue] = props.useSubmitValueFactory(
    getIndexedUpdateBGSizeWidthNumberValue(props.index),
  )
  const [onSubmitHeightValue, onTransientSubmitHeightValue] = props.useSubmitValueFactory(
    getIndexedUpdateBGSizeHeightNumberValue(props.index),
  )

  const bgSizeValue = props.value.bgSize.size.value
  let widthValue: CSSNumber | null = null
  let heightValue: CSSNumber | null = null
  if (isParsedCurlyBrace(bgSizeValue)) {
    const firstValue = bgSizeValue.value[0]
    const secondValue = bgSizeValue.value[1] ?? firstValue
    if (isCSSNumber(firstValue)) {
      widthValue = firstValue
    }
    if (isCSSNumber(secondValue)) {
      heightValue = secondValue
    }
  }
  const isPercentageLength = bgSizeValueToSelectOption(bgSizeValue).value === 'percentage-length'

  return (
    <UIGridRow padded variant='<--1fr--><--1fr-->' style={{ padding: '0 8px 0 4px' }}>
      <PopupList
        style={{ background: 'transparent', width: 130 }}
        options={BGSizeKeywordValueSelectOptions}
        value={bgSizeValueToSelectOption(bgSizeValue)}
        onSubmitValue={onSubmitPopupListValue}
      />
      {isPercentageLength && widthValue != null && heightValue != null ? (
        <UIGridRow padded={false} variant='<--1fr--><--1fr-->'>
          <NumberInput
            id='bgSize-width-component'
            testId='bgSize-width-component'
            value={widthValue}
            stepSize={1}
            minimum={0}
            numberType='LengthPercent'
            scrubbableInnerLabel='W'
            onSubmitValue={onSubmitWidthValue}
            onTransientSubmitValue={onTransientSubmitWidthValue}
            controlStatus={props.controlStatus}
            defaultUnitToHide={'px'}
            incrementControls={false}
          />
          <NumberInput
            id='bgSize-height-component'
            testId='bgSize-height-component'
            value={heightValue}
            stepSize={1}
            minimum={0}
            numberType='LengthPercent'
            scrubbableInnerLabel='H'
            onSubmitValue={onSubmitHeightValue}
            onTransientSubmitValue={onTransientSubmitHeightValue}
            controlStatus={props.controlStatus}
            defaultUnitToHide={'px'}
            incrementControls={false}
          />
        </UIGridRow>
      ) : null}
    </UIGridRow>
  )
}
