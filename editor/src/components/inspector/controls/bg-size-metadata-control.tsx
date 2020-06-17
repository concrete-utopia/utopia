import * as React from 'react'
import { NumberInput, PopupList } from '../../../uuiui'
import { SelectOption } from '../../../uuiui-deps'
import {
  CSSBackgroundLayers,
  cssBGSize,
  CSSBGSize,
  CSSBGSizeValue,
  cssDefault,
  CSSGradientBackgroundLayer,
  CSSKeyword,
  cssKeyword,
  CSSNumber,
  cssPixelLengthZero,
  CSSURLFunctionBackgroundLayer,
  EmptyInputValue,
  isCSSBackgroundLayerWithBGSize,
  isCSSKeyword,
  isCSSNumber,
  isEmptyInputValue,
  isParsedCurlyBrace,
  parsedCurlyBrace,
} from '../common/css-utils'
import { ControlStatus } from '../common/control-status'
import { UseSubmitTransformedValuesFactory } from '../sections/style-section/background-subsection/background-layer-helpers'
import { MetadataControlsStyle } from '../sections/style-section/background-subsection/background-picker'

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
            return cssBGSize(cssDefault(parsedCurlyBrace([cssKeyword('auto')])))
          }
          case 'contain':
          case 'cover': {
            return cssBGSize(cssDefault(cssKeyword(newValue.value), false))
          }
          case 'percentage-length': {
            return cssBGSize(
              cssDefault(
                parsedCurlyBrace([{ ...cssPixelLengthZero }, { ...cssPixelLengthZero }]),
                false,
              ),
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
    newValue: CSSNumber | EmptyInputValue,
    oldValue: CSSBackgroundLayers,
  ): CSSBackgroundLayers {
    let newCSSBackgroundLayers = [...oldValue]
    const indexedLayer = newCSSBackgroundLayers[index]
    if (isCSSBackgroundLayerWithBGSize(indexedLayer)) {
      const bgSizeValue = indexedLayer.bgSize.size.value
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
      } else {
        const heightComponent = isParsedCurlyBrace(bgSizeValue)
          ? bgSizeValue.value[1]
          : cssKeyword('auto')
        const bgSize = cssBGSize(cssDefault(parsedCurlyBrace([newValue, heightComponent]), false))
        newCSSBackgroundLayers[index] = { ...indexedLayer, bgSize }
      }
    }
    return newCSSBackgroundLayers
  }
}

function getIndexedUpdateBGSizeHeightNumberValue(index: number) {
  return function updateBGSizeHeightNumberValue(
    newValue: CSSNumber | EmptyInputValue,
    oldValue: CSSBackgroundLayers,
  ): CSSBackgroundLayers {
    let newCSSBackgroundLayers = [...oldValue]
    const indexedLayer = newCSSBackgroundLayers[index]
    if (isCSSBackgroundLayerWithBGSize(indexedLayer)) {
      const bgSizeValue = indexedLayer.bgSize.size.value
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
      } else {
        const widthComponent = isParsedCurlyBrace(bgSizeValue)
          ? bgSizeValue.value[0]
          : cssKeyword('auto')
        const bgSize = cssBGSize(cssDefault(parsedCurlyBrace([widthComponent, newValue]), false))
        newCSSBackgroundLayers[index] = { ...indexedLayer, bgSize }
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
    // When one component is defined, it defines both the x and y components
    const second = value.value[1] ?? first

    if (first.value === 'auto' && second.value === 'auto') {
      return autoSelectOption
    } else {
      return percentageLengthSelectOption
    }
  }
}

export const BGSizeMetadataControl: React.FunctionComponent<BGSizeMetadataControlProps> = (
  props,
) => {
  const [onSubmitPopupListValue] = props.useSubmitValueFactory(
    getIndexedUpdateBGSizePopupList(props.index),
  )
  const [onSubmitWidthValue, onTransientSubmitWidthValue] = props.useSubmitValueFactory(
    getIndexedUpdateBGSizeWidthNumberValue(props.index),
  )
  const [onSubmitHeightValue, onTransientSubmitHeightValue] = props.useSubmitValueFactory(
    getIndexedUpdateBGSizeHeightNumberValue(props.index),
  )

  if (isCSSBackgroundLayerWithBGSize(props.value)) {
    const bgSizeValue = props.value.bgSize.size.value
    let widthValue: CSSNumber | CSSKeyword | null = null
    let heightValue: CSSNumber | CSSKeyword | null = null
    if (isParsedCurlyBrace(bgSizeValue)) {
      widthValue = bgSizeValue.value[0]
      heightValue = bgSizeValue.value[1] ?? widthValue
    }

    return (
      <div style={MetadataControlsStyle}>
        <PopupList
          style={{ gridColumn: '1 / span 3' }}
          options={BGSizeKeywordValueSelectOptions}
          value={bgSizeValueToSelectOption(bgSizeValue)}
          onSubmitValue={onSubmitPopupListValue}
        />
        {isParsedCurlyBrace(bgSizeValue) ? (
          <>
            {isCSSNumber(widthValue) ? (
              <NumberInput
                style={{ gridColumn: '5 / span 1' }}
                numberType='LengthPercent'
                id='bgSize-width-component'
                value={widthValue}
                onSubmitValue={onSubmitWidthValue}
                onTransientSubmitValue={onTransientSubmitWidthValue}
                controlStatus={props.controlStatus}
                labelBelow='width'
              />
            ) : (
              <div style={{ gridColumn: '5 / span 1' }}>auto</div>
            )}
            {isCSSNumber(heightValue) ? (
              <NumberInput
                style={{ gridColumn: '7 / span 1' }}
                numberType='LengthPercent'
                id='bgSize-height-component'
                value={heightValue}
                onSubmitValue={onSubmitHeightValue}
                onTransientSubmitValue={onTransientSubmitHeightValue}
                controlStatus={props.controlStatus}
                labelBelow='height'
              />
            ) : (
              <div style={{ gridColumn: '7 / span 1' }}>auto</div>
            )}
          </>
        ) : null}
      </div>
    )
  } else {
    return null
  }
}
