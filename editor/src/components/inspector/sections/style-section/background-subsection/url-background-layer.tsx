import * as React from 'react'
import { CheckboxInput, PopupList } from '../../../../../uuiui'
import { betterReactMemo, InspectorContextMenuWrapper } from '../../../../../uuiui-deps'
import { removeRow } from '../../../common/context-menu-items'
import {
  CSSBackgroundLayer,
  CSSBackgroundLayers,
  CSSUnknownArrayItem,
  CSSURLFunctionBackgroundLayer,
  EmptyInputValue,
  fallbackOnEmptyInputValueToCSSEmptyValue,
  isCSSImageURLBackgroundLayer,
} from '../../../common/css-utils'
import { getIndexedSpliceArrayItem, stopPropagation } from '../../../common/inspector-utils'
import { ImageThumbnailControl } from '../../../controls/image-thumbnail-control'
import { StringControl } from '../../../controls/string-control'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import {
  BackgroundLayerProps,
  backgroundLayerTypeSelectOptions,
  getIndexedOnCSSBackgroundLayerTypeSelectSubmitValue,
  getIndexedUpdateEnabled,
  imageSelectOption,
  UseSubmitTransformedValuesFactory,
} from './background-layer-helpers'

interface URLBackgroundLayerProps extends BackgroundLayerProps {
  value: CSSURLFunctionBackgroundLayer
  useSubmitTransformedValuesFactory: UseSubmitTransformedValuesFactory
}

function getIndexedUpdateNewURL(index: number) {
  return function updateNewURL(
    newValue: string | EmptyInputValue,
    oldValue: CSSBackgroundLayers,
  ): CSSBackgroundLayers {
    const oldLayer = oldValue[index]
    if (oldLayer != null) {
      let newCSSBackgroundLayers = [...oldValue]
      const newURL = fallbackOnEmptyInputValueToCSSEmptyValue('', newValue)
      if (isCSSImageURLBackgroundLayer(oldLayer)) {
        newCSSBackgroundLayers[index] = {
          ...oldLayer,
          url: newURL,
        }
      }
      return newCSSBackgroundLayers
    } else {
      throw new Error(`No background layer exists at index ${index}`)
    }
  }
}

export const URLBackgroundLayer = betterReactMemo<URLBackgroundLayerProps>(
  'URLBackgroundLayer',
  (props) => {
    const [onCheckboxSubmitValue] = props.useSubmitTransformedValuesFactory(
      getIndexedUpdateEnabled(props.index),
    )
    const [onURLSubmitValue] = props.useSubmitTransformedValuesFactory(
      getIndexedUpdateNewURL(props.index),
    )
    const toggleCheckbox = React.useCallback(() => onCheckboxSubmitValue(!props.value.enabled), [
      onCheckboxSubmitValue,
      props,
    ])

    const [backgroundLayerType] = props.useSubmitTransformedValuesFactory(
      getIndexedOnCSSBackgroundLayerTypeSelectSubmitValue(props.index),
    )
    const [onRemoveRowSubmit] = props.useSubmitTransformedValuesFactory(
      getIndexedSpliceArrayItem<CSSBackgroundLayer | CSSUnknownArrayItem>(props.index),
    )

    return (
      <InspectorContextMenuWrapper
        id={`background-layer-subsection-context-menu-row-${props.index}`}
        items={[removeRow(onRemoveRowSubmit), ...props.unsetContextMenuItem]}
        data={null}
      >
        <UIGridRow tall alignItems='start' padded={true} variant='<---1fr--->|------172px-------|'>
          <UIGridRow
            tall
            alignItems='start'
            padded={false}
            variant='<-auto-><----------1fr--------->'
          >
            <CheckboxInput
              onChange={toggleCheckbox}
              checked={props.value.enabled}
              controlStatus={props.controlStatus}
              onMouseDown={stopPropagation}
            />
            <ImageThumbnailControl
              id={`image-thumbnail-control-${props.index}`}
              key={`image-thumbnail-control-${props.index}-${props.value.url}`}
              testId={`image-thumbnail-control-${props.index}-${props.value.url}`}
              value={props.value}
              backgroundIndex={props.index}
              controlStatus={props.controlStatus}
              controlStyles={props.controlStyles}
              setOpenPopup={props.setOpenPopup}
              popupOpen={props.popupOpen}
              useSubmitValueFactory={props.useSubmitTransformedValuesFactory}
              modalOffset={{ x: -45, y: 0 }}
            />
          </UIGridRow>
          <UIGridRow
            tall
            alignItems='start'
            padded={false}
            variant='<-------1fr------>|----80px----|'
          >
            <PopupList
              value={imageSelectOption}
              options={backgroundLayerTypeSelectOptions}
              onSubmitValue={backgroundLayerType}
              controlStyles={props.controlStyles}
              containerMode='default'
            />
            <StringControl
              id={`background-layer-image-${props.index}`}
              key={`background-layer-image-${props.index}`}
              testId={`background-layer-image-${props.index}`}
              value={props.value.url}
              onSubmitValue={onURLSubmitValue}
              controlStatus={props.controlStatus}
              controlStyles={props.controlStyles}
            />
          </UIGridRow>
        </UIGridRow>
      </InspectorContextMenuWrapper>
    )
  },
)
