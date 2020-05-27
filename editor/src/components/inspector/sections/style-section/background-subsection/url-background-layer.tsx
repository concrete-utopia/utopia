import * as React from 'react'
import { PopupList } from 'uuiui'
import { CheckboxInput } from '../../../../../uuiui'
import { betterReactMemo, NewInspectorContextMenuWrapper } from '../../../../../uuiui-deps'
import { ImageThumbnailControl } from '../../../common/image-thumbnail-control'
import { StringControl } from '../../../controls/string-control'
import { removeRow } from '../../../new-inspector/context-menu-items'
import {
  CSSBackground,
  CSSBackgroundLayers,
  CSSUnknownArrayItem,
  CSSURLFunctionBackgroundLayer,
  EmptyInputValue,
  fallbackOnEmptyInputValueToCSSEmptyValue,
  isCSSImageURLBackgroundLayer,
  CSSBackgroundLayer,
} from '../../../new-inspector/css-utils'
import { getIndexedSpliceArrayItem } from '../../../new-inspector/inspector-utils'
import { stopPropagation } from '../../../utils'
import { GridRow } from '../../../widgets/grid-row'
import {
  BackgroundLayerProps,
  backgroundLayerTypeSelectOptions,
  getIndexedOnCSSBackgroundLayerTypeSelectSubmitValue,
  getIndexedToggleEnabled,
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
      getIndexedToggleEnabled(props.index),
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
      <NewInspectorContextMenuWrapper
        id={`background-layer-subsection-context-menu-row-${props.index}`}
        items={[removeRow(onRemoveRowSubmit), ...props.unsetContextMenuItem]}
        data={null}
      >
        <GridRow tall alignItems='start' padded={true} type='<---1fr--->|------172px-------|'>
          <GridRow tall alignItems='start' padded={false} type='<-auto-><----------1fr--------->'>
            <CheckboxInput
              onChange={toggleCheckbox}
              checked={props.value.enabled}
              controlStatus={props.controlStatus}
              onMouseDown={stopPropagation}
            />
            <ImageThumbnailControl
              id={`image-thumbnail-control-${props.index}`}
              key={`image-thumbnail-control-${props.index}-${props.value.url}`}
              value={props.value}
              backgroundIndex={props.index}
              controlStatus={props.controlStatus}
              controlStyles={props.controlStyles}
              setOpenPopup={props.setOpenPopup}
              popupOpen={props.popupOpen}
              useSubmitValueFactory={props.useSubmitTransformedValuesFactory}
              modalOffset={{ x: -45, y: 0 }}
            />
          </GridRow>
          <GridRow tall alignItems='start' padded={false} type='<-------1fr------>|----80px----|'>
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
              value={props.value.url}
              onSubmitValue={onURLSubmitValue}
              controlStatus={props.controlStatus}
              controlStyles={props.controlStyles}
            />
          </GridRow>
        </GridRow>
      </NewInspectorContextMenuWrapper>
    )
  },
)
