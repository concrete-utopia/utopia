import React from 'react'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { removeRow } from '../../../common/context-menu-items'
import type {
  CSSBackgroundLayer,
  CSSBackgroundLayers,
  CSSLinearGradientBackgroundLayer,
  CSSNumber,
  CSSUnknownArrayItem,
  EmptyInputValue,
} from '../../../common/css-utils'
import {
  cssDefault,
  cssNumber,
  fallbackOnEmptyInputValueToCSSDefaultEmptyValue,
} from '../../../common/css-utils'
import { getIndexedSpliceArrayItem } from '../../../common/inspector-utils'
import { stopPropagation } from '../../../common/inspector-utils'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import type { BackgroundLayerProps } from './background-layer-helpers'
import {
  backgroundLayerTypeSelectOptions,
  getIndexedOnCSSBackgroundLayerTypeSelectSubmitValue,
  getIndexedUpdateEnabled,
  linearGradientSelectOption,
} from './background-layer-helpers'
import { BackgroundSolidOrGradientThumbnailControl } from '../../../controls/background-solid-or-gradient-thumbnail-control'
import {
  useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue,
  CheckboxInput,
  FlexRow,
  PopupList,
  NumberInput,
} from '../../../../../uuiui'

export function getIndexedUpdateCSSBackgroundLayerLinearGradientAngle(index: number) {
  return function updateCSSBackgroundLayersLinearGradientAngle(
    newAngle: CSSNumber | EmptyInputValue,
    oldValue: CSSBackgroundLayers,
  ): CSSBackgroundLayers {
    let newBackgroundLayers = [...oldValue]
    let workingBackgroundLayer = newBackgroundLayers[index]
    if (workingBackgroundLayer.type === 'linear-gradient-background-layer') {
      workingBackgroundLayer.angle = fallbackOnEmptyInputValueToCSSDefaultEmptyValue(
        cssDefault(cssNumber(0)),
        newAngle,
      )
      newBackgroundLayers[index] = workingBackgroundLayer
      return newBackgroundLayers
    } else {
      return newBackgroundLayers
    }
  }
}

interface LinearGradientBackgroundLayerProps extends BackgroundLayerProps {
  value: CSSLinearGradientBackgroundLayer
}

export const LinearGradientBackgroundLayer = React.memo<LinearGradientBackgroundLayerProps>(
  (props) => {
    const [gradientCheckboxSubmitValue] = props.useSubmitTransformedValuesFactory(
      getIndexedUpdateEnabled(props.index),
    )
    const onEnabledChange = React.useCallback(
      () => gradientCheckboxSubmitValue(!props.value.enabled),
      [gradientCheckboxSubmitValue, props.value.enabled],
    )
    const [gradientAngleSubmitValue, gradientAngleTransientSubmitValue] =
      useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
        props.useSubmitTransformedValuesFactory(
          getIndexedUpdateCSSBackgroundLayerLinearGradientAngle(props.index),
        ),
      )
    const [backgroundLayerType] = props.useSubmitTransformedValuesFactory(
      getIndexedOnCSSBackgroundLayerTypeSelectSubmitValue(props.index),
    )
    const [onRemoveRowSubmit] = props.useSubmitTransformedValuesFactory(
      getIndexedSpliceArrayItem<CSSBackgroundLayer | CSSUnknownArrayItem>(props.index),
    )

    const enabled = props.value.enabled
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
              onChange={onEnabledChange}
              checked={enabled}
              controlStatus={props.controlStatus}
              onMouseDown={stopPropagation}
            />
            <BackgroundSolidOrGradientThumbnailControl
              id={`background-layer-gradient-${props.index}`}
              key={`background-layer-gradient-${props.index}`}
              testId={`background-layer-gradient-${props.index}`}
              controlStyles={props.controlStyles}
              controlStatus={props.controlStatus}
              modalOffset={{ x: -45, y: 0 }}
              value={props.value}
              backgroundIndex={props.index}
              useSubmitValueFactory={props.useSubmitTransformedValuesFactory}
              popupOpen={props.popupOpen}
              setOpenPopup={props.setOpenPopup}
            />
          </UIGridRow>
          <UIGridRow
            tall
            alignItems='start'
            padded={false}
            variant='<-------1fr------>|----80px----|'
          >
            <FlexRow style={{ alignItems: 'start' }} onMouseDown={stopPropagation}>
              <PopupList
                value={linearGradientSelectOption}
                options={backgroundLayerTypeSelectOptions}
                onSubmitValue={backgroundLayerType}
                controlStyles={props.controlStyles}
                containerMode='default'
              />
            </FlexRow>
            <NumberInput
              id={`background-layer-gradient-angle-${props.index}`}
              testId={`background-layer-gradient-angle-${props.index}`}
              value={props.value.angle.value}
              onSubmitValue={gradientAngleSubmitValue}
              onTransientSubmitValue={gradientAngleTransientSubmitValue}
              controlStatus={props.controlStatus}
              DEPRECATED_labelBelow='angle'
              inputProps={{ onMouseDown: stopPropagation }}
              numberType='AnglePercent'
              defaultUnitToHide={null}
            />
          </UIGridRow>
        </UIGridRow>
      </InspectorContextMenuWrapper>
    )
  },
)
