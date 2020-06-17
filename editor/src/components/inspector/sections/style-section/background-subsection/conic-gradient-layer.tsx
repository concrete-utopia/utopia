import * as React from 'react'
import { ChainedNumberInput, CheckboxInput, FlexRow, PopupList } from 'uuiui'
import { betterReactMemo } from 'uuiui-deps'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { removeRow } from '../../../common/context-menu-items'
import {
  CSSUnknownArrayItem,
  CSSConicGradientBackgroundLayer,
  CSSBackgroundLayer,
} from '../../../common/css-utils'
import { getIndexedSpliceArrayItem } from '../../../common/inspector-utils'
import { stopPropagation } from '../../../common/inspector-utils'
import { GridRow } from '../../../widgets/grid-row'
import {
  BackgroundLayerProps,
  backgroundLayerTypeSelectOptions,
  conicGradientSelectOption,
  getIndexedOnCSSBackgroundLayerTypeSelectSubmitValue,
  getIndexedToggleEnabled,
  getIndexedUpdateRadialOrConicGradientCenterX,
  getIndexedUpdateRadialOrConicGradientCenterY,
} from './background-layer-helpers'
import { BackgroundSolidOrGradientThumbnailControl } from '../../../controls/background-solid-or-gradient-thumbnail-control'

interface ConicGradientBackgroundLayerProps extends BackgroundLayerProps {
  value: CSSConicGradientBackgroundLayer
}

export const ConicGradientBackgroundLayer = betterReactMemo<ConicGradientBackgroundLayerProps>(
  'ConicGradientBackgroundLayer',
  (props) => {
    const [gradientCheckboxSubmitValue] = props.useSubmitTransformedValuesFactory(
      getIndexedToggleEnabled(props.index),
    )
    const onEnabledChange = React.useCallback(
      () => gradientCheckboxSubmitValue(!props.value.enabled),
      [gradientCheckboxSubmitValue, props.value.enabled],
    )
    const [
      gradientCenterXSubmitValue,
      gradientCenterXTransientSubmitValue,
    ] = props.useSubmitTransformedValuesFactory(
      getIndexedUpdateRadialOrConicGradientCenterX(props.index),
    )
    const [
      gradientCenterYSubmitValue,
      gradientCenterYTransientSubmitValue,
    ] = props.useSubmitTransformedValuesFactory(
      getIndexedUpdateRadialOrConicGradientCenterY(props.index),
    )
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
        <GridRow tall alignItems='start' padded={true} type='<---1fr--->|------172px-------|'>
          <GridRow tall alignItems='start' padded={false} type='<-auto-><----------1fr--------->'>
            <CheckboxInput
              onChange={onEnabledChange}
              checked={props.value.enabled}
              controlStatus={props.controlStatus}
              onMouseDown={stopPropagation}
            />
            <BackgroundSolidOrGradientThumbnailControl
              id={`background-layer-gradient-${props.index}`}
              key={`background-layer-gradient-${props.index}`}
              controlStyles={props.controlStyles}
              controlStatus={props.controlStatus}
              modalOffset={{ x: -45, y: 0 }}
              value={props.value}
              backgroundIndex={props.index}
              useSubmitValueFactory={props.useSubmitTransformedValuesFactory}
              popupOpen={props.popupOpen}
              setOpenPopup={props.setOpenPopup}
            />
          </GridRow>
          <GridRow tall alignItems='start' padded={false} type='<-------1fr------>|----80px----|'>
            <FlexRow style={{ alignItems: 'start' }} onMouseDown={stopPropagation}>
              <PopupList
                value={{
                  value: conicGradientSelectOption.value,
                  label: conicGradientSelectOption.label,
                }}
                options={backgroundLayerTypeSelectOptions}
                onSubmitValue={backgroundLayerType}
                controlStyles={props.controlStyles}
                containerMode='default'
              />
            </FlexRow>
            <ChainedNumberInput
              idPrefix='background-layer-gradient-center'
              propsArray={[
                {
                  value: props.value.center.x.value,
                  DEPRECATED_labelBelow: 'x',
                  onSubmitValue: gradientCenterXSubmitValue,
                  onTransientSubmitValue: gradientCenterXTransientSubmitValue,
                  controlStatus: props.controlStatus,
                  numberType: 'LengthPercent' as const,
                },
                {
                  value: props.value.center.y.value,
                  DEPRECATED_labelBelow: 'y',
                  onSubmitValue: gradientCenterYSubmitValue,
                  onTransientSubmitValue: gradientCenterYTransientSubmitValue,
                  controlStatus: props.controlStatus,
                  numberType: 'LengthPercent' as const,
                },
              ]}
            />
          </GridRow>
        </GridRow>
      </InspectorContextMenuWrapper>
    )
  },
)
