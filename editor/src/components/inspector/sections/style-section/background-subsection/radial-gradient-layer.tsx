import * as React from 'react'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { removeRow } from '../../../common/context-menu-items'
import {
  CSSUnknownArrayItem,
  CSSBackgroundLayer,
  CSSRadialGradientBackgroundLayer,
} from '../../../common/css-utils'
import { getIndexedSpliceArrayItem } from '../../../common/inspector-utils'
import { stopPropagation } from '../../../common/inspector-utils'
import { GridRow } from '../../../widgets/grid-row'
import {
  BackgroundLayerProps,
  backgroundLayerTypeSelectOptions,
  getIndexedOnCSSBackgroundLayerTypeSelectSubmitValue,
  getIndexedUpdateEnabled,
  getIndexedUpdateRadialOrConicGradientCenterX,
  getIndexedUpdateRadialOrConicGradientCenterY,
  radialGradientSelectOption,
} from './background-layer-helpers'
import { BackgroundSolidOrGradientThumbnailControl } from '../../../controls/background-solid-or-gradient-thumbnail-control'
import {
  useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue,
  CheckboxInput,
  FlexRow,
  PopupList,
  ChainedNumberInput,
} from '../../../../../uuiui'
import { betterReactMemo } from '../../../../../uuiui-deps'

interface RadialGradientBackgroundLayerProps extends BackgroundLayerProps {
  value: CSSRadialGradientBackgroundLayer
}

export const RadialGradientBackgroundLayer = betterReactMemo<RadialGradientBackgroundLayerProps>(
  'RadialGradientBackgroundLayer',
  (props) => {
    const [gradientCheckboxSubmitValue] = props.useSubmitTransformedValuesFactory(
      getIndexedUpdateEnabled(props.index),
    )
    const onEnabledChange = React.useCallback(
      () => gradientCheckboxSubmitValue(!props.value.enabled),
      [gradientCheckboxSubmitValue, props.value.enabled],
    )
    const [
      gradientCenterXSubmitValue,
      gradientCenterXTransientSubmitValue,
    ] = useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
      props.useSubmitTransformedValuesFactory(
        getIndexedUpdateRadialOrConicGradientCenterX(props.index),
      ),
    )
    const [
      gradientCenterYSubmitValue,
      gradientCenterYTransientSubmitValue,
    ] = useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
      props.useSubmitTransformedValuesFactory(
        getIndexedUpdateRadialOrConicGradientCenterY(props.index),
      ),
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
          </GridRow>
          <GridRow tall alignItems='start' padded={false} type='<-------1fr------>|----80px----|'>
            <FlexRow style={{ alignItems: 'start' }} onMouseDown={stopPropagation}>
              <PopupList
                value={{
                  value: radialGradientSelectOption.value,
                  label: radialGradientSelectOption.label,
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
                  testId: 'background-layer-gradient-center-x',
                  defaultUnitToHide: null,
                },
                {
                  value: props.value.center.y.value,
                  DEPRECATED_labelBelow: 'y',
                  onSubmitValue: gradientCenterYSubmitValue,
                  onTransientSubmitValue: gradientCenterYTransientSubmitValue,
                  controlStatus: props.controlStatus,
                  numberType: 'LengthPercent' as const,
                  testId: 'background-layer-gradient-center-y',
                  defaultUnitToHide: null,
                },
              ]}
            />
          </GridRow>
        </GridRow>
      </InspectorContextMenuWrapper>
    )
  },
)
