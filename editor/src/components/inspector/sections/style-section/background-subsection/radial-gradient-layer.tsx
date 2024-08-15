import React from 'react'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { removeRow } from '../../../common/context-menu-items'
import type {
  CSSUnknownArrayItem,
  CSSBackgroundLayer,
  CSSRadialGradientBackgroundLayer,
} from '../../../common/css-utils'
import { getIndexedSpliceArrayItem } from '../../../common/inspector-utils'
import { stopPropagation } from '../../../common/inspector-utils'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import type { BackgroundLayerProps } from './background-layer-helpers'
import {
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

interface RadialGradientBackgroundLayerProps extends BackgroundLayerProps {
  value: CSSRadialGradientBackgroundLayer
}

export const RadialGradientBackgroundLayer = React.memo<RadialGradientBackgroundLayerProps>(
  (props) => {
    const [gradientCheckboxSubmitValue] = props.useSubmitTransformedValuesFactory(
      getIndexedUpdateEnabled(props.index),
    )
    const onEnabledChange = React.useCallback(
      () => gradientCheckboxSubmitValue(!props.value.enabled),
      [gradientCheckboxSubmitValue, props.value.enabled],
    )
    const [gradientCenterXSubmitValue, gradientCenterXTransientSubmitValue] =
      useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
        props.useSubmitTransformedValuesFactory(
          getIndexedUpdateRadialOrConicGradientCenterX(props.index),
        ),
      )
    const [gradientCenterYSubmitValue, gradientCenterYTransientSubmitValue] =
      useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
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
        <UIGridRow
          tall
          alignItems='start'
          padded={true}
          variant='<-auto-><-auto->|70px|<----1fr---->|'
        >
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
              style={{ background: 'transparent' }}
            />
          </FlexRow>
          <ChainedNumberInput
            idPrefix='background-layer-gradient-center'
            propsArray={[
              {
                value: props.value.center.x.value,
                scrubbable_innerlabel: 'x',
                onSubmitValue: gradientCenterXSubmitValue,
                onTransientSubmitValue: gradientCenterXTransientSubmitValue,
                controlStatus: props.controlStatus,
                numberType: 'LengthPercent' as const,
                testId: 'background-layer-gradient-center-x',
                defaultUnitToHide: null,
              },
              {
                value: props.value.center.y.value,
                scrubbable_innerlabel: 'y',
                onSubmitValue: gradientCenterYSubmitValue,
                onTransientSubmitValue: gradientCenterYTransientSubmitValue,
                controlStatus: props.controlStatus,
                numberType: 'LengthPercent' as const,
                testId: 'background-layer-gradient-center-y',
                defaultUnitToHide: null,
              },
            ]}
          />
        </UIGridRow>
      </InspectorContextMenuWrapper>
    )
  },
)
