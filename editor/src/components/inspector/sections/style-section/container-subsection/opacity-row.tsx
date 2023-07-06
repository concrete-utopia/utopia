import React from 'react'
import * as PP from '../../../../../core/shared/property-path'
import { PropertyLabel } from '../../../widgets/property-label'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { useInspectorStyleInfo, useIsSubSectionVisible } from '../../../common/property-path-hooks'
import { useWrappedEmptyOrUnknownOnSubmitValue, NumberInput } from '../../../../../uuiui'
import {
  CSSUtils,
  InspectorContextMenuItems,
  InspectorContextMenuWrapper,
  SliderControl,
} from '../../../../../uuiui-deps'
import { SliderNumberControl } from '../../../controls/slider-number-control'
import type { CSSNumber } from '../../../common/css-utils'
import { setCSSNumberValue } from '../../../common/css-utils'

const sliderControlOptions = {
  minimum: 0,
  maximum: 1,
  stepSize: 0.01,
  origin: 1,
  filled: true,
}

// TODO: path should match target
const opacityProp = [PP.create('style', 'opacity')]

export const OpacityRow = React.memo(() => {
  const opacityMetadata = useInspectorStyleInfo('opacity')

  const opacity = opacityMetadata.value
  const scale = opacity.unit === '%' ? 100 : 1

  const isVisible = useIsSubSectionVisible('opacity')
  const updateScaledValue = React.useCallback(
    (newValue: number, oldValue: CSSNumber) => {
      return setCSSNumberValue(oldValue, newValue * scale)
    },
    [scale],
  )

  const [onScaledSubmit, onScaledTransientSubmit] =
    opacityMetadata.useSubmitValueFactory(updateScaledValue)
  const transformNewScaledValue = React.useCallback<(newValue: number) => CSSNumber>(
    (newValue) => updateScaledValue(newValue, opacity),
    [updateScaledValue, opacity],
  )

  const opacityContextMenuItems = InspectorContextMenuItems.optionalAddOnUnsetValues(
    opacity != null,
    ['opacity'],
    opacityMetadata.onUnsetValues,
  )

  const wrappedOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    opacityMetadata.onSubmitValue,
    opacityMetadata.onUnsetValues,
  )
  const wrappedOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    opacityMetadata.onTransientSubmitValue,
    opacityMetadata.onUnsetValues,
  )

  if (!isVisible) {
    return null
  }

  return (
    <InspectorContextMenuWrapper
      id='opacity-row-context-menu'
      items={opacityContextMenuItems}
      data={null}
    >
      <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
        <PropertyLabel target={opacityProp}>Opacity</PropertyLabel>
        <SliderNumberControl
          id='opacity'
          key='opacity'
          testId='opacity'
          value={opacity}
          DEPRECATED_controlOptions={sliderControlOptions}
          controlStatus={opacityMetadata.controlStatus}
          controlStyles={opacityMetadata.controlStyles}
          minimum={0}
          maximum={1}
          stepSize={0.01}
          numberType='UnitlessPercent'
          defaultUnitToHide={null}
          onSubmitValue={wrappedOnSubmitValue}
          onTransientSubmitValue={wrappedOnTransientSubmitValue}
          onSliderSubmitValue={onScaledSubmit}
          onSliderTransientSubmitValue={onScaledTransientSubmit}
          transformSliderValueToCSSNumber={transformNewScaledValue}
        />
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})
