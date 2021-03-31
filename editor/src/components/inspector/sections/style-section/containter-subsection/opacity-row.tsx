import * as React from 'react'
import * as PP from '../../../../../core/shared/property-path'
import { PropertyLabel } from '../../../widgets/property-label'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { useInspectorStyleInfo, useIsSubSectionVisible } from '../../../common/property-path-hooks'
import { useWrappedEmptyOrUnknownOnSubmitValue, NumberInput } from '../../../../../uuiui'
import {
  betterReactMemo,
  CSSUtils,
  InspectorContextMenuItems,
  InspectorContextMenuWrapper,
  SliderControl,
} from '../../../../../uuiui-deps'

const sliderControlOptions = {
  minimum: 0,
  maximum: 1,
  stepSize: 0.01,
  origin: 1,
  filled: true,
}

// TODO: path should match target
const opacityProp = [PP.create(['style', 'opacity'])]

export const OpacityRow = betterReactMemo('OpacityRow', () => {
  const opacityMetadata = useInspectorStyleInfo('opacity')

  const opacity = opacityMetadata.value
  const scale = opacity.unit === '%' ? 100 : 1
  const scaledOpacity = opacity.value / scale

  const isVisible = useIsSubSectionVisible('opacity')
  const [onScaledSubmit, onScaledTransientSubmit] = opacityMetadata.useSubmitValueFactory(
    (newValue: number, oldValue) => {
      return CSSUtils.setCSSNumberValue(oldValue, newValue * scale)
    },
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
        <UIGridRow padded={false} variant='<--------auto-------->|--45px--|'>
          <SliderControl
            DEPRECATED_controlOptions={sliderControlOptions}
            id={`opacity-slider`}
            key={`opacity-slider`}
            testId={`opacity-slider`}
            value={scaledOpacity}
            controlStatus={opacityMetadata.controlStatus}
            controlStyles={opacityMetadata.controlStyles}
            onSubmitValue={onScaledSubmit}
            onTransientSubmitValue={onScaledTransientSubmit}
            onForcedSubmitValue={onScaledSubmit}
          />
          <NumberInput
            value={opacity}
            minimum={0}
            maximum={1}
            stepSize={0.01}
            id='opacity-number-control'
            testId='opacity-number-control'
            onSubmitValue={wrappedOnSubmitValue}
            onTransientSubmitValue={wrappedOnTransientSubmitValue}
            controlStatus={opacityMetadata.controlStatus}
            numberType='UnitlessPercent'
            defaultUnitToHide={null}
          />
        </UIGridRow>
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})
