import React from 'react'
import * as PP from '../../../../../core/shared/property-path'
import { PropertyLabel } from '../../../widgets/property-label'
import { useInspectorStyleInfo } from '../../../common/property-path-hooks'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { PopupList } from '../../../../../uuiui'
import type { SelectOption } from '../../../../../uuiui-deps'
import { InspectorContextMenuItems, InspectorContextMenuWrapper } from '../../../../../uuiui-deps'

const blendModeOptions = [
  { value: 'normal', label: 'Normal' },
  { value: 'multiply', label: 'Multiply' },
  { value: 'screen', label: 'Screen' },
  { value: 'overlay', label: 'Overlay' },
  { value: 'darken', label: 'Darken' },
  { value: 'lighten', label: 'Lighten' },
  { value: 'color-dodge', label: 'Color Dodge' },
  { value: 'color-burn', label: 'Color Burn' },
  { value: 'hard-light', label: 'Hard Light' },
  { value: 'soft-light', label: 'Soft Light' },
  { value: 'difference', label: 'Difference' },
  { value: 'exclusion', label: 'Exclusion' },
  { value: 'hue', label: 'Hue' },
  { value: 'saturation', label: 'Saturation' },
  { value: 'color', label: 'Color' },
  { value: 'luminosity', label: 'Luminosity' },
]

const blendModeProp = [PP.create('style', 'mixBlendMode')]

export const BlendModeRow = React.memo(() => {
  const blendModeMetadata = useInspectorStyleInfo('mixBlendMode')

  // Check if blendModeMetadata is correctly populated
  if (!blendModeMetadata) {
    console.error('blendModeMetadata is not available')
    return null
  }

  const [onSubmitBlendModeOption] = blendModeMetadata.useSubmitValueFactory(
    (selectedOption: SelectOption) => selectedOption.value,
  )

  const blendMode = blendModeMetadata.value

  // Check if blendMode is correctly populated
  if (!blendMode) {
    console.error('blendMode is not available')
  }

  const whichBlendModeOption = blendModeOptions.find((option) => option.value === blendMode)

  const blendModeContextMenuItems = InspectorContextMenuItems.optionalAddOnUnsetValues(
    blendMode != null,
    ['mixBlendMode'],
    blendModeMetadata.onUnsetValues,
  )

  return (
    <InspectorContextMenuWrapper
      id='blendmode-row-context-menu'
      items={blendModeContextMenuItems}
      data={null}
    >
      <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
        <PopupList
          containerMode='default'
          value={whichBlendModeOption}
          options={blendModeOptions}
          onSubmitValue={onSubmitBlendModeOption}
          controlStyles={blendModeMetadata.controlStyles}
          style={{ background: 'transparent', width: 133 }}
        />
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})
