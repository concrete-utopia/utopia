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
  { value: 'darken', label: 'Darken' },
]

const blendModeProp = [PP.create('style', 'mixBlendMode')]

export const BlendModeRow = React.memo(() => {
  const blendModeMetadata = useInspectorStyleInfo('mixBlendMode')
  const [onSubmitBlendModeOption] = blendModeMetadata.useSubmitValueFactory(
    (selectedOption: SelectOption) => selectedOption.value,
  )
  const blendMode = blendModeMetadata.value

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
      <PopupList
        containerMode='default'
        value={whichBlendModeOption}
        options={blendModeOptions}
        onSubmitValue={onSubmitBlendModeOption}
        controlStyles={blendModeMetadata.controlStyles}
        style={{ background: 'transparent' }}
      />
    </InspectorContextMenuWrapper>
  )
})
