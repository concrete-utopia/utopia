import * as React from 'react'
import * as PP from '../../../../../core/shared/property-path'
import { PropertyLabel } from '../../../widgets/property-label'
import { useInspectorStyleInfo } from '../../../common/property-path-hooks'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { PopupList } from '../../../../../uuiui'
import {
  betterReactMemo,
  SelectOption,
  InspectorContextMenuItems,
  InspectorContextMenuWrapper,
} from '../../../../../uuiui-deps'

const blendModeOptions = [
  { value: 'normal', label: 'Normal' },
  { value: 'multiply', label: 'Multiply' },
  { value: 'screen', label: 'Screen' },
  { value: 'darken', label: 'Darken' },
]

const blendModeProp = [PP.create(['style', 'mixBlendMode'])]

export const BlendModeRow = betterReactMemo('BlendModeRow', () => {
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
      <UIGridRow padded={true} type='<---1fr--->|------172px-------|'>
        <PropertyLabel target={blendModeProp}>Blendmode</PropertyLabel>
        <PopupList
          containerMode='default'
          value={whichBlendModeOption}
          options={blendModeOptions}
          onSubmitValue={onSubmitBlendModeOption}
          controlStyles={blendModeMetadata.controlStyles}
        />
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})
