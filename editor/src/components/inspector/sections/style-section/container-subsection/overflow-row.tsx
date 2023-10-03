import React from 'react'
import * as PP from '../../../../../core/shared/property-path'
import { PropertyLabel } from '../../../widgets/property-label'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { useInspectorStyleInfo, useIsSubSectionVisible } from '../../../common/property-path-hooks'
import type { OptionChainOption } from '../../../controls/option-chain-control'
import { OptionChainControl } from '../../../controls/option-chain-control'
import { InspectorContextMenuItems, InspectorContextMenuWrapper } from '../../../../../uuiui-deps'

const overflowProp = [PP.create('style', 'overflow')]

const OverflowControlOptions: Array<OptionChainOption<boolean>> = [
  {
    tooltip: 'Hidden',
    label: 'Hidden',
    value: false,
  },
  {
    tooltip: 'Visible',
    label: 'Visible',
    value: true,
  },
]

export const OverflowRow = React.memo(() => {
  const isVisible = useIsSubSectionVisible('overflow')

  const { value, controlStatus, controlStyles, onSubmitValue, onUnsetValues } =
    useInspectorStyleInfo('overflow')

  const overflowContextMenuItems = InspectorContextMenuItems.optionalAddOnUnsetValues(
    value != null,
    ['overflow'],
    onUnsetValues,
  )

  if (!isVisible) {
    return null
  }

  return (
    <InspectorContextMenuWrapper
      id='overflow-row-context-menu'
      data={null}
      items={overflowContextMenuItems}
    >
      <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
        <PropertyLabel target={overflowProp}>Overflow</PropertyLabel>
        <OptionChainControl
          id={'overflow-control'}
          key={'overflow-control'}
          testId={'overflow-control'}
          onSubmitValue={onSubmitValue}
          value={value}
          options={OverflowControlOptions}
          controlStatus={controlStatus}
          controlStyles={controlStyles}
        />
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})
