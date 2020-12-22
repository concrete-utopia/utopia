import * as React from 'react'
import * as PP from '../../../../../core/shared/property-path'
import { PropertyLabel } from '../../../widgets/property-label'
import { GridRow } from '../../../widgets/grid-row'
import { useInspectorStyleInfo } from '../../../common/property-path-hooks'
import { OptionChainControl, OptionChainOption } from '../../../controls/option-chain-control'
import {
  betterReactMemo,
  InspectorContextMenuItems,
  InspectorContextMenuWrapper,
} from '../../../../../uuiui-deps'

const overflowProp = [PP.create(['style', 'overflow'])]

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

export const OverflowRow = betterReactMemo('OverflowRow', () => {
  const {
    value,
    controlStatus,
    controlStyles,
    onSubmitValue,
    onUnsetValues,
  } = useInspectorStyleInfo('overflow')

  const overflowContextMenuItems = InspectorContextMenuItems.optionalAddOnUnsetValues(
    value != null,
    ['overflow'],
    onUnsetValues,
  )

  return (
    <InspectorContextMenuWrapper
      id='overflow-row-context-menu'
      data={null}
      items={overflowContextMenuItems}
    >
      <GridRow padded={true} type='<---1fr--->|------172px-------|'>
        <PropertyLabel target={overflowProp}>Overflow</PropertyLabel>
        <OptionChainControl
          id={'overflow-control'}
          key={'overflow-control'}
          onSubmitValue={onSubmitValue}
          value={value}
          options={OverflowControlOptions}
          controlStatus={controlStatus}
          controlStyles={controlStyles}
        />
      </GridRow>
    </InspectorContextMenuWrapper>
  )
})
