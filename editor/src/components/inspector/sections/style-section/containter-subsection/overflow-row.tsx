import * as React from 'react'
import { betterReactMemo, InspectorContextMenuItems, InspectorContextMenuWrapper } from 'uuiui-deps'
import * as PP from '../../../../../core/shared/property-path'
import { CSSOverflow } from '../../../common/css-utils'
import { useInspectorStyleInfo } from '../../../common/property-path-hooks'
import { SegmentControl, SegmentOption } from '../../../controls/segment-control'
import { GridRow } from '../../../widgets/grid-row'
import { PropertyLabel } from '../../../widgets/property-label'

const overflowProp = [PP.create(['style', 'overflow'])]

const OverflowControlOptions: Array<SegmentOption<CSSOverflow>> = [
  {
    label: 'Hidden',
    value: 'hidden',
  },
  {
    label: 'Visible',
    value: 'visible',
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
        <SegmentControl
          id={'overflow-control'}
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
