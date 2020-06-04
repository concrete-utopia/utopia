import * as React from 'react'
import { PopupList, Icn } from 'uuiui'
import { betterReactMemo } from 'uuiui-deps'
import { LayoutWrapper } from '../../../../core/shared/project-file-types'
import * as Utils from '../../../../utils/utils'
import { SelectOption } from '../../controls/select-control'
import { getControlStyles } from '../../common/control-status'
import { GridRow } from '../../widgets/grid-row'
import { NO_OP } from '../../../../core/shared/utils'

export interface LayoutWrapperCoreProps {
  onWrap: (value: LayoutWrapper) => void
  onUnwrap: () => void
}

export interface LayoutWrapperRowProps {
  value: null | LayoutWrapper
}

export const LayoutWrapperRow = betterReactMemo(
  'LayoutWrapperSection',
  (props: LayoutWrapperCoreProps & LayoutWrapperRowProps) => {
    const onSelect = React.useCallback(
      (selectOption: SelectOption) => {
        const value = selectOption.value
        if (value == null) {
          props.onUnwrap()
        } else {
          props.onWrap(value)
        }
      },
      [props],
    )

    return (
      <GridRow padded={true} type='<---1fr--->|------172px-------|'>
        <span>Wrap in </span>
        <PopupList
          disabled={!controlStyles.interactive}
          value={{ value: props.value, label: props.value || 'Not wrapped' }}
          onSubmitValue={onSelect}
          options={popupListOptions}
          containerMode='default'
        />
      </GridRow>
    )
  },
)

const controlStyles = getControlStyles('simple')

const popupListOptions: any[] = [
  { value: null, label: 'Not wrapped' },
  {
    label: 'Wrap In',
    options: [
      { value: 'Positionable', label: 'Positionable' },
      { value: 'Resizeable', label: 'Resizeable' },
      { value: 'Layoutable', label: 'Layoutable' },
    ],
  },
]
