import React from 'react'
import type { CSSUnknownArrayItem, EmptyInputValue } from '../common/css-utils'
import { isCSSUnknownArrayItem, isEmptyInputValue } from '../common/css-utils'
import { CSSArrayItem, getIndexedSpliceArrayItem } from '../common/inspector-utils'
import type { UseSubmitValueFactory } from '../common/property-path-hooks'
import { stopPropagation } from '../common/inspector-utils'
import type { ControlStyles } from '../common/control-styles'
import type { ControlStatus } from '../common/control-status'
import { PropertyRow } from '../widgets/property-row'
import { StringControl } from './string-control'
import { CheckboxInput, FlexRow, Tooltip } from '../../../uuiui'

interface FakeUnknownArrayItemProps {
  controlStatus: ControlStatus
}

export const FakeUnknownArrayItem = React.memo<FakeUnknownArrayItemProps>((props) => (
  <PropertyRow
    style={{
      gridTemplateColumns: '12px 1fr',
      gridColumnGap: 8,
    }}
  >
    <CheckboxInput
      disabled={true}
      controlStatus={props.controlStatus}
      onMouseDown={stopPropagation}
    />
    <FlexRow style={{ height: 22 }}>Unknown</FlexRow>
  </PropertyRow>
))

function getIndexedUpdateUnknownArrayItemValue<T>(index: number) {
  return function updateUnknownArrayItemValue(
    newValue: string | EmptyInputValue,
    oldValue: ReadonlyArray<T>,
  ): ReadonlyArray<T> {
    if (oldValue[index] != null) {
      const oldLayer = oldValue[index]
      let newCSSBackgroundLayers = [...oldValue]
      if (isCSSUnknownArrayItem(oldLayer)) {
        if (isEmptyInputValue(newValue)) {
          delete newCSSBackgroundLayers[index]
        } else {
          newCSSBackgroundLayers[index] = {
            ...oldLayer,
            value: newValue,
          }
        }
      }
    }
    return oldValue
  }
}

interface UnknownArrayItemProps<T> {
  index: number
  value: CSSUnknownArrayItem
  controlStatus: ControlStatus
  controlStyles: ControlStyles
  useSubmitTransformedValuesFactory: UseSubmitValueFactory<ReadonlyArray<T | CSSUnknownArrayItem>>
}

export function UnknownArrayItem<T>(props: UnknownArrayItemProps<T>) {
  const [onSubmitValue] = props.useSubmitTransformedValuesFactory(
    getIndexedUpdateUnknownArrayItemValue<T | CSSUnknownArrayItem>(props.index),
  )
  return (
    <PropertyRow
      style={{
        gridTemplateColumns: '12px 1fr',
        gridColumnGap: 8,
      }}
    >
      <CheckboxInput
        checked={props.value.enabled}
        controlStatus={props.controlStatus}
        onMouseDown={stopPropagation}
      />
      <Tooltip title='Value may be valid, but can’t be parsed by Utopia' placement='left'>
        <div>
          <StringControl
            style={{ height: 22 }}
            id={`unknown-${props.index}`}
            testId={`unknown-${props.index}`}
            key={`unknown-${props.index}-${props.value.value}`}
            value={props.value.value}
            onSubmitValue={onSubmitValue}
            controlStatus={props.controlStatus}
            controlStyles={props.controlStyles}
          />
        </div>
      </Tooltip>
    </PropertyRow>
  )
}
