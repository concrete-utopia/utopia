import * as React from 'react'
import { CheckboxInput, FlexRow, Tooltip } from 'uuiui'
import { betterReactMemo } from 'uuiui-deps'
import { ControlStatus, ControlStyles } from '../common/control-status'
import {
  CSSUnknownArrayItem,
  EmptyInputValue,
  isCSSUnknownArrayItem,
  isEmptyInputValue,
} from '../common/css-utils'
import { stopPropagation } from '../common/inspector-utils'
import { UseSubmitValueFactory } from '../common/property-path-hooks'
import { StringControl } from '../controls/string-control'
import { PropertyRow } from '../widgets/property-row'

interface FakeUnknownArrayItemProps {
  controlStatus: ControlStatus
}

export const FakeUnknownArrayItem = betterReactMemo<FakeUnknownArrayItemProps>(
  'FakeUnknownArrayItem',
  (props) => (
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
  ),
)

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
