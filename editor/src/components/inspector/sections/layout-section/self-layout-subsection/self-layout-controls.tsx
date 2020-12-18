import * as React from 'react'
import { OptionChainControl } from '../../../controls/option-chain-control'
import { useInspectorStyleInfo } from '../../../common/property-path-hooks'
import { SelfLayoutTab } from './self-layout-subsection'
import { getControlStyles } from '../../../common/control-status'
import { betterReactMemo } from '../../../../../uuiui-deps'

interface LayoutTypePickerProps {
  value: SelfLayoutTab
  setActiveTab: React.Dispatch<SelfLayoutTab>
}

export const LayoutTypePicker = betterReactMemo(
  'LayoutTypePicker',
  ({ value, setActiveTab }: LayoutTypePickerProps) => {
    const { onSubmitValue, onUnsetValues, controlStatus, controlStyles } = useInspectorStyleInfo(
      'position',
    )
    const changeSelectedType = React.useCallback(
      (newValue: SelfLayoutTab) => {
        setActiveTab(newValue)
        switch (newValue) {
          case 'absolute':
          case 'sticky': {
            onSubmitValue(newValue)
            break
          }
          case 'flex':
          case 'flow': {
            onUnsetValues()
            break
          }
          default: {
            const _exhaustiveCheck: never = newValue
            throw new Error(`SelfLayoutTab type ${newValue} not found`)
          }
        }
      },
      [onSubmitValue, setActiveTab, onUnsetValues],
    )

    return (
      <OptionChainControl
        id='layoutSystem'
        key='layoutSystem'
        onSubmitValue={changeSelectedType}
        value={value}
        options={layoutSystemOptions}
        controlStatus={value === 'absolute' || value === 'sticky' ? controlStatus : 'simple'}
        controlStyles={
          value === 'absolute' || value === 'sticky' ? controlStyles : getControlStyles('simple')
        }
      />
    )
  },
)

const layoutSystemOptions = [
  {
    value: 'absolute',
    tooltip: "Shows controls for absolute positioning and sets position: 'absolute'",
    label: 'Absolute',
  },
  {
    value: 'flex',
    tooltip: 'Flex children properties',
    label: 'Flex',
  },
  {
    value: 'flow',
    tooltip: 'Size-only properties',
    label: 'Flow',
  },
  {
    value: 'sticky',
    tooltip: "Shows controls for sticky positioning and sets position: 'sticky'",
    label: 'Sticky',
  },
]
