import * as React from 'react'
import { components, ValueType } from 'react-select'
import Select from 'react-select'
import { DeviceID, deviceInfoList } from './devices'

export const getDeviceReactSelectOption = (deviceID: DeviceID): DeviceReactSelectOption => {
  const device = deviceInfoList[deviceID]
  return {
    value: deviceID,
    label: `${device.prettyName}`,
  }
}

export type DeviceReactSelectOption = { value: DeviceID; label: string }
export type DeviceReactSelectList = Array<DeviceReactSelectOption>
export const deviceReactSelectOptionList: DeviceReactSelectList = (Object.keys(
  deviceInfoList,
) as Array<DeviceID>).map((value): DeviceReactSelectOption => getDeviceReactSelectOption(value))

interface PreviewReactSelectDeviceSelectorProps {
  value: DeviceReactSelectOption
  onChange: (value: DeviceReactSelectOption) => void
  caratOffset: number
}

export const PreviewReactSelectDeviceSelector: React.FunctionComponent<PreviewReactSelectDeviceSelectorProps> = ({
  value,
  onChange,
  caratOffset,
}) => {
  const PreviewReactSelectSingleValue = (singleValueProps: any) => {
    return components.SingleValue == null ? null : (
      <components.SingleValue {...singleValueProps}>
        <span
          style={{
            whiteSpace: 'nowrap',
            maxWidth: '10em',
            textOverflow: 'ellipsis',
            overflow: 'hidden',
            display: 'inline-block',
            paddingRight: '.3em',
          }}
        >
          {singleValueProps.children}
        </span>
        <svg
          width='9'
          height='5'
          viewBox='0 0 9 5'
          xmlns='http://www.w3.org/2000/svg'
          style={{
            position: 'relative',
            bottom: caratOffset,
          }}
        >
          <path
            id='dropdown_control'
            d='M1,1 C5,4.66 3,4.66 7,1'
            strokeWidth='1'
            fill='none'
            stroke='rgb(155, 155, 155)'
          />
        </svg>
      </components.SingleValue>
    )
  }

  const selectOnChange = React.useCallback(
    (newValue: ValueType<DeviceReactSelectOption>) => {
      if (newValue != null && !Array.isArray(newValue)) {
        onChange(newValue as DeviceReactSelectOption)
      }
    },
    [onChange],
  )

  return (
    <Select
      className={``}
      classNamePrefix='preview-lightselect'
      menuShouldScrollIntoView={true}
      menuPlacement='auto'
      value={value}
      isClearable={false}
      isSearchable={false}
      isDisabled={false}
      components={{ SingleValue: PreviewReactSelectSingleValue }}
      onChange={selectOnChange}
      options={deviceReactSelectOptionList}
      styles={{
        container: (base: any, state: any) => {
          return {
            ...base,
            backgroundColor: 'transparent',
            boxShadow: 'none',
            textAlign: 'left',
            cursor: 'default',
          }
        },
        menu: (base: any, state: any) => {
          return {
            ...base,
            width: '200px !important',
            left: -12,
          }
        },
        control: (base: any, state: any) => {
          return {
            display: 'inline-block',
          }
        },
        valueContainer: (base: any, state: any) => {
          return {
            display: 'inline-block',
          }
        },
        singleValue: (base: any, state: any) => {
          return {
            display: 'inline-block',
            height: 18,
            position: 'relative',
          }
        },
        indicatorsContainer: () => {
          return {
            display: 'none',
          }
        },
      }}
    />
  )
}

type Dimensions = { width: number; height: number }

export const calculatePreviewScale = (
  viewport: Dimensions,
  edgePadding: number,
  sourceWidth: number,
  sourceHeight: number,
): number => {
  const destinationWidth = viewport.width - 2 * edgePadding
  const destinationHeight = viewport.height - 2 * edgePadding

  if (sourceWidth > 0 && sourceHeight > 0) {
    const scale = Math.min(destinationWidth / sourceWidth, destinationHeight / sourceHeight)
    return Math.min(scale, 1)
  } else {
    return 1
  }
}
