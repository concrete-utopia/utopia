import * as React from 'react'
import { components } from 'react-select'
import { OptionProps } from 'react-select/src/components/Option'
import { GoogleFontVariantOption } from './google-fonts-utils'
import { FlexRow, Icons } from '../../../uuiui'

export const GoogleFontsResourcesListSearchOption: React.FunctionComponent<OptionProps<
  GoogleFontVariantOption
>> = (props) => {
  if (components.Option != null) {
    return (
      <components.Option {...props}>
        <FlexRow>
          <div style={{ flexGrow: 1 }}>{props.label}</div>
          {props.isSelected ? <Icons.Downloaded /> : <Icons.Download />}
        </FlexRow>
      </components.Option>
    )
  } else {
    return null
  }
}
