import * as React from 'react'
import { components } from 'react-select'
import { GroupProps } from 'react-select/src/components/Group'
import { GoogleFontVariantOption } from './google-fonts-utils'
import { FlexRow, Icons } from '../../../uuiui'

export const GoogleFontsResourcesListSearchGroupHeading: React.FunctionComponent<GroupProps<
  GoogleFontVariantOption
>> = (props) => {
  if (components.GroupHeading != null) {
    const expanded = true
    return (
      <components.GroupHeading {...props}>
        <FlexRow>
          <div style={{}}>
            {expanded ? <Icons.ExpansionArrowDown /> : <Icons.ExpansionArrowRight />}
          </div>
          <div style={{ flexGrow: 1 }}>{props.children}</div>
        </FlexRow>
      </components.GroupHeading>
    )
  } else {
    return null
  }
}
