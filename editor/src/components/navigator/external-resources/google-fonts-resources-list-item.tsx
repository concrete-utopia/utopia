import * as React from 'react'
import { GoogleFontsResource } from '../../../printer-parsers/html/external-resources-parser'
import { betterReactMemo } from '../../../uuiui-deps'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'

interface GoogleFontsResourcesListItemProps {
  value: GoogleFontsResource
}

export const GoogleFontsResourcesListItem = betterReactMemo<GoogleFontsResourcesListItemProps>(
  'GoogleFontsResourcesListItem',
  ({ value }) => {
    return (
      <UIGridRow padded={false} layout='<-------1fr------>|----80px----|'>
        <div
          style={{
            textOverflow: 'ellipsis',
            overflow: 'hidden',
            whiteSpace: 'nowrap',
          }}
        >
          {value.fontFamily}
        </div>
        <div
          style={{
            textOverflow: 'ellipsis',
            overflow: 'hidden',
            whiteSpace: 'nowrap',
            textAlign: 'right',
          }}
        >
          400
        </div>
      </UIGridRow>
    )
  },
)
