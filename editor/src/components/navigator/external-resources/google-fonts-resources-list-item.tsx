import * as React from 'react'
import { GoogleFontsResource } from '../../../printer-parsers/html/external-resources-parser'
import { betterReactMemo } from '../../../uuiui-deps'
import { GridRow } from '../../inspector/widgets/grid-row'

interface GoogleFontsResourcesListItemProps {
  value: GoogleFontsResource
}

export const GoogleFontsResourcesListItem = betterReactMemo<GoogleFontsResourcesListItemProps>(
  'GoogleFontsResourcesListItem',
  ({ value }) => {
    return (
      <GridRow padded={false} type='<-------1fr------>|----80px----|'>
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
      </GridRow>
    )
  },
)
