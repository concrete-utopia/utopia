import React from 'react'
import type { GoogleFontsResource } from '../../../printer-parsers/html/external-resources-parser'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'

interface GoogleFontsResourcesListItemProps {
  value: GoogleFontsResource
}

export const GoogleFontsResourcesListItem = React.memo<GoogleFontsResourcesListItemProps>(
  ({ value }) => {
    return (
      <UIGridRow padded={false} variant='<-------1fr------>|----80px----|'>
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
