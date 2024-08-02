import React from 'react'
import { FlexRow, SectionActionSheet, SquareButton, colorTheme } from '../../uuiui'
import { ExpandableIndicator } from '../navigator/navigator-item/expandable-indicator'

export function InspectorSectionHeader({
  title,
  open,
  toggle,
  uppercase = false,
}: {
  title: string
  open: boolean
  toggle: () => void
  uppercase?: boolean
}) {
  return (
    <FlexRow
      style={{
        padding: 8,
        cursor: 'pointer',
      }}
      onClick={toggle}
      data-testid={`section-header-${title}`}
    >
      <div
        style={{
          flexGrow: 1,
          display: 'inline',
          overflow: 'hidden',
          fontSize: '11px',
          fontWeight: 600,
          textTransform: uppercase ? 'uppercase' : undefined,
        }}
      >
        {title}
      </div>
      <SectionActionSheet className='actionsheet' style={{ gap: 4 }}>
        <SquareButton highlightOnHover style={{ width: 12 }}>
          <ExpandableIndicator visible collapsed={!open} selected={false} />
        </SquareButton>
      </SectionActionSheet>
    </FlexRow>
  )
}
