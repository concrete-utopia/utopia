import React from 'react'
import { FlexRow, SectionActionSheet, SquareButton, colorTheme } from '../../uuiui'
import { ExpandableIndicator } from '../navigator/navigator-item/expandable-indicator'

export function InspectorSectionHeader({
  title,
  open,
  toggle,
}: {
  title: string
  open: boolean
  toggle: () => void
}) {
  return (
    <FlexRow
      style={{
        padding: 8,
        cursor: 'pointer',
        boxShadow: `0 -1px 0 ${colorTheme.seperator.value}`,
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
          textTransform: 'uppercase',
        }}
      >
        {title}
      </div>
      <SectionActionSheet className='actionsheet' style={{ gap: 4 }}>
        <SquareButton highlight style={{ width: 12 }}>
          <ExpandableIndicator visible collapsed={!open} selected={false} />
        </SquareButton>
      </SectionActionSheet>
    </FlexRow>
  )
}
