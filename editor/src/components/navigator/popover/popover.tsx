import React from 'react'
import * as RadixPopover from '@radix-ui/react-popover'
import { colorTheme } from '../../../uuiui'

const handleClick = (event: React.MouseEvent) => {
  event.preventDefault()
  event.stopPropagation()
}

interface PopoverProps {
  activator: React.ReactNode
  align?: 'start' | 'center' | 'end'
  children: React.ReactNode
}

export const Popover = ({align = 'start', activator, children}: PopoverProps) => (
  <RadixPopover.Root>
    <RadixPopover.Trigger style={{
      background: 'none',
      outline: 'none',
      border: 'none',
      padding: 0,
      cursor: 'pointer',
    }}>
      {activator}
    </RadixPopover.Trigger>
    <RadixPopover.Anchor />
    <RadixPopover.Portal>
      <RadixPopover.Content
        onContextMenu={handleClick}
        onClick={handleClick}
        align={align}
        sideOffset={8}
        style={{
          background: colorTheme.bg0.value,
          width: 260,
          padding: '4px 16px',
          borderRadius: 4,
        }}
      >
        {children}
      </RadixPopover.Content>
    </RadixPopover.Portal>
  </RadixPopover.Root>
)