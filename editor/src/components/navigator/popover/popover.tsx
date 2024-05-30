import React from 'react'
import * as RadixPopover from '@radix-ui/react-popover'
import { colorTheme } from '../../../uuiui'

interface PopoverProps {
  activator: React.ReactNode
  children: React.ReactNode
}

export const Popover = ({activator, children}: PopoverProps) => (
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
      <RadixPopover.Content align='start' sideOffset={8} style={{
        background: colorTheme.bg0.value,
        width: 260,
        padding: 20,
        borderRadius: 4,
      }}>
        {children}
      </RadixPopover.Content>
    </RadixPopover.Portal>
  </RadixPopover.Root>
)