import * as RadixDropdownMenu from '@radix-ui/react-dropdown-menu'
import type { DropdownMenuContentProps } from '@radix-ui/react-dropdown-menu'
import { styled } from '@stitches/react'
import React from 'react'
import { colorTheme } from './styles/theme'
import { Icons } from './icons'
import { when } from '../utils/react-conditionals'

const RadixItemContainer = styled(RadixDropdownMenu.Item, {
  minWidth: 128,
  padding: '4px 8px',
  display: 'flex',
  justifyContent: 'space-between',
  cursor: 'pointer',
  '&[data-highlighted]': { backgroundColor: colorTheme.subtleBackground.value, borderRadius: 6 },
})

const RadixDropdownContent = styled(RadixDropdownMenu.Content, {
  padding: '6px 8px',
  flexDirection: 'column',
  backgroundColor: colorTheme.inspectorBackground.value,
  borderRadius: 4,
  display: 'grid',
  gridTemplateRows: '1fr auto',
})

export interface DropdownMenuItem {
  id: string
  label: React.ReactNode
  shortcut?: string
  checked?: boolean
  onSelect: () => void
}

export interface DropdownMenuProps {
  opener: (open: boolean) => React.ReactNode
  items: DropdownMenuItem[]
  sideOffset?: number
  alignOffset?: number
  side?: DropdownMenuContentProps['side']
}

export const DropdownMenu = React.memo<DropdownMenuProps>((props) => {
  const stopPropagation = React.useCallback((e: React.KeyboardEvent) => e.stopPropagation(), [])

  const [open, onOpen] = React.useState(false)

  const shouldShowCheckboxes = props.items.some((i) => i.checked != null)

  return (
    <RadixDropdownMenu.Root open={open} onOpenChange={onOpen}>
      <RadixDropdownMenu.Trigger style={{ background: 'none', border: 'none' }}>
        {props.opener(open)}
      </RadixDropdownMenu.Trigger>
      <RadixDropdownMenu.Portal>
        <RadixDropdownContent
          onKeyDown={stopPropagation}
          sideOffset={props.sideOffset}
          side={props.side}
          collisionPadding={{ top: -4 }}
          align='start'
          alignOffset={props.alignOffset}
        >
          {props.items.map((item) => (
            <RadixItemContainer key={item.id} onSelect={item.onSelect}>
              <div style={{ display: 'flex', gap: 4 }}>
                {when(
                  shouldShowCheckboxes,
                  <div style={{ opacity: item.checked ? 1 : 0 }}>
                    <Icons.Checkmark />
                  </div>,
                )}
                <span>{item.label}</span>
              </div>
              <span style={{ opacity: 0.5 }}>{item.shortcut}</span>
            </RadixItemContainer>
          ))}
        </RadixDropdownContent>
      </RadixDropdownMenu.Portal>
    </RadixDropdownMenu.Root>
  )
})
