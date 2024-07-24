import * as RadixDropdownMenu from '@radix-ui/react-dropdown-menu'
import { styled } from '@stitches/react'
import React from 'react'
import { colorTheme } from './styles/theme'
import { Icons } from './icons'
import { when } from '../utils/react-conditionals'

const RadixItemContainer = styled(RadixDropdownMenu.Item, {
  minWidth: 128,
  padding: '4px 8px',
  display: 'flex',
  gap: 12,
  justifyContent: 'space-between',
  alignItems: 'center',
  cursor: 'pointer',
  color: colorTheme.contextMenuForeground.value,
  '&[data-highlighted]': {
    backgroundColor: colorTheme.contextMenuHighlightBackground.value,
    color: colorTheme.contextMenuForeground.value,
    borderRadius: 6,
  },
})

const RadixDropdownContent = styled(RadixDropdownMenu.Content, {
  padding: '6px 8px',
  flexDirection: 'column',
  backgroundColor: colorTheme.contextMenuBackground.value,
  borderRadius: 6,
  display: 'grid',
  gridTemplateRows: '1fr auto',
})

export interface DropdownMenuItem {
  id: string
  label: React.ReactNode
  shortcut?: string
  icon?: React.ReactNode
  checked?: boolean
  onSelect: () => void
}

export interface DropdownMenuProps {
  opener: (open: boolean) => React.ReactNode
  items: DropdownMenuItem[]
  align?: RadixDropdownMenu.DropdownMenuContentProps['align']
  sideOffset?: number
  alignOffset?: number
}

export const DropdownMenu = React.memo<DropdownMenuProps>((props) => {
  const stopPropagation = React.useCallback((e: React.KeyboardEvent) => {
    const hasModifiers = e.altKey || e.metaKey || e.shiftKey || e.ctrlKey
    if (!hasModifiers) {
      e.stopPropagation()
    }
  }, [])
  const onEscapeKeyDown = React.useCallback((e: KeyboardEvent) => e.stopPropagation(), [])

  const [open, onOpen] = React.useState(false)

  const shouldShowCheckboxes = props.items.some((i) => i.checked != null)
  const shouldShowIcons = props.items.some((i) => i.icon != null)

  return (
    <RadixDropdownMenu.Root open={open} onOpenChange={onOpen}>
      <RadixDropdownMenu.Trigger style={{ background: 'none', border: 'none', padding: 0 }}>
        {props.opener(open)}
      </RadixDropdownMenu.Trigger>
      <RadixDropdownMenu.Portal>
        <RadixDropdownContent
          onKeyDown={stopPropagation}
          onEscapeKeyDown={onEscapeKeyDown}
          sideOffset={props.sideOffset}
          collisionPadding={{ top: -4 }}
          align={props.align ?? 'start'}
          alignOffset={props.alignOffset}
        >
          {props.items.map((item) => (
            <RadixItemContainer key={item.id} onSelect={item.onSelect}>
              <div style={{ display: 'flex', gap: 4, alignItems: 'center' }}>
                {when(
                  shouldShowCheckboxes,
                  <div style={{ opacity: item.checked ? 1 : 0 }}>
                    <Icons.Checkmark color='white' />
                  </div>,
                )}
                {when(
                  shouldShowIcons,
                  <div style={{ opacity: item.icon == null ? 0 : 1 }}>{item.icon}</div>,
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
