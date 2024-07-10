import * as RadixDropdownMenu from '@radix-ui/react-dropdown-menu'
import type { DropdownMenuContentProps } from '@radix-ui/react-dropdown-menu'
import { styled } from '@stitches/react'
import React from 'react'
import { colorTheme, useColorTheme } from './styles/theme'

const RadixItemContainer = styled(RadixDropdownMenu.Item, {
  padding: '4px 8px',
  '&[data-highlighted]': { backgroundColor: colorTheme.subtleBackground.value, borderRadius: 6 },
})

export interface DropdownMenuItem {
  id: string
  label: React.ReactNode
  onSelect: () => void
}

export interface DropdownMenuProps {
  opener: React.ReactNode
  items: DropdownMenuItem[]
  sideOffset?: number
  side?: DropdownMenuContentProps['side']
}

export const DropdownMenu = React.memo<DropdownMenuProps>((props) => {
  const stopPropagation = React.useCallback((e: React.KeyboardEvent) => e.stopPropagation(), [])
  const theme = useColorTheme()

  return (
    <RadixDropdownMenu.Root>
      <RadixDropdownMenu.Trigger style={{ background: 'none', border: 'none' }}>
        {props.opener}
      </RadixDropdownMenu.Trigger>
      <RadixDropdownMenu.Portal>
        <RadixDropdownMenu.Content
          onKeyDown={stopPropagation}
          sideOffset={props.sideOffset}
          side={props.side}
          collisionPadding={{ top: 8 }}
          align='start'
          style={{
            padding: '6px 8px',
            flexDirection: 'column',
            backgroundColor: theme.inspectorBackground.value,
            borderRadius: 4,
            display: 'flex',
            gap: 4,
          }}
        >
          {props.items.map((item) => (
            <RadixItemContainer
              key={item.id}
              style={{ cursor: 'pointer' }}
              onSelect={item.onSelect}
            >
              {item.label}
            </RadixItemContainer>
          ))}
        </RadixDropdownMenu.Content>
      </RadixDropdownMenu.Portal>
    </RadixDropdownMenu.Root>
  )
})
