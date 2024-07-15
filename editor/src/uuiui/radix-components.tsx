import * as RadixDropdownMenu from '@radix-ui/react-dropdown-menu'
import { styled } from '@stitches/react'
import React from 'react'
import { colorTheme } from './styles/theme'
import { Icons } from './icons'
import { when } from '../utils/react-conditionals'

const StyledItemContainer = styled('div', {
  minWidth: 128,
  padding: '4px 8px',
  cursor: 'pointer',
  color: colorTheme.contextMenuForeground.value,
  '[data-highlighted] > &': {
    backgroundColor: colorTheme.contextMenuHighlightBackground.value,
    color: colorTheme.contextMenuForeground.value,
    borderRadius: 6,
  },
})

const contentStyles = {
  padding: '6px 8px',
  flexDirection: 'column',
  backgroundColor: colorTheme.contextMenuBackground.value,
  borderRadius: 6,
  display: 'grid',
  gridTemplateRows: '1fr auto',
}

const RadixDropdownContent = styled(RadixDropdownMenu.Content, contentStyles)
const RadixDropdownSubcontent = styled(RadixDropdownMenu.SubContent, contentStyles)

export interface DropdownMenuItem {
  id: string
  label: React.ReactNode
  onSelect: () => void
  shortcut?: string
  icon?: React.ReactNode
  checked?: boolean
  subMenuItems?: Omit<DropdownMenuItem, 'subMenuItems'>[]
}

export interface DropdownMenuProps {
  opener: (open: boolean) => React.ReactNode
  items: DropdownMenuItem[]
  sideOffset?: number
  alignOffset?: number
}

interface DropdownItemProps {
  shouldShowCheckboxes: boolean
  shouldShowChevrons: boolean
  shouldShowIcons: boolean
  onSelect: () => void
  label: React.ReactNode
  icon: React.ReactNode | null
  checked: boolean | null
  shortcut: string | null
  subMenuItems: Omit<DropdownMenuItem, 'subMenuItems'>[] | null
}

const ItemContainer = React.memo<
  React.PropsWithChildren<{ isSubmenu: boolean; onSelect: () => void }>
>(({ children, isSubmenu, onSelect }) => {
  if (isSubmenu) {
    return (
      <RadixDropdownMenu.Sub>
        <RadixDropdownMenu.SubTrigger>
          <StyledItemContainer>{children}</StyledItemContainer>
        </RadixDropdownMenu.SubTrigger>
      </RadixDropdownMenu.Sub>
    )
  }

  return (
    <RadixDropdownMenu.Item onSelect={onSelect}>
      <StyledItemContainer>{children}</StyledItemContainer>
    </RadixDropdownMenu.Item>
  )
})

const DropdownItem = React.memo<DropdownItemProps>((props) => {
  const {
    shouldShowCheckboxes,
    shouldShowIcons,
    shouldShowChevrons,
    onSelect,
    checked,
    icon,
    label,
    shortcut,
    subMenuItems,
  } = props

  const shouldShowCheckboxesForSubmenuItems = subMenuItems?.some((s) => s.checked === true) ?? false
  const shouldShowIconsForSubmenuItems = subMenuItems?.some((s) => s.icon != null) ?? false

  return (
    <ItemContainer isSubmenu={subMenuItems != null} onSelect={onSelect}>
      <div
        style={{
          display: 'flex',
          gap: 12,
          justifyContent: 'space-between',
          alignItems: 'center',
        }}
      >
        <div style={{ display: 'flex', gap: 4, alignItems: 'center' }}>
          {when(
            shouldShowCheckboxes,
            <div style={{ opacity: checked ? 1 : 0 }}>
              <Icons.Checkmark color='white' />
            </div>,
          )}
          {when(shouldShowIcons, <div style={{ opacity: icon == null ? 0 : 1 }}>{icon}</div>)}
          <span>{label}</span>
        </div>
        <div style={{ display: 'flex', gap: 4, alignItems: 'center', justifyContent: 'flex-end' }}>
          <span style={{ opacity: 0.5 }}>{shortcut}</span>
          {when(
            shouldShowChevrons,
            <>
              <div style={{ opacity: subMenuItems == null ? 0 : 1 }}>
                <Icons.ExpansionArrowRight color='white' />
              </div>
              {subMenuItems == null ? null : (
                <RadixDropdownMenu.Portal>
                  <RadixDropdownSubcontent sideOffset={10} alignOffset={-6}>
                    {subMenuItems.map((child) => (
                      <DropdownItem
                        key={child.id}
                        shouldShowChevrons={false}
                        shouldShowCheckboxes={shouldShowCheckboxesForSubmenuItems}
                        shouldShowIcons={shouldShowIconsForSubmenuItems}
                        onSelect={child.onSelect}
                        label={child.label}
                        icon={child.icon}
                        checked={child.checked ?? null}
                        shortcut={child.shortcut ?? null}
                        subMenuItems={null}
                      />
                    ))}
                  </RadixDropdownSubcontent>
                </RadixDropdownMenu.Portal>
              )}
            </>,
          )}
        </div>
      </div>
    </ItemContainer>
  )
})

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
  const shouldShowChevrons = props.items.some((i) => i.subMenuItems != null)

  return (
    <RadixDropdownMenu.Root open={open} onOpenChange={onOpen}>
      <RadixDropdownMenu.Trigger style={{ background: 'none', border: 'none' }}>
        {props.opener(open)}
      </RadixDropdownMenu.Trigger>
      <RadixDropdownMenu.Portal>
        <RadixDropdownContent
          onKeyDown={stopPropagation}
          onEscapeKeyDown={onEscapeKeyDown}
          sideOffset={props.sideOffset}
          collisionPadding={{ top: -4 }}
          align='start'
          alignOffset={props.alignOffset}
        >
          {props.items.map((item) => (
            <DropdownItem
              key={item.id}
              shouldShowChevrons={shouldShowChevrons}
              shouldShowCheckboxes={shouldShowCheckboxes}
              shouldShowIcons={shouldShowIcons}
              onSelect={item.onSelect}
              label={item.label}
              icon={item.icon}
              checked={item.checked ?? null}
              shortcut={item.shortcut ?? null}
              subMenuItems={item.subMenuItems ?? null}
            />
          ))}
        </RadixDropdownContent>
      </RadixDropdownMenu.Portal>
    </RadixDropdownMenu.Root>
  )
})

export interface DropdownMenuContainerProps {
  opener: (open: boolean) => React.ReactNode
  contents: React.ReactNode
  sideOffset?: number
  alignOffset?: number
}

export const DropdownMenuContainer = React.memo<DropdownMenuContainerProps>((props) => {
  const stopPropagation = React.useCallback((e: React.KeyboardEvent) => {
    const hasModifiers = e.altKey || e.metaKey || e.shiftKey || e.ctrlKey
    if (!hasModifiers) {
      e.stopPropagation()
    }
  }, [])
  const onEscapeKeyDown = React.useCallback((e: KeyboardEvent) => e.stopPropagation(), [])

  const [open, onOpen] = React.useState(false)

  return (
    <RadixDropdownMenu.Root open={open} onOpenChange={onOpen}>
      <RadixDropdownMenu.Trigger style={{ background: 'none', border: 'none' }}>
        {props.opener(open)}
      </RadixDropdownMenu.Trigger>
      <RadixDropdownMenu.Portal>
        <RadixDropdownContent
          onKeyDown={stopPropagation}
          onEscapeKeyDown={onEscapeKeyDown}
          sideOffset={props.sideOffset}
          collisionPadding={{ top: -4 }}
          align='start'
          alignOffset={props.alignOffset}
        >
          {props.contents}
        </RadixDropdownContent>
      </RadixDropdownMenu.Portal>
    </RadixDropdownMenu.Root>
  )
})
