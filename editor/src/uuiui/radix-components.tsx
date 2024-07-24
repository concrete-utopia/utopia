import * as RadixDropdownMenu from '@radix-ui/react-dropdown-menu'
import { styled } from '@stitches/react'
import React from 'react'
import { colorTheme } from './styles/theme'
import { Icons } from './icons'
import { when } from '../utils/react-conditionals'

export const Separator = styled('div', {
  height: 1,
  margin: 5,
  backgroundColor: colorTheme.contextMenuSeparator.value,
})

const DropdownMenuItemContainer = styled('div', {
  minWidth: 128,
  padding: '6px 8px',
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
  align?: RadixDropdownMenu.DropdownMenuContentProps['align']
  sideOffset?: number
  alignOffset?: number
}

type OnSelect = (e: Event) => void

interface DropdownItemProps {
  itemId: string
  shouldShowCheckboxes: boolean
  shouldShowChevrons: boolean
  shouldShowIcons: boolean
  onSelect: OnSelect
  label: React.ReactNode
  icon: React.ReactNode | null
  checked: boolean | null
  shortcut: string | null
  subMenuItems: Omit<DropdownMenuItem, 'subMenuItems'>[] | null
}

const ItemContainer = React.memo<
  React.PropsWithChildren<{ isSubmenu: boolean; onSelect: OnSelect; testid: string }>
>(({ children, isSubmenu, onSelect, testid }) => {
  if (isSubmenu) {
    return (
      <RadixDropdownMenu.Sub>
        <RadixDropdownMenu.SubTrigger>
          <DropdownMenuItemContainer data-testid={testid}>{children}</DropdownMenuItemContainer>
        </RadixDropdownMenu.SubTrigger>
      </RadixDropdownMenu.Sub>
    )
  }

  return (
    <RadixDropdownMenu.Item onSelect={onSelect}>
      <DropdownMenuItemContainer data-testid={testid}>{children}</DropdownMenuItemContainer>
    </RadixDropdownMenu.Item>
  )
})

export const DropdownItem = React.memo<DropdownItemProps>((props) => {
  const {
    itemId,
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
    <ItemContainer
      testid={itemId}
      isSubmenu={!isNullOrEmptyArray(subMenuItems)}
      onSelect={onSelect}
    >
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
              <div style={{ opacity: isNullOrEmptyArray(subMenuItems) ? 0 : 1 }}>
                <Icons.ExpansionArrowRight color='white' />
              </div>
              {isNullOrEmptyArray(subMenuItems) ? null : (
                <RadixDropdownMenu.Portal>
                  <RadixDropdownSubcontent sideOffset={10} alignOffset={-6}>
                    {subMenuItems.map((child) => (
                      <DropdownItem
                        itemId={child.id}
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

export interface DropdownMenuItemListProps {
  items: DropdownMenuItem[]
}

export const DropdownMenuItemList = React.memo<DropdownMenuItemListProps>((props) => {
  const shouldShowCheckboxes = props.items.some((i) => i.checked != null)
  const shouldShowIcons = props.items.some((i) => i.icon != null)
  const shouldShowChevrons = props.items.some((i) => i.subMenuItems != null)

  return (
    <>
      {props.items.map((item) => (
        <DropdownItem
          itemId={item.id}
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
    </>
  )
})

export const DropdownMenu = React.memo<DropdownMenuProps>((props) => {
  const [open, onOpen] = React.useState(false)
  const stopPropagation = React.useCallback((e: React.KeyboardEvent) => {
    const hasModifiers = e.altKey || e.metaKey || e.shiftKey || e.ctrlKey
    if (!hasModifiers) {
      e.stopPropagation()
    }
  }, [])

  const opener = React.useMemo(() => props.opener(open), [open, props])

  const onEscapeKeyDown = React.useCallback((e: KeyboardEvent) => e.stopPropagation(), [])
  return (
    <RadixDropdownMenu.Root open={open} onOpenChange={onOpen}>
      <RadixDropdownMenu.Trigger style={{ background: 'none', border: 'none' }}>
        {opener}
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
          <DropdownMenuItemList items={props.items} />
        </RadixDropdownContent>
      </RadixDropdownMenu.Portal>
    </RadixDropdownMenu.Root>
  )
})

export interface DropdownMenuContainerProps {
  opener: (open: boolean) => React.ReactNode
  contents: React.ReactNode
  onClose?: () => void
  align?: RadixDropdownMenu.DropdownMenuContentProps['align']
  sideOffset?: number
  alignOffset?: number
  style?: React.CSSProperties
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
  const onOpenCallback = React.useCallback(
    (opened: boolean) => {
      if (props.onClose != null && opened === false) {
        props.onClose()
      }
      onOpen(opened)
    },
    [props],
  )

  return (
    <RadixDropdownMenu.Root open={open} onOpenChange={onOpenCallback}>
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
          style={{ ...props.style }}
        >
          {props.contents}
        </RadixDropdownContent>
      </RadixDropdownMenu.Portal>
    </RadixDropdownMenu.Root>
  )
})

function isNullOrEmptyArray<T>(ts: Array<T> | null | undefined): ts is null | undefined {
  return ts == null || ts.length === 0
}
