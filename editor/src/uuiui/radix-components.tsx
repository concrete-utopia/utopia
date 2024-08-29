/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as RadixDropdownMenu from '@radix-ui/react-dropdown-menu'
import * as Select from '@radix-ui/react-select'
import { styled } from '@stitches/react'
import type { CSSProperties } from 'react'
import React from 'react'
import { colorTheme, UtopiaStyles, UtopiaTheme } from './styles/theme'
import { Icons, SmallerIcons } from './icons'
import { when } from '../utils/react-conditionals'
import { Icn, type IcnProps } from './icn'
import { forceNotNull } from '../core/shared/optional-utils'

export const RadixComponentsPortalId = 'radix-components-portal'

const RadixItemContainer = styled(RadixDropdownMenu.Item, {
  minWidth: 128,
  padding: '4px 6px',
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
  padding: 4,
  flexDirection: 'column',
  backgroundColor: colorTheme.contextMenuBackground.value,
  borderRadius: 6,
  display: 'grid',
  gridTemplateRows: '1fr auto',
})

type BaseDropdownMenuItem = {
  id: string
}

type RegularDropdownMenuItem = BaseDropdownMenuItem & {
  type: 'LABELED'
  shortcut?: string
  icon?: React.ReactNode
  checked?: boolean
  danger?: boolean
  label: React.ReactNode
  onSelect: () => void
  disabled?: boolean
}

export function regularDropdownMenuItem(
  props: Omit<RegularDropdownMenuItem, 'type'>,
): RegularDropdownMenuItem {
  return {
    type: 'LABELED',
    ...props,
  }
}

export function separatorDropdownMenuItem(id: string): SeparatorDropdownMenuItem {
  return {
    id: id,
    type: 'SEPARATOR',
  }
}

type SeparatorDropdownMenuItem = BaseDropdownMenuItem & {
  type: 'SEPARATOR'
}

function isSeparatorDropdownMenuItem(item: DropdownMenuItem): item is SeparatorDropdownMenuItem {
  return item.type === 'SEPARATOR'
}

export type DropdownMenuItem = RegularDropdownMenuItem | SeparatorDropdownMenuItem

export interface DropdownMenuProps {
  opener: (open: boolean) => React.ReactNode
  items: DropdownMenuItem[]
  align?: RadixDropdownMenu.DropdownMenuContentProps['align']
  sideOffset?: number
  alignOffset?: number
  onOpenChange?: (open: boolean) => void
  style?: CSSProperties
}

export const ItemContainerTestId = (id: string) => `item-container-${id}`

export const DropdownMenu = React.memo<DropdownMenuProps>((props) => {
  const stopPropagation = React.useCallback((e: React.KeyboardEvent) => {
    const hasModifiers = e.altKey || e.metaKey || e.shiftKey || e.ctrlKey
    if (!hasModifiers) {
      e.stopPropagation()
    }
  }, [])
  const onEscapeKeyDown = React.useCallback((e: KeyboardEvent) => e.stopPropagation(), [])

  const [open, setOpen] = React.useState(false)

  const shouldShowCheckboxes = props.items.some(
    (i) => !isSeparatorDropdownMenuItem(i) && i.checked != null,
  )
  const shouldShowIcons = props.items.some((i) => !isSeparatorDropdownMenuItem(i) && i.icon != null)

  const onOpenChange = React.useCallback(
    (isOpen: boolean) => {
      setOpen(isOpen)
      props.onOpenChange?.(isOpen)
    },
    [props, setOpen],
  )

  const radixPortalContainer = forceNotNull(
    'Should be able to find the radix portal.',
    document.getElementById(RadixComponentsPortalId),
  )

  return (
    <RadixDropdownMenu.Root open={open} onOpenChange={onOpenChange}>
      <RadixDropdownMenu.Trigger style={{ background: 'none', border: 'none', padding: 0 }}>
        {props.opener(open)}
      </RadixDropdownMenu.Trigger>
      <RadixDropdownMenu.Portal container={radixPortalContainer}>
        <RadixDropdownContent
          onKeyDown={stopPropagation}
          onEscapeKeyDown={onEscapeKeyDown}
          sideOffset={props.sideOffset}
          collisionPadding={{ top: -4 }}
          align={props.align ?? 'start'}
          alignOffset={props.alignOffset}
          style={{
            ...props.style,
            boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
            zIndex: 999999,
          }}
        >
          {props.items.map((item) => {
            if (isSeparatorDropdownMenuItem(item)) {
              return <Separator key={item.id} />
            }
            return (
              <RadixItemContainer
                data-testid={ItemContainerTestId(item.id)}
                key={item.id}
                onSelect={item.onSelect}
                css={{
                  ':hover': {
                    backgroundColor: item.danger
                      ? colorTheme.errorForeground.value
                      : item.disabled
                      ? 'inherit'
                      : undefined,
                  },
                  cursor: item.disabled ? 'default' : undefined,
                }}
              >
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
            )
          })}
        </RadixDropdownContent>
      </RadixDropdownMenu.Portal>
    </RadixDropdownMenu.Root>
  )
})

const Separator = React.memo(() => {
  return (
    <RadixItemContainer
      css={{
        ':hover': {
          backgroundColor: 'inherit',
        },
        cursor: 'default',
      }}
    >
      <div
        style={{
          width: '100%',
          height: 1,
          backgroundColor: colorTheme.fg4.value,
        }}
      />
    </RadixItemContainer>
  )
})
Separator.displayName = 'Separator'

type RegularRadixSelectOption = {
  type: 'REGULAR'
  value: string
  label: string
  icon?: IcnProps
  placeholder?: boolean
}

export function regularRadixSelectOption(
  params: Omit<RegularRadixSelectOption, 'type'>,
): RegularRadixSelectOption {
  return {
    type: 'REGULAR',
    ...params,
  }
}

type Separator = {
  type: 'SEPARATOR'
}

export function separatorRadixSelectOption(): Separator {
  return {
    type: 'SEPARATOR',
  }
}

export type RadixSelectOption = RegularRadixSelectOption | Separator

export const RadixSelect = React.memo(
  (props: {
    id: string
    value: RegularRadixSelectOption | null
    options: RadixSelectOption[]
    style?: CSSProperties
    onValueChange?: (value: string) => void
  }) => {
    const stopPropagation = React.useCallback((e: React.KeyboardEvent) => {
      e.stopPropagation()
    }, [])

    return (
      <Select.Root value={props.value?.value} onValueChange={props.onValueChange}>
        <Select.Trigger
          style={{
            background: 'transparent',
            display: 'flex',
            alignItems: 'center',
            gap: 8,
            fontSize: 11,
            height: 22,
            color: props.value?.placeholder ? colorTheme.fg6.value : colorTheme.fg1.value,
            fontFamily: 'utopian-inter',
            border: 'none',
            borderRadius: UtopiaTheme.inputBorderRadius,
            ...props.style,
          }}
          css={{
            boxShadow: 'none',
            ':hover': {
              boxShadow: `inset 0px 0px 0px 1px ${colorTheme.fg7.value}`,
              justifyContent: 'space-between',
            },
          }}
        >
          <Select.Value placeholder={props.value?.label} />
          <Select.Icon style={{ width: 12, height: 12 }}>
            <SmallerIcons.ExpansionArrowDown />
          </Select.Icon>
        </Select.Trigger>
        <Select.Portal>
          <Select.Content
            onKeyDown={stopPropagation}
            style={{
              background: colorTheme.black.value,
              padding: 4,
              margin: '0 4px',
              borderRadius: 6,
              color: 'white',
              boxShadow: UtopiaStyles.shadowStyles.high.boxShadow,
            }}
          >
            <Select.ScrollUpButton>
              <Icons.ExpansionArrow color='on-highlight-main' />
            </Select.ScrollUpButton>
            <Select.Viewport
              style={{
                display: 'flex',
                flexDirection: 'column',
                gap: 2,
              }}
            >
              {props.options.map((option, index) => {
                if (option.type === 'SEPARATOR') {
                  return (
                    <Select.Separator
                      key={`select-option-${props.id}-${index}`}
                      style={{
                        borderTop: '1px solid #393d49',
                        width: '100%',
                      }}
                    />
                  )
                }

                const label = `${option.label.charAt(0).toUpperCase()}${option.label.slice(1)}`
                return (
                  <Select.Item
                    key={`select-option-${props.id}-${index}`}
                    value={option.value}
                    style={{
                      padding: '3px 4px',
                      display: 'flex',
                      alignItems: 'center',
                      gap: 4,
                      fontSize: 11,
                    }}
                    css={{
                      ':hover': {
                        backgroundColor: colorTheme.primary.value,
                        borderRadius: 4,
                      },
                    }}
                  >
                    {props.value?.value === option.value ? (
                      <Icons.Checkmark color='white' />
                    ) : (
                      <div style={{ width: 16, height: 16 }} />
                    )}
                    {option.icon != null ? (
                      <Icn {...option.icon} width={16} height={16} color='white' />
                    ) : null}
                    <Select.ItemText>{label}</Select.ItemText>
                  </Select.Item>
                )
              })}
            </Select.Viewport>
            <Select.ScrollDownButton>
              <Icons.ExpansionArrowDown color='white' />
            </Select.ScrollDownButton>
          </Select.Content>
        </Select.Portal>
      </Select.Root>
    )
  },
)
RadixSelect.displayName = 'RadixSelect'
