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
import { usePropControlledStateV2 } from '../components/inspector/common/inspector-utils'

// Keep this in sync with the radix-components-portal div in index.html.
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
  badge?: string | null
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
  isOpen?: boolean
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

  const [open, setOpen] = usePropControlledStateV2(props.isOpen || false)

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
                  opacity: item.disabled ? 0.5 : 1,
                  ':hover': {
                    backgroundColor: item.danger
                      ? colorTheme.errorForeground.value
                      : item.disabled
                      ? 'inherit'
                      : undefined,
                    [`.item-badge-${item.id}`]: {
                      color: 'inherit',
                    },
                  },
                  cursor: item.disabled ? 'default' : undefined,
                  [`.item-badge-${item.id}`]: {
                    color: colorTheme.secondaryBlue.value,
                  },
                }}
              >
                <div
                  style={{
                    display: 'flex',
                    gap: 4,
                    alignItems: 'center',
                    width: '100%',
                  }}
                >
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
                  <span style={{ flex: 1 }}>{item.label}</span>
                  {when(
                    item.badge != null,
                    <span className={`item-badge-${item.id}`}>{item.badge}</span>,
                  )}
                </div>
                {when(item.shortcut != null, <span style={{ opacity: 0.5 }}>{item.shortcut}</span>)}
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
  label: string | ((isOpen: boolean, currentValue: string | null) => string)
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

function optionLabelToString(
  option: RegularRadixSelectOption | null,
  isOpen: boolean,
  currentValue: string | null,
): string | null {
  if (option == null) {
    return null
  }

  const label = typeof option.label === 'string' ? option.label : option.label(isOpen, currentValue)

  return `${label.charAt(0).toUpperCase()}${label.slice(1)}`
}

export const RadixSelect = React.memo(
  (props: {
    id: string
    value: RegularRadixSelectOption | null
    options: RadixSelectOption[]
    style?: CSSProperties
    contentStyle?: CSSProperties
    onValueChange?: (value: string) => void
    contentClassName?: string
    onOpenChange?: (open: boolean) => void
  }) => {
    const stopPropagation = React.useCallback((e: React.KeyboardEvent) => {
      e.stopPropagation()
    }, [])

    const { onOpenChange: propsOnOpenChange } = props

    const [isOpen, setIsOpen] = React.useState(false)
    const onOpenChange = React.useCallback(
      (open: boolean) => {
        setIsOpen(open)
        propsOnOpenChange?.(open)
      },
      [propsOnOpenChange],
    )

    const valueLabel = optionLabelToString(props.value ?? null, isOpen, props.value?.value ?? null)

    return (
      <Select.Root
        value={props.value?.value}
        onValueChange={props.onValueChange}
        onOpenChange={onOpenChange}
      >
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
          <Select.Value placeholder={valueLabel} />
          <Select.Icon style={{ width: 12, height: 12 }}>
            <SmallerIcons.ExpansionArrowDown />
          </Select.Icon>
        </Select.Trigger>
        <Select.Portal>
          <Select.Content
            className={props.contentClassName}
            onKeyDown={stopPropagation}
            style={{
              background: colorTheme.black.value,
              padding: 4,
              margin: '0 4px',
              borderRadius: 6,
              color: 'white',
              boxShadow: UtopiaStyles.shadowStyles.high.boxShadow,
              ...(props.contentStyle ?? {}),
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

                const label = optionLabelToString(option, isOpen, props.value?.value ?? null)
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
