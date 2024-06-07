/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import type { IcnColor } from '../../../../uuiui'
import { FlexRow, Icn, Tooltip, UtopiaStyles, useColorTheme } from '../../../../uuiui'
import { when } from '../../../../utils/react-conditionals'
import { assertNever } from '../../../../core/shared/utils'

export interface HoverHandlers {
  onMouseEnter: (e: React.MouseEvent) => void
  onMouseLeave: (e: React.MouseEvent) => void
}

export type CartoucheDataType = 'renderable' | 'boolean' | 'array' | 'object' | 'unknown'

type CartoucheSource = 'internal' | 'external' | 'literal'
export type CartoucheHighlight = 'strong' | 'subtle'

export type CartoucheUIProps = React.PropsWithChildren<{
  tooltip?: string | null
  source: CartoucheSource
  role: 'selection' | 'information' | 'folder'
  datatype: CartoucheDataType
  selected: boolean
  highlight?: CartoucheHighlight | null
  testId: string
  preview?: boolean
  onDelete?: (e: React.MouseEvent) => void
  onClick?: (e: React.MouseEvent) => void
  onDoubleClick?: (e: React.MouseEvent) => void
  onHover?: HoverHandlers
}>

export const CartoucheUI = React.forwardRef(
  (props: CartoucheUIProps, ref: React.Ref<HTMLDivElement>) => {
    const {
      tooltip,
      onClick,
      onDoubleClick,
      onDelete,
      children,
      source,
      selected,
      highlight,
      role,
      datatype,
      onHover,
      preview = false,
    } = props

    const colors = useCartoucheColors(source, highlight ?? null)

    const wrappedOnClick = useStopPropagation(onClick)
    const wrappedOnDoubleClick = useStopPropagation(onDoubleClick)
    const wrappedOnDelete = useStopPropagation(onDelete)

    return (
      <div
        onClick={wrappedOnClick}
        onDoubleClick={wrappedOnDoubleClick}
        onMouseEnter={onHover?.onMouseEnter}
        onMouseLeave={onHover?.onMouseLeave}
        style={{
          minWidth: 0, // this ensures that the div can never expand the allocated grid space
        }}
        ref={ref}
      >
        <Tooltip title={tooltip ?? ''} disabled={tooltip == null}>
          <FlexRow
            style={{
              cursor: 'pointer',
              fontSize: 10,
              fontWeight: 400,
              padding: '0px 6px 0 4px',
              borderRadius: 4,
              height: 20,
              display: 'flex',
              flex: 1,
              gap: 4,
              opacity: preview ? 0.5 : 1,
            }}
            css={{
              color: selected || highlight === 'strong' ? colors.fg.selected : colors.fg.default,
              backgroundColor: selected ? colors.bg.selected : colors.bg.default,
              ':hover': {
                color: selected || highlight === 'strong' ? undefined : colors.fg.hovered,
                backgroundColor: selected ? undefined : colors.bg.hovered,
              },
            }}
          >
            {source === 'literal' ? null : (
              <Icn
                category='navigator-element'
                type={dataTypeToIconType(datatype)}
                color={
                  selected || highlight === 'strong' ? colors.icon.selected : colors.icon.default
                }
                width={12}
                height={12}
              />
            )}
            <div
              style={{
                flex: 1,
                paddingTop: 1,
                /* Standard CSS ellipsis */
                whiteSpace: 'nowrap',
                overflow: 'hidden',
                textOverflow: 'ellipsis',

                /* Beginning of string */
                direction: source === 'literal' ? 'ltr' : 'rtl', // TODO we need a better way to ellipsize the beginnign because rtl eats ' " marks
                textAlign: 'left',
                ...(role !== 'information' ? UtopiaStyles.fontStyles.monospaced : {}),
              }}
            >
              {children}
              &lrm;
              {/* the &lrm; non-printing character is added to fix the punctuation marks disappearing because of direction: rtl */}
            </div>
            {when(
              datatype === 'object' && role === 'folder',
              // a trailing ellipsis is added to indicate that the object can be traversed
              <span>…</span>,
            )}
            {when(
              onDelete != null,
              <Icn
                category='semantic'
                type='cross'
                color={
                  selected || highlight === 'strong' ? colors.icon.selected : colors.icon.default
                }
                width={12}
                height={12}
                data-testid={`delete-${props.testId}`}
                onClick={wrappedOnDelete}
              />,
            )}
          </FlexRow>
        </Tooltip>
      </div>
    )
  },
)

function dataTypeToIconType(dataType: CartoucheUIProps['datatype']): string {
  switch (dataType) {
    case 'renderable':
      return 'data'
    case 'boolean':
      return '👻'
    case 'array':
      return 'array'
    case 'object':
      return 'object'
    case 'unknown':
      return '👻'
  }
}

function useStopPropagation(callback: ((e: React.MouseEvent) => void) | undefined) {
  return React.useCallback(
    (e: React.MouseEvent) => {
      if (callback == null) {
        return
      }
      e.stopPropagation()
      callback(e)
    },
    [callback],
  )
}

type CartoucheStateColor<T> = {
  default: T
  hovered: T
  selected: T
}

function useCartoucheColors(source: CartoucheSource, highlight: CartoucheHighlight | null) {
  const colorTheme = useColorTheme()

  const colors: {
    fg: CartoucheStateColor<string>
    bg: CartoucheStateColor<string>
    icon: CartoucheStateColor<IcnColor>
  } = React.useMemo(() => {
    switch (source) {
      case 'external':
        return {
          fg: {
            default: colorTheme.green.value,
            hovered: colorTheme.green.value,
            selected: colorTheme.white.value,
          },
          bg: {
            default:
              highlight === 'strong' ? colorTheme.whiteOpacity20.value : colorTheme.green10.value,
            hovered:
              highlight === 'strong' ? colorTheme.whiteOpacity30.value : colorTheme.green20.value,
            selected: colorTheme.green.value,
          },
          icon: { default: 'green', hovered: 'green', selected: 'on-highlight-main' },
        }
      case 'internal':
        return {
          fg: {
            default: colorTheme.selectionBlue.value,
            hovered: colorTheme.selectionBlue.value,
            selected: colorTheme.white.value,
          },
          bg: {
            default:
              highlight === 'strong'
                ? colorTheme.whiteOpacity20.value
                : colorTheme.selectionBlue10.value,
            hovered:
              highlight === 'strong'
                ? colorTheme.whiteOpacity30.value
                : colorTheme.selectionBlue20.value,
            selected: colorTheme.selectionBlue.value,
          },
          icon: { default: 'dynamic', hovered: 'dynamic', selected: 'on-highlight-main' },
        }
      case 'literal':
        return {
          fg: {
            default: colorTheme.fg1.value,
            hovered: colorTheme.fg1.value,
            selected: colorTheme.white.value,
          },
          bg: {
            default:
              highlight === 'strong'
                ? colorTheme.whiteOpacity20.value
                : highlight === 'subtle'
                ? colorTheme.cartoucheLiteralHighlightDefault.value
                : colorTheme.bg5.value,
            hovered:
              highlight === 'strong'
                ? colorTheme.whiteOpacity30.value
                : highlight === 'subtle'
                ? colorTheme.cartoucheLiteralHighlightHovered.value
                : colorTheme.fg8.value,
            selected:
              highlight === 'subtle'
                ? colorTheme.cartoucheLiteralHighlightSelected.value
                : colorTheme.fg6.value,
          },
          icon: { default: 'secondary', hovered: 'secondary', selected: 'on-highlight-main' },
        }
      default:
        assertNever(source)
    }
  }, [source, colorTheme, highlight])

  return colors
}
