import React from 'react'
import {
  FlexRow,
  Icn,
  Tooltip,
  UtopiaStyles,
  createUtopiColor,
  useColorTheme,
} from '../../../../uuiui'
import { when } from '../../../../utils/react-conditionals'

export interface HoverHandlers {
  onMouseEnter: (e: React.MouseEvent) => void
  onMouseLeave: (e: React.MouseEvent) => void
}

export type CartoucheUIProps = React.PropsWithChildren<{
  tooltip?: string | null
  source: 'internal' | 'external' | 'literal'
  role: 'selection' | 'information' | 'folder'
  datatype: 'renderable' | 'boolean' | 'array' | 'object'
  inverted: boolean
  selected: boolean
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
      inverted,
      selected,
      role,
      datatype,
      onHover,
      preview = false,
    } = props

    const colorTheme = useColorTheme()

    const cartoucheIconColorToUse = source === 'external' ? 'green' : 'dynamic'

    const cartoucheIconColor = inverted
      ? 'on-highlight-main'
      : source === 'literal'
      ? 'secondary'
      : cartoucheIconColorToUse

    const borderColor = inverted
      ? colorTheme.white.value
      : role === 'folder' && !selected
      ? colorTheme.verySubduedForeground.value
      : source === 'external'
      ? colorTheme.green.value
      : colorTheme.selectionBlue.value

    const primaryForegoundColorToUse =
      source === 'external' ? colorTheme.green.value : colorTheme.dynamicBlue.value

    const primaryBackgroundColorToUse =
      source === 'external' ? colorTheme.green10.value : colorTheme.selectionBlue10.value

    const foregroundColor = inverted
      ? colorTheme.white.value
      : source === 'literal' || role === 'information' || role === 'folder'
      ? colorTheme.neutralForeground.value
      : primaryForegoundColorToUse

    const backgroundColor =
      role === 'information' || role === 'folder'
        ? colorTheme.neutralBackground.value
        : source === 'literal'
        ? colorTheme.fg0Opacity10.value
        : primaryBackgroundColorToUse

    const wrappedOnClick = useStopPropagation(onClick)
    const wrappedOnDoubleClick = useStopPropagation(onDoubleClick)
    const wrappedOnDelete = useStopPropagation(onDelete)

    const shouldShowBorder = selected || role === 'folder'

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
              color: foregroundColor,
              backgroundColor: backgroundColor,
              border: shouldShowBorder ? '1px solid ' + borderColor : '1px solid transparent',
              padding: '0px 6px 0 4px',
              borderRadius: 4,
              display: 'flex',
              gap: 4,
              opacity: preview ? 0.5 : 1,
              maxWidth: 160,
            }}
          >
            {source === 'literal' ? null : (
              <Icn
                category='navigator-element'
                type={dataTypeToIconType(datatype)}
                color={cartoucheIconColor}
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
                // direction: source === 'literal' ? 'ltr' : 'rtl', // TODO we need a better way to ellipsize the beginnign because rtl eats ' " marks
                textAlign: 'left',
                ...(role !== 'information' ? UtopiaStyles.fontStyles.monospaced : {}),
              }}
            >
              {children}
              {/* &lrm; */}
              {/* commented because it adds makes the flexcolumn in children too large */}
              {/* the &lrm; non-printing character is added to fix the punctuation marks disappearing because of direction: rtl */}
            </div>
            {/* {when(
              datatype === 'object' && role === 'folder',
              // a trailing ellipsis is added to indicate that the object can be traversed
              <span>…</span>,
            )} */}
            {when(
              onDelete != null,
              <Icn
                category='semantic'
                type='cross'
                color={cartoucheIconColor}
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
