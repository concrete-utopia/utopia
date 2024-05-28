import React from 'react'
import { FlexRow, Icn, Icons, Tooltip, UtopiaStyles, useColorTheme } from '../../../../uuiui'
import { when } from '../../../../utils/react-conditionals'

export type CartoucheUIProps = React.PropsWithChildren<{
  tooltip: string
  source: 'internal' | 'external' | 'literal'
  role: 'selection' | 'information' | 'folder'
  inverted: boolean
  selected: boolean
  testId: string
  onDelete?: (e: React.MouseEvent) => void
  onClick?: (e: React.MouseEvent) => void
  onDoubleClick?: (e: React.MouseEvent) => void
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
      ? colorTheme.neutralForeground.value
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
        style={{
          minWidth: 0, // this ensures that the div can never expand the allocated grid space
        }}
        ref={ref}
      >
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
            height: 20,
            display: 'flex',
            flex: 1,
            gap: 4,
          }}
        >
          {source === 'literal' ? null : <Icons.NavigatorData color={cartoucheIconColor} />}
          <Tooltip title={tooltip}>
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
                ...UtopiaStyles.fontStyles.monospaced,
              }}
            >
              {children}
              &lrm;
              {/* the &lrm; non-printing character is added to fix the punctuation marks disappearing because of direction: rtl */}
            </div>
          </Tooltip>
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
      </div>
    )
  },
)

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
