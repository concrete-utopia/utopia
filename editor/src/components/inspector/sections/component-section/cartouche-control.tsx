import React from 'react'
import { when } from '../../../../utils/react-conditionals'
import { FlexRow, Icn, Tooltip, colorTheme } from '../../../../uuiui'
import { stopPropagation } from '../../common/inspector-utils'

interface IdentifierExpressionCartoucheControlProps {
  contents: string
  icon: React.ReactChild
  matchType: 'full' | 'partial' | 'none'
  onOpenDataPicker: () => void
  onDeleteCartouche: () => void
  safeToDelete: boolean
  testId: string
}
export const IdentifierExpressionCartoucheControl = React.memo(
  (props: IdentifierExpressionCartoucheControlProps) => {
    const { onDeleteCartouche, testId, safeToDelete } = props
    const onDelete = React.useCallback<React.MouseEventHandler<HTMLDivElement>>(
      (e) => {
        stopPropagation(e)
        onDeleteCartouche()
      },
      [onDeleteCartouche],
    )

    return (
      <FlexRow
        style={{
          cursor: 'pointer',
          fontSize: 10,
          color:
            props.matchType === 'full'
              ? colorTheme.white.value
              : props.matchType === 'partial'
              ? colorTheme.primary.value
              : colorTheme.neutralForeground.value,
          backgroundColor:
            props.matchType === 'full'
              ? colorTheme.primary.value
              : props.matchType === 'partial'
              ? colorTheme.primary10.value
              : colorTheme.bg4.value,
          padding: '0px 4px',
          borderRadius: 4,
          height: 22,
          display: 'flex',
          flex: 1,
          gap: 2,
        }}
        onClick={props.onOpenDataPicker}
      >
        {props.icon}
        <Tooltip title={props.contents}>
          <div
            style={{
              flex: 1,
              /* Standard CSS ellipsis */
              whiteSpace: 'nowrap',
              overflow: 'hidden',
              textOverflow: 'ellipsis',

              /* Beginning of string */
              // direction: 'rtl',
              textAlign: 'left',
            }}
          >
            {props.contents}
            &lrm;
            {/* the &lrm; non-printing character is added to fix the punctuation marks disappearing because of direction: rtl */}
          </div>
        </Tooltip>
        {when(
          safeToDelete,
          <Icn
            category='semantic'
            type='cross-medium'
            color='on-highlight-main'
            width={16}
            height={16}
            data-testid={`delete-${testId}`}
            onClick={onDelete}
          />,
        )}
      </FlexRow>
    )
  },
)
