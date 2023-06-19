import * as React from 'react'
import { when } from '../../../../utils/react-conditionals'
import { FlexColumn, UtopiaStyles, colorTheme } from '../../../../uuiui'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { stopPropagation } from '../../../inspector/common/inspector-utils'

export const PostActionMenu = React.memo(() => {
  const postActionSessionType = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.postActionInteractionType,
    'post action on',
  )

  return (
    <>
      {when(
        postActionSessionType != null,
        <div
          style={{
            pointerEvents: 'initial',
            position: 'absolute',
            top: 4,
            right: 4,
            fontSize: 9,
          }}
          onMouseDown={stopPropagation}
          onClick={stopPropagation}
        >
          <FlexColumn
            style={{
              minHeight: 84,
              display: 'flex',
              alignItems: 'stretch',
              padding: 4,
              gap: 4,
              borderRadius: 4,
              background: colorTheme.bg0.value,
              boxShadow: UtopiaStyles.popup.boxShadow,
            }}
          >
            {postActionSessionType}
            <div
              style={{
                alignSelf: 'center',
                marginTop: 'auto',
                color: colorTheme.fg5.value,
              }}
            >
              Press{' '}
              <span
                style={{ padding: 2, borderRadius: 2, border: `1px solid ${colorTheme.fg8.value}` }}
              >
                Tab
              </span>{' '}
              to switch
            </div>
          </FlexColumn>
        </div>,
      )}
    </>
  )
})

const KeyIndicator = ({ keyNumber }: { keyNumber: number }) => {
  const height = 12
  const width = 12
  return (
    <div
      style={{
        width: width,
        height: height,
        marginRight: 5,
        border: `1px solid ${colorTheme.fg4.value}`,
        borderRadius: 3,
        display: 'flex',
        flexDirection: 'row',
        justifyContent: 'center',
        alignItems: 'center',
      }}
    >
      <span
        style={{
          fontWeight: 700,
          color: colorTheme.fg4.value,
          fontSize: '8px',
        }}
      >
        {keyNumber}
      </span>
    </div>
  )
}
