/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import {
  colorTheme,
  SimpleFlexRow,
  SquareButton,
  UNSAFE_getIconURL,
  UtopiaStyles,
  UtopiaTheme,
} from '../../uuiui'
import { togglePanel } from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { RoundButton } from '../titlebar/buttons'

export const MiniTitleBar = React.memo(() => {
  const dispatch = useDispatch()
  const projectName = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return store.editor.projectName
    },
    'MiniTitleBar projectName',
  )

  const toggleLeftPanel = React.useCallback(() => {
    dispatch([togglePanel('leftmenu')])
  }, [dispatch])

  return (
    <SimpleFlexRow
      style={{
        paddingLeft: UtopiaTheme.layout.rowHorizontalPadding,
        paddingRight: UtopiaTheme.layout.rowHorizontalPadding,
        height: UtopiaTheme.layout.rowHeight.max,
        fontWeight: 600,
        gap: 6,
        borderBottom: `1px solid ${colorTheme.border0.value}`,
        marginBottom: 4,
      }}
    >
      <SquareButton
        onClick={toggleLeftPanel}
        style={{ borderRadius: 5 }}
        css={{ '&:hover': { opacity: 0.7 } }}
      >
        <img
          style={{
            userSelect: 'none',
            display: 'block',
            transform: 'scale(0.7)',
          }}
          width={30}
          src={UNSAFE_getIconURL('utopia-logo', 'black', 'special', 60, 47)}
        />
      </SquareButton>
      <div
        style={{
          color: colorTheme.fg0.value,
        }}
      >
        {projectName}
      </div>
    </SimpleFlexRow>
  )
})
