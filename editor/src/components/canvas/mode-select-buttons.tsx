/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { useColorTheme, FlexRow, UtopiaStyles } from '../../uuiui'
import { switchEditorMode } from '../editor/actions/action-creators'
import { EditorModes, isLiveMode, isSelectLiteMode, isSelectMode } from '../editor/editor-modes'
import { useEditorState } from '../editor/store/store-hook'

interface ModeSelectButtonProps {
  selected: boolean
  title: string
  onMouseDown: () => void
}

const ModeSelectButton = React.memo((props: ModeSelectButtonProps) => {
  const colorTheme = useColorTheme()
  return props.selected ? (
    <div
      // this is the selected variant. No hover effects on this one
      style={{
        padding: '2px 4px',
        background: colorTheme.secondaryBackground.value,
        color: colorTheme.primary.value,
        borderRadius: 2,
      }}
      onMouseDown={props.onMouseDown}
    >
      {props.title}
    </div>
  ) : (
    <div
      css={{
        padding: '2px 4px',
        background: 'transparent',
        borderRadius: 2,
        opacity: 0.6,
        transition: 'background .1s ease-in-out',
        '&:hover': {
          opacity: 0.8,
          background: colorTheme.secondaryBackground.value,
        },
        '&:active': {
          opacity: 1,
        },
      }}
      onMouseDown={props.onMouseDown}
    >
      {props.title}
    </div>
  )
})

export const ModeSelectButtons = React.memo(() => {
  const colorTheme = useColorTheme()
  const currentMode = useEditorState((store) => store.editor.mode, 'ModeSelectButtons editor.mode')
  const dispatch = useEditorState((store) => store.dispatch, 'ModeSelectButtons dispatch')

  const switchToSelectMode = React.useCallback(
    () => dispatch([switchEditorMode(EditorModes.selectMode())]),
    [dispatch],
  )
  const switchToSelectLiteMode = React.useCallback(
    () => dispatch([switchEditorMode(EditorModes.selectLiteMode())]),
    [dispatch],
  )
  const switchToLiveMode = React.useCallback(
    () => dispatch([switchEditorMode(EditorModes.liveMode())]),
    [dispatch],
  )

  return (
    <div
      style={{
        pointerEvents: 'initial',
        position: 'absolute',
        top: '0',
        right: '0',
      }}
    >
      <FlexRow style={{ paddingTop: 4, paddingRight: 4 }}>
        <div
          style={{
            height: 29,
            maxWidth: 150,
            display: 'flex',
            alignItems: 'center',
            paddingLeft: 4,
            paddingRight: 4,
            gap: 4,
            borderRadius: 4,
            background: colorTheme.bg0.value,
            boxShadow: UtopiaStyles.popup.boxShadow,
            cursor: 'pointer',
          }}
        >
          <ModeSelectButton
            selected={isSelectMode(currentMode)}
            title={'Edit ᵝ'}
            onMouseDown={switchToSelectMode}
          />
          <ModeSelectButton
            selected={isSelectLiteMode(currentMode)}
            title={'Select'}
            onMouseDown={switchToSelectLiteMode}
          />
          <ModeSelectButton
            selected={isLiveMode(currentMode)}
            title={'Live'}
            onMouseDown={switchToLiveMode}
          />
        </div>
      </FlexRow>
    </div>
  )
})
