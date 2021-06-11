/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as React from 'react'
import { colorTheme, FlexRow } from '../../uuiui'
import { betterReactMemo } from '../../uuiui-deps'
import { switchEditorMode } from '../editor/actions/action-creators'
import { EditorModes, isLiveMode, isSelectLiteMode, isSelectMode } from '../editor/editor-modes'
import { useEditorState } from '../editor/store/store-hook'

interface ModeSelectButtonProps {
  selected: boolean
  title: string
  onMouseDown: () => void
}

const ModeSelectButton = betterReactMemo('ModeSelectButton', (props: ModeSelectButtonProps) => {
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

export const ModeSelectButtons = betterReactMemo('ModeSelectButtons', () => {
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
            boxShadow: 'inset 0px 0px 0px .5px hsl(0,0%,83%), 0px 2px 4px 0px hsla(0,0%,65%,50%)',
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
