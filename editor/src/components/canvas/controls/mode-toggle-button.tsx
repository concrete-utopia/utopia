/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { css, jsx } from '@emotion/react'
import { useEditorState } from '../../editor/store/store-hook'
import { updateFormulaBarMode } from '../../editor/actions/action-creators'
import { betterReactMemo } from '../../../uuiui-deps'
import { useColorTheme } from '../../../uuiui'

export const ModeToggleButton = betterReactMemo('ModeToggleButton', () => {
  const colorTheme = useColorTheme()
  const selectedMode = useEditorState(
    (store) => store.editor.topmenu.formulaBarMode,
    'ModeToggleButton selectedMode',
  )
  const dispatch = useEditorState((store) => store.dispatch, 'ModeToggleButton dispatch')
  const toggleMode = React.useCallback(
    (mode: 'css' | 'content') => {
      dispatch([updateFormulaBarMode(mode)], 'everyone')
    },
    [dispatch],
  )

  const cssButtonOnClick = React.useCallback(() => toggleMode('css'), [toggleMode])
  const contentButtonOnClick = React.useCallback(() => toggleMode('content'), [toggleMode])
  return (
    <div
      style={{
        display: 'flex',
        gap: 2,
        alignItems: 'center',
        padding: '0 2px',
      }}
    >
      <Button
        style={{
          background: colorTheme.primary.value,
        }}
        selected={selectedMode === 'css'}
        width={30}
        onClick={cssButtonOnClick}
      >
        CSS
      </Button>
      <Button
        style={{
          background: colorTheme.inverted.neutralForeground.value,
        }}
        selected={selectedMode === 'content'}
        width={45}
        onClick={contentButtonOnClick}
      >
        Content
      </Button>
    </div>
  )
})

interface ButtonProps {
  width: number
  style: React.CSSProperties
  selected: boolean
  onClick: () => void
}

const Button: React.FunctionComponent<ButtonProps> = (props) => (
  <span
    css={{
      fontSize: 9,
      fontWeight: 700,
      borderRadius: 2,
      paddingLeft: 4,
      paddingRight: 4,
      width: props.selected ? props.width : 6,
      flexShrink: 0,
      flexGrow: 1,
      display: 'block',
      cursor: 'pointer',
      transition: 'width .2s linear',
      color: props.selected ? 'black' : 'transparent',
      transitionDelay: '.1s',
      overflow: 'hidden',
      '&:hover': {
        color: 'black',
        width: props.width,
        transition: 'all .1s linear',
        transitionDelay: '.1s',
      },
      ...props.style,
    }}
    onClick={props.onClick}
  >
    {props.children}
  </span>
)
