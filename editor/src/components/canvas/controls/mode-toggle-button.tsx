/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { css, jsx } from '@emotion/react'
import { useEditorState } from '../../editor/store/store-hook'
import { updateFormulaBarMode } from '../../editor/actions/action-creators'
import { AlternateColorThemeComponent, useColorTheme } from '../../../uuiui'

export const ModeToggleButton = React.memo(() => {
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
          background: colorTheme.bg2.value,
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

const Button: React.FunctionComponent<React.PropsWithChildren<ButtonProps>> = (props) => (
  <span
    style={{
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
      ...(props.style as any), // TODO Emotion and React 18 types don't like each other
    }}
    onClick={props.onClick}
  >
    {props.children}
  </span>
)
