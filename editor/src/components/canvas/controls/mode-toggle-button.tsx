/** @jsxRuntime classic */
/** @jsx jsx */
import * as React from 'react'
import { css, jsx, useTheme } from '@emotion/react'
import { useEditorState } from '../../editor/store/store-hook'
import { updateFormulaBarMode } from '../../editor/actions/action-creators'

export const ModeToggleButton = () => {
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
    <React.Fragment>
      <Button
        style={{
          background: '#00aaff',
        }}
        selected={selectedMode === 'css'}
        width={30}
        onClick={cssButtonOnClick}
      >
        CSS
      </Button>
      <Button
        style={{
          background: '#ffffffcc',
        }}
        selected={selectedMode === 'content'}
        width={45}
        onClick={contentButtonOnClick}
      >
        Content
      </Button>
    </React.Fragment>
  )
}

interface ButtonProps {
  width: number
  style: React.CSSProperties
  selected: boolean
  onClick: () => void
}

const Button: React.FunctionComponent<ButtonProps> = (props) => (
  <div
    css={{
      paddingLeft: 2,
      paddingRight: 2,
      display: 'block',
      cursor: 'pointer',
      transition: 'width .2s linear',
      width: props.selected ? props.width : 4,
      flexShrink: 0,
      flexGrow: 0,
      borderRadius: 2,
      overflow: 'hidden',
      color: props.selected ? '#000033' : 'transparent',
      '&:hover': {
        color: '#000033',
        width: props.width,
        transition: 'width .1s linear',
      },
      ...props.style,
    }}
    onClick={props.onClick}
  >
    {props.children}
  </div>
)
