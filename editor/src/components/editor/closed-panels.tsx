/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React, { useState } from 'react'
import type { TooltipProps } from '../../uuiui'
import { UtopiaTheme, colorTheme } from '../../uuiui'
import {
  FlexColumn,
  Icn,
  SquareButton,
  Tooltip as TooltipWithoutSpanFixme,
  useColorTheme,
} from '../../uuiui'
import { useDispatch } from './store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from './store/store-hook'
import { togglePanel } from './actions/action-creators'
import { stopPropagation } from '../inspector/common/inspector-utils'
import { setFocus } from '../common/actions'

export const ClosedPanels = React.memo(() => {
  const dispatch = useDispatch()
  const theme = useColorTheme()

  const inspectorInvisible = useEditorState(
    Substores.restOfEditor,
    (store) => !store.editor.rightMenu.visible,
    'SettingsPanel inspector.minimized',
  )

  const toggleInspectorVisible = React.useCallback(() => {
    dispatch([togglePanel('rightmenu')])
  }, [dispatch])

  const navigatorInvisible = useEditorState(
    Substores.restOfEditor,
    (store) => !store.editor.leftMenu.visible,
    'SettingsPanel navigator.minimised',
  )

  const toggleNavigatorVisible = React.useCallback(() => {
    dispatch([togglePanel('leftmenu')])
  }, [dispatch])

  const editorInvisible = useEditorState(
    Substores.restOfEditor,
    (store) => !store.editor.interfaceDesigner.codePaneVisible,
    'SettingsPanel navigator.minimised',
  )

  const toggleCodeEditorVisible = React.useCallback(
    () => dispatch([togglePanel('codeEditor')]),
    [dispatch],
  )

  const focusCanvasOnMouseDown = React.useCallback(
    (event: React.MouseEvent<Element>) => {
      stopPropagation(event)
      dispatch([setFocus('canvas')], 'everyone')
    },
    [dispatch],
  )

  const [isVisible, setIsVisible] = useState(false)
  const setIsVisibleTrue = React.useCallback(() => {
    setIsVisible(true)
  }, [])

  const setIsVisibleFalse = React.useCallback(() => {
    setIsVisible(false)
  }, [])

  return (
    <FlexColumn
      // Mouse events should never go through this component.
      onClick={stopPropagation}
      onMouseDown={focusCanvasOnMouseDown}
      onMouseUp={stopPropagation}
      style={{
        gap: 10,
        height: 300,
        width: 32,
        zIndex: 100,
        // background: isVisible ? 'pink' : 'yellow',
      }}
      onMouseEnter={setIsVisibleTrue}
      onMouseLeave={setIsVisibleFalse}
    >
      {navigatorInvisible ? (
        <Tooltip title='Toggle Navigator (⌘⌥1)' placement='bottom'>
          <InsertModeButton
            iconType='navigator'
            onClick={toggleNavigatorVisible}
            visible={isVisible}
          />
        </Tooltip>
      ) : null}
      {editorInvisible ? (
        <Tooltip title='Toggle Code Editor (⌘.)' placement='bottom'>
          <InsertModeButton iconType='cody' onClick={toggleCodeEditorVisible} visible={isVisible} />
        </Tooltip>
      ) : null}
      {inspectorInvisible ? (
        <Tooltip title='Toggle Inspector (⌘⌥2)' placement='bottom'>
          <InsertModeButton
            iconType='inspector'
            onClick={toggleInspectorVisible}
            visible={isVisible}
          />
        </Tooltip>
      ) : null}
    </FlexColumn>
  )
})

interface InsertModeButtonProps {
  iconType: string
  visible?: boolean
  onClick: (event: React.MouseEvent<Element>) => void
}
const InsertModeButton = React.memo((props: InsertModeButtonProps) => {
  const [isHovered, setIsHovered] = useState(false)
  const setIsHoveredTrue = React.useCallback(() => {
    setIsHovered(true)
  }, [])

  const setIsHoveredFalse = React.useCallback(() => {
    setIsHovered(false)
  }, [])

  return (
    <SquareButton
      style={{
        backgroundColor: props.visible ? colorTheme.bg1.value : 'transparent',
        overflow: 'hidden',
        boxShadow: UtopiaTheme.panelStyles.shadows.medium,
        pointerEvents: 'initial',
        display: 'flex',
        flexDirection: 'row',
        width: 32,
        height: 32,
        borderRadius: 32,
      }}
      onClick={props.onClick}
      onMouseEnter={setIsHoveredTrue}
      onMouseLeave={setIsHoveredFalse}
    >
      <Icn
        category='semantic'
        type={props.iconType}
        width={22}
        height={22}
        color={isHovered ? 'dynamic' : 'main'}
      />
    </SquareButton>
  )
})

const Tooltip = (props: TooltipProps) => {
  return (
    <TooltipWithoutSpanFixme {...props}>
      {/* TODO why do we need to wrap the children in a span? */}
      <span>{props.children}</span>
    </TooltipWithoutSpanFixme>
  )
}
