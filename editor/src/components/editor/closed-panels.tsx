/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React, { useState } from 'react'
import type { TooltipProps } from '../../uuiui'
import { UtopiaTheme } from '../../uuiui'
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

  return (
    <FlexColumn
      style={{ gap: 10, background: 'lime', flex: 1 }}
      // Mouse events should never go through this component.
      onClick={stopPropagation}
      onMouseDown={focusCanvasOnMouseDown}
      onMouseUp={stopPropagation}
    >
      {navigatorInvisible ? (
        <div
          style={{
            backgroundColor: theme.inspectorBackground.value,
            overflow: 'hidden',
            boxShadow: UtopiaTheme.panelStyles.shadows.medium,
            pointerEvents: 'initial',
            display: 'flex',
            flexDirection: 'row',
            width: 32,
            height: 32,
            borderRadius: 32,
          }}
        >
          <Tooltip title='Toggle Navigator (⌘⌥1)' placement='bottom'>
            <InsertModeButton
              iconType='navigator'
              iconCategory='semantic'
              size={22}
              onClick={toggleNavigatorVisible}
            />
          </Tooltip>
        </div>
      ) : null}
      {editorInvisible ? (
        <div
          style={{
            backgroundColor: theme.inspectorBackground.value,
            overflow: 'hidden',
            boxShadow: UtopiaTheme.panelStyles.shadows.medium,
            pointerEvents: 'initial',
            display: 'flex',
            flexDirection: 'row',
            width: 32,
            height: 32,
            borderRadius: 32,
          }}
        >
          <Tooltip title='Toggle Code Editor (⌘.)' placement='bottom'>
            <InsertModeButton
              iconType='cody'
              iconCategory='semantic'
              size={22}
              onClick={toggleCodeEditorVisible}
            />
          </Tooltip>
        </div>
      ) : null}
      {inspectorInvisible ? (
        <div
          style={{
            backgroundColor: theme.inspectorBackground.value,
            overflow: 'hidden',
            boxShadow: UtopiaTheme.panelStyles.shadows.medium,
            pointerEvents: 'initial',
            display: 'flex',
            flexDirection: 'row',
            width: 32,
            height: 32,
            borderRadius: 32,
          }}
        >
          <Tooltip title='Toggle Inspector (⌘⌥2)' placement='bottom'>
            <InsertModeButton
              iconType='inspector'
              iconCategory='semantic'
              size={22}
              onClick={toggleInspectorVisible}
            />
          </Tooltip>
        </div>
      ) : null}
    </FlexColumn>
  )
})

interface InsertModeButtonProps {
  iconType: string
  iconCategory?: string
  primary?: boolean
  secondary?: boolean
  keepActiveInLiveMode?: boolean
  style?: React.CSSProperties
  testid?: string
  onClick: (event: React.MouseEvent<Element>) => void
  size?: number
}
const InsertModeButton = React.memo((props: InsertModeButtonProps) => {
  const [isHovered, setIsHovered] = useState(false)
  const keepActiveInLiveMode = props.keepActiveInLiveMode ?? false
  const primary = props.primary ?? false
  const secondary = props.secondary ?? false
  const canvasInLiveMode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode.type === 'live',
    'CanvasToolbar canvasInLiveMode',
  )
  const iconCategory = props.iconCategory ?? 'element'
  const onClickHandler = React.useCallback(
    (event: React.MouseEvent<Element>) => {
      event.stopPropagation()
      props.onClick(event)
    },
    [props],
  )
  const setIsHoveredTrue = React.useCallback(() => {
    setIsHovered(true)
  }, [])

  const setIsHoveredFalse = React.useCallback(() => {
    setIsHovered(false)
  }, [])

  return (
    <SquareButton
      data-testid={props.testid}
      style={{ height: 32, width: 32, ...props.style }}
      primary={primary}
      spotlight={secondary}
      highlight
      onClick={props.onClick}
      disabled={canvasInLiveMode && !keepActiveInLiveMode}
      overriddenBackground={secondary ? 'transparent' : undefined}
      onMouseEnter={setIsHoveredTrue}
      onMouseLeave={setIsHoveredFalse}
    >
      <Icn
        category={iconCategory}
        type={props.iconType}
        width={props.size ?? 18}
        height={props.size ?? 18}
        testId={props.testid == null ? undefined : `${props.testid}-icon`}
        color={
          (isHovered && !props.primary) || props.secondary
            ? 'dynamic'
            : props.primary
            ? 'on-highlight-main'
            : 'main'
        }
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
