/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React, { useState } from 'react'
import type { TooltipProps } from '../../uuiui'
import { UtopiaStyles, UtopiaTheme, colorTheme } from '../../uuiui'
import { FlexColumn, Icn, SquareButton, Tooltip as TooltipWithoutSpanFixme } from '../../uuiui'
import { useDispatch } from './store/dispatch-context'
import { Substores, useEditorState } from './store/store-hook'
import { togglePanel } from './actions/action-creators'
import { stopPropagation } from '../inspector/common/inspector-utils'
import { setFocus } from '../common/actions'
import { useAtom } from 'jotai'
import { GridPanelsStateAtom } from '../canvas/grid-panels-state'
import type { StoredLayout, StoredPanel } from '../canvas/stored-layout'
import { assertNever } from '../../core/shared/utils'
import { TestMenu } from '../titlebar/test-menu'
import { ToastRenderer } from './editor-component'

function getPanelColumn(side: 'left' | 'right', panels: StoredLayout): Array<StoredPanel> {
  switch (side) {
    case 'left':
      return [...panels[0].panels, ...panels[1].panels]
    case 'right':
      return [...panels[2].panels, ...panels[3].panels]
    default:
      assertNever(side)
  }
}

export const ClosedPanels = React.memo((props: { side: 'left' | 'right' }) => {
  const dispatch = useDispatch()
  const [panelState] = useAtom(GridPanelsStateAtom)
  const thisColumn = getPanelColumn(props.side, panelState)

  const inspectorInvisible = useEditorState(
    Substores.restOfEditor,
    (store) => {
      const theInspectorInvisible = !store.editor.rightMenu.visible
      const inspectorOnThisSide = thisColumn.some((panel) => panel.name === 'inspector')
      return theInspectorInvisible && inspectorOnThisSide
    },
    'SettingsPanel inspector.minimized',
  )

  const toggleInspectorVisible = React.useCallback(() => {
    dispatch([togglePanel('rightmenu')])
  }, [dispatch])

  const navigatorInvisible = useEditorState(
    Substores.restOfEditor,
    (store) => {
      const theNavigatorInvisible = !store.editor.leftMenu.visible
      const navigatorOnThisSide = thisColumn.some((panel) => panel.name === 'navigator')
      return theNavigatorInvisible && navigatorOnThisSide
    },
    'SettingsPanel navigator.minimised',
  )

  const toggleNavigatorVisible = React.useCallback(() => {
    dispatch([togglePanel('leftmenu')])
  }, [dispatch])

  const editorInvisible = useEditorState(
    Substores.restOfEditor,
    (store) => {
      const theEditorInvisible = !store.editor.interfaceDesigner.codePaneVisible
      const codeEditorOnThisSide = thisColumn.some((panel) => panel.name === 'code-editor')
      return theEditorInvisible && codeEditorOnThisSide
    },
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
      style={{
        alignItems: props.side === 'left' ? 'flex-start' : 'flex-end',
        justifyContent: 'space-between',
        height: '100%',
        width: 32,
      }}
    >
      <FlexColumn
        // Mouse events should never go through this component.
        onClick={stopPropagation}
        onMouseDown={focusCanvasOnMouseDown}
        onMouseUp={stopPropagation}
        style={{
          gap: 10,
          width: 32,
          pointerEvents: 'initial',
        }}
        onMouseEnter={setIsVisibleTrue}
        onMouseLeave={setIsVisibleFalse}
      >
        {navigatorInvisible ? (
          <Tooltip title='Toggle Navigator (⌘⌥1)' placement='bottom'>
            <ClosedPanelButton
              iconType='navigator'
              onClick={toggleNavigatorVisible}
              visible={isVisible}
            />
          </Tooltip>
        ) : null}
        {editorInvisible ? (
          <Tooltip title='Toggle Code Editor (⌘.)' placement='bottom'>
            <ClosedPanelButton
              iconType='cody'
              onClick={toggleCodeEditorVisible}
              visible={isVisible}
            />
          </Tooltip>
        ) : null}
        {inspectorInvisible ? (
          <Tooltip title='Toggle Inspector (⌘⌥2)' placement='bottom'>
            <ClosedPanelButton
              iconType='inspector'
              onClick={toggleInspectorVisible}
              visible={isVisible}
            />
          </Tooltip>
        ) : null}
      </FlexColumn>
      {props.side === 'left' ? <TestMenu /> : <ToastRenderer />}
    </FlexColumn>
  )
})
ClosedPanels.displayName = 'ClosedPanels'

interface ClosedPanelButtonProps {
  iconType: string
  visible?: boolean
  onClick: (event: React.MouseEvent<Element>) => void
}
const ClosedPanelButton = React.memo((props: ClosedPanelButtonProps) => {
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
        background: props.visible ? colorTheme.bg1.value : colorTheme.canvasBackground.value,
        overflow: 'hidden',
        boxShadow: `inset 0px 1px 3px 0px #ffffff10, ${UtopiaStyles.shadowStyles.low.boxShadow}`,
        pointerEvents: 'initial',
        display: 'flex',
        flexDirection: 'row',
        width: 28,
        height: 28,
        borderRadius: 28,
        transition: 'all .1s .1s ease-in-out',
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
