/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import * as React from 'react'
import type { TooltipProps } from '../../uuiui'
import { UtopiaTheme } from '../../uuiui'
import {
  colorTheme,
  FlexColumn,
  FlexRow,
  Icn,
  LargerIcons,
  SquareButton,
  Tooltip as TooltipWithoutSpanFixme,
  useColorTheme,
  UtopiaStyles,
} from '../../uuiui'
import { Utils } from '../../uuiui-deps'
import CanvasActions from '../canvas/canvas-actions'
import { stopPropagation } from '../inspector/common/inspector-utils'
import type { EditorAction } from './action-types'
import {
  applyCommandsAction,
  openFloatingInsertMenu,
  resetCanvas,
  setPanelVisibility,
  setRightMenuTab,
  switchEditorMode,
  wrapInElement,
} from './actions/action-creators'
import { EditorModes } from './editor-modes'
import {
  useCheckInsertModeForElementType,
  useEnterDrawToInsertForButton,
  useEnterDrawToInsertForConditional,
  useEnterDrawToInsertForDiv,
  useEnterDrawToInsertForImage,
  useEnterTextEditMode,
} from './insert-callbacks'
import { useDispatch } from './store/dispatch-context'
import { RightMenuTab } from './store/editor-state'
import { Substores, useEditorState, useRefEditorState } from './store/store-hook'
import { togglePanel } from './actions/action-creators'
import { defaultTransparentViewElement } from './defaults'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import { useToolbarMode } from './canvas-toolbar-states'
import { when } from '../../utils/react-conditionals'
import { StrategyIndicator } from '../canvas/controls/select-mode/strategy-indicator'
import { toggleAbsolutePositioningCommands } from '../inspector/inspector-common'
import { useState } from 'react'

export const InsertMenuButtonTestId = 'insert-menu-button'
export const InsertConditionalButtonTestId = 'insert-mode-conditional'

export const CanvasToolbarId = 'canvas-toolbar'

export const CanvasToolbar = React.memo(() => {
  const dispatch = useDispatch()
  const theme = useColorTheme()

  const [forcedInsertMode, setForceInsertMode] = React.useState(false)

  const toggleInsertButtonClicked = React.useCallback((e: React.MouseEvent<Element>) => {
    e.stopPropagation()
    setForceInsertMode((value) => !value)
  }, [])

  React.useEffect(() => {
    const turnOffForcedInsertMode = () => {
      setForceInsertMode(false)
    }
    window.addEventListener('click', turnOffForcedInsertMode)
    return function cleanup() {
      window.removeEventListener('click', turnOffForcedInsertMode)
    }
  }, [])

  const canvasToolbarMode = useToolbarMode(forcedInsertMode)

  const editorStateRef = useRefEditorState((store) => store.editor)
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)

  const insertDivCallback = useEnterDrawToInsertForDiv()
  const insertImgCallback = useEnterDrawToInsertForImage()
  const insertTextCallback = useEnterTextEditMode()
  const insertButtonCallback = useEnterDrawToInsertForButton()
  const insertConditionalCallback = useEnterDrawToInsertForConditional()

  const insertMenuMode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.floatingInsertMenu.insertMenuMode,
    'CanvasToolbar insertMenuMode',
  )

  const openFloatingInsertMenuCallback = React.useCallback(() => {
    dispatch([
      openFloatingInsertMenu({
        insertMenuMode: 'insert',
        parentPath: null,
        indexPosition: null,
      }),
    ])
  }, [dispatch])
  const openFloatingConvertMenuCallback = React.useCallback(() => {
    dispatch([
      openFloatingInsertMenu({
        insertMenuMode: 'convert',
      }),
    ])
  }, [dispatch])
  const openFloatingWrapInMenuCallback = React.useCallback(() => {
    dispatch([
      openFloatingInsertMenu({
        insertMenuMode: 'wrap',
      }),
    ])
  }, [dispatch])

  const wrapInGroupCallback = React.useCallback(() => {
    dispatch([
      wrapInElement(selectedViewsRef.current, {
        element: defaultTransparentViewElement(
          generateUidWithExistingComponents(projectContentsRef.current),
        ),
        importsToAdd: {},
      }),
    ])
  }, [dispatch, selectedViewsRef, projectContentsRef])

  const toggleAbsolutePositioningCallback = React.useCallback(() => {
    const editorState = editorStateRef.current
    const commands = toggleAbsolutePositioningCommands(
      editorState.jsxMetadata,
      editorState.allElementProps,
      editorState.elementPathTree,
      editorState.selectedViews,
    )
    if (commands.length === 0) {
      return
    }
    dispatch([applyCommandsAction(commands)])
  }, [dispatch, editorStateRef])

  const clickSelectModeButton = React.useCallback(() => {
    dispatch([switchEditorMode(EditorModes.selectMode())])
  }, [dispatch])

  const insertMenuSelected = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.rightMenu.selectedTab === RightMenuTab.Insert,
    'CanvasToolbar insertMenuSelected',
  )

  const selectInsertMenuPane = React.useCallback(() => {
    let actions: Array<EditorAction> = []
    if (!insertMenuSelected) {
      actions.push(setPanelVisibility('rightmenu', true))
    }
    actions.push(setRightMenuTab(RightMenuTab.Insert))
    dispatch(actions)
  }, [dispatch, insertMenuSelected])

  const zoomLevel = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'CanvasToolbar zoomLevel',
  )

  const zoomIn = React.useCallback(
    () => dispatch([CanvasActions.zoom(Utils.increaseScale(zoomLevel))]),
    [dispatch, zoomLevel],
  )
  const zoomOut = React.useCallback(
    () => dispatch([CanvasActions.zoom(Utils.decreaseScale(zoomLevel))]),
    [dispatch, zoomLevel],
  )

  const zoom100pct = React.useCallback(() => dispatch([CanvasActions.zoom(1)]), [dispatch])

  const isLiveMode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode.type === 'live',
    'TopMenu isLiveMode',
  )
  const toggleLiveMode = React.useCallback(() => {
    if (isLiveMode) {
      dispatch([switchEditorMode(EditorModes.selectMode())])
    } else {
      dispatch([switchEditorMode(EditorModes.liveMode())])
    }
  }, [dispatch, isLiveMode])

  const resetCanvasCallback = React.useCallback(() => {
    dispatch([resetCanvas()])
  }, [dispatch])

  const inspectorInvisible = useEditorState(
    Substores.restOfEditor,
    (store) => !store.editor.rightMenu.expanded,
    'SettingsPanel inspector.minimized',
  )
  const toggleInspectorVisible = React.useCallback(() => {
    dispatch([togglePanel('rightmenu')])
  }, [dispatch])

  const navigatorInvisible = useEditorState(
    Substores.restOfEditor,
    (store) => !store.editor.leftMenu.expanded,
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

  return (
    <div
      style={{
        display: 'flex',
        gap: 10,
        flexDirection: 'row',
      }}
    >
      {navigatorInvisible ? (
        <div
          style={{
            backgroundColor: theme.inspectorBackground.value,
            borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
            overflow: 'hidden',
            boxShadow: UtopiaTheme.panelStyles.shadows.medium,
            pointerEvents: 'initial',
            display: 'flex',
            flexDirection: 'row',
            width: 32,
            height: 32,
          }}
        >
          <Tooltip title='Toggle Navigator (⌘⌥1)' placement='bottom'>
            <InsertModeButton
              iconType='navigator-larger'
              iconCategory='semantic'
              onClick={toggleNavigatorVisible}
            />
          </Tooltip>
        </div>
      ) : null}
      {editorInvisible ? (
        <div
          style={{
            backgroundColor: theme.inspectorBackground.value,
            borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
            overflow: 'hidden',
            boxShadow: UtopiaTheme.panelStyles.shadows.medium,
            pointerEvents: 'initial',
            display: 'flex',
            flexDirection: 'row',
            width: 32,
            height: 32,
          }}
        >
          <Tooltip title='Toggle Code Editor (⌘.)' placement='bottom'>
            <InsertModeButton
              iconType='codymccodeface-larger'
              iconCategory='semantic'
              onClick={toggleCodeEditorVisible}
            />
          </Tooltip>
        </div>
      ) : null}
      {inspectorInvisible ? (
        <div
          style={{
            backgroundColor: theme.inspectorBackground.value,
            borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
            overflow: 'hidden',
            boxShadow: UtopiaTheme.panelStyles.shadows.medium,
            pointerEvents: 'initial',
            display: 'flex',
            flexDirection: 'row',
            width: 32,
            height: 32,
          }}
        >
          <Tooltip title='Toggle Inspector (⌘⌥2)' placement='bottom'>
            <InsertModeButton
              iconType='inspector-larger'
              iconCategory='semantic'
              onClick={toggleInspectorVisible}
            />
          </Tooltip>
        </div>
      ) : null}
      <FlexColumn style={{ alignItems: 'start' }}>
        <div
          id={CanvasToolbarId}
          style={{
            backgroundColor: theme.inspectorBackground.value,
            borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
            overflow: 'hidden',
            boxShadow: UtopiaTheme.panelStyles.shadows.medium,
            pointerEvents: 'initial',
            display: 'flex',
            flexDirection: 'row',
            padding: '0 8px',
          }}
        >
          <Tooltip title='Edit' placement='bottom'>
            <InsertModeButton
              iconType='pointer'
              iconCategory='tools'
              primary={canvasToolbarMode.primary === 'edit'}
              onClick={clickSelectModeButton}
            />
          </Tooltip>
          <Tooltip title='Insert or Edit Text' placement='bottom'>
            <InsertModeButton
              iconType='pure-text'
              primary={canvasToolbarMode.primary === 'text'}
              onClick={insertTextCallback}
            />
          </Tooltip>
          <Tooltip title='Insert...' placement='bottom'>
            <InsertModeButton
              testid={InsertMenuButtonTestId}
              iconType='plusbutton-larger'
              iconCategory='semantic'
              primary={canvasToolbarMode.primary === 'insert'}
              onClick={toggleInsertButtonClicked}
            />
          </Tooltip>
          <Tooltip title='Toggle Live Mode' placement='bottom'>
            <InsertModeButton
              iconType='playbutton'
              iconCategory='semantic'
              primary={canvasToolbarMode.primary === 'play'}
              onClick={toggleLiveMode}
              keepActiveInLiveMode
            />
          </Tooltip>
          <Separator />
          <Tooltip title='Zoom to 100%' placement='bottom'>
            <SquareButton
              highlight
              style={{
                textAlign: 'center',
                width: 'min-content',
                minWidth: 32,
                height: 32,
                padding: '0 8px',
              }}
              css={{
                '&:hover': {
                  color: colorTheme.dynamicBlue.value,
                },
              }}
              onClick={zoom100pct}
            >
              {zoomLevel * 100}%
            </SquareButton>
          </Tooltip>
          <Tooltip title='Reset Canvas' placement='bottom'>
            <InsertModeButton
              iconType='refresh'
              iconCategory='semantic'
              onClick={resetCanvasCallback}
            />
          </Tooltip>
        </div>

        {/* Edit Mode submenus */}
        {when(
          canvasToolbarMode.primary === 'edit' &&
            canvasToolbarMode.secondary === 'nothing-selected',
          null, // show nothing!
        )}
        {when(
          canvasToolbarMode.primary === 'edit' && canvasToolbarMode.secondary === 'selected',
          <FlexRow
            data-testid='canvas-toolbar-submenu'
            style={{
              alignItems: 'start',
              marginLeft: 15,
              padding: '0 8px',
              height: 32,
              overflow: 'hidden',
              backgroundColor: colorTheme.bg2.value,
              borderRadius: '0px 10px 10px 10px',
              boxShadow: UtopiaTheme.panelStyles.shadows.medium,
              pointerEvents: 'initial',
              zIndex: -1, // it sits below the main menu row, but we want the main menu's shadow to cast over this one
            }}
          >
            <Tooltip title='Wrap selection in Group (⌘G)' placement='bottom'>
              <InsertModeButton iconType='group-open' onClick={wrapInGroupCallback} />
            </Tooltip>
            <Tooltip title='Wrap selection in an element' placement='bottom'>
              <InsertModeButton
                iconType='designtool-larger'
                iconCategory='semantic'
                primary={insertMenuMode === 'wrap'}
                onClick={openFloatingWrapInMenuCallback}
              />
            </Tooltip>
            <Tooltip title='Converts an element or component into another (C)' placement='bottom'>
              <InsertModeButton
                iconType='convertobject'
                iconCategory='semantic'
                primary={insertMenuMode === 'convert'}
                onClick={openFloatingConvertMenuCallback}
              />
            </Tooltip>
            <Tooltip
              title='Toggle between absolute and static positioning (X)' // help I need better copy
              placement='bottom'
            >
              <InsertModeButton
                iconType='position-absolute' // TODO this needs an icon!
                iconCategory='layout/systems'
                size={16}
                onClick={toggleAbsolutePositioningCallback}
              />
            </Tooltip>
          </FlexRow>,
        )}
        {when(
          canvasToolbarMode.primary === 'edit' && canvasToolbarMode.secondary === 'strategy-active',
          <StrategyIndicator />,
        )}
        {/* Insert Mode */}
        {canvasToolbarMode.primary === 'insert' && ( // sorry Sean but I need to narrow the type of canvasToolbarMode.primary here :)
          <FlexRow
            data-testid='canvas-toolbar-submenu'
            style={{
              alignItems: 'start',
              marginLeft: 15,
              padding: '0 8px',
              height: 32,
              overflow: 'hidden',
              backgroundColor: colorTheme.bg2.value,
              borderRadius: '0px 10px 10px 10px',
              boxShadow: UtopiaTheme.panelStyles.shadows.medium,
              pointerEvents: 'initial',
              zIndex: -1, // it sits below the main menu row, but we want the main menu's shadow to cast over this one
            }}
          >
            <Tooltip title='Insert div' placement='bottom'>
              <InsertModeButton
                iconType='view'
                primary={canvasToolbarMode.secondary.divInsertionActive}
                onClick={insertDivCallback}
              />
            </Tooltip>
            <Tooltip title='Insert image' placement='bottom'>
              <InsertModeButton
                iconType='image'
                primary={canvasToolbarMode.secondary.imageInsertionActive}
                onClick={insertImgCallback}
              />
            </Tooltip>
            <Tooltip title='Insert button' placement='bottom'>
              <InsertModeButton
                iconType='clickable'
                primary={canvasToolbarMode.secondary.buttonInsertionActive}
                onClick={insertButtonCallback}
              />
            </Tooltip>
            <Tooltip title='Insert conditional' placement='bottom'>
              <InsertModeButton
                testid={InsertConditionalButtonTestId}
                iconType='conditional'
                primary={canvasToolbarMode.secondary.conditionalInsertionActive}
                onClick={insertConditionalCallback}
              />
            </Tooltip>
            <Tooltip title='Choose and insert a component' placement='bottom'>
              <InsertModeButton
                iconType='componentinstance'
                primary={canvasToolbarMode.secondary.floatingInsertMenuOpen}
                onClick={openFloatingInsertMenuCallback}
              />
            </Tooltip>
            <Tooltip title='Open insert menu' placement='bottom'>
              <InsertModeButton
                iconType='dotdotdot'
                iconCategory='semantic'
                primary={canvasToolbarMode.secondary.insertSidebarOpen}
                onClick={selectInsertMenuPane}
              />
            </Tooltip>
          </FlexRow>
        )}
      </FlexColumn>
    </div>
  )
})

interface InsertModeButtonProps {
  iconType: string
  iconCategory?: string
  primary?: boolean
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
      style={{ ...props.style, height: 32, width: 32 }}
      primary={primary}
      highlight
      onClick={onClickHandler}
      disabled={canvasInLiveMode && !keepActiveInLiveMode}
      onMouseEnter={setIsHoveredTrue}
      onMouseLeave={setIsHoveredFalse}
    >
      <Icn
        category={iconCategory}
        type={props.iconType}
        color={
          isHovered && !props.primary ? 'primary' : props.primary ? 'on-highlight-main' : 'main'
        }
        width={props.size ?? 18}
        height={props.size ?? 18}
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

const Separator = React.memo((props) => {
  return (
    <div
      style={{
        width: 1,
        height: 16,
        alignSelf: 'center',
        margin: '0 8px',
        backgroundColor: colorTheme.seperator.value,
      }}
    ></div>
  )
})
