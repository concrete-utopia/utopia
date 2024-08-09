/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React, { useState } from 'react'
import type { TooltipProps } from '../../uuiui'
import { Button, Icons, LargerIcons, SmallerIcons, Tile, UtopiaStyles, opacity } from '../../uuiui'
import { UtopiaTheme } from '../../uuiui'
import {
  colorTheme,
  FlexColumn,
  FlexRow,
  Icn,
  SquareButton,
  Tooltip as TooltipWithoutSpanFixme,
  useColorTheme,
} from '../../uuiui'
import CanvasActions from '../canvas/canvas-actions'
import {
  insertAsChildTarget,
  resetCanvas,
  setRightMenuTab,
  switchEditorMode,
  togglePanel,
} from './actions/action-creators'
import { EditorModes } from './editor-modes'
import {
  useEnterDrawToInsertForButton,
  useEnterDrawToInsertForConditional,
  useEnterDrawToInsertForDiv,
  useEnterDrawToInsertForImage,
  useEnterTextEditMode,
} from './insert-callbacks'
import { useDispatch } from './store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from './store/store-hook'
import { useToolbarMode } from './canvas-toolbar-states'
import { unless, when } from '../../utils/react-conditionals'
import { StrategyIndicator } from '../canvas/controls/select-mode/strategy-indicator'
import { stopPropagation } from '../inspector/common/inspector-utils'
import { useWrapInDiv } from './wrap-in-callbacks'
import { RemixNavigationBar } from './remix-navigation-bar'
import {
  type InsertableComponent,
  fragmentComponentInfo,
  fragmentElementsDescriptors,
  insertableComponentGroupFragment,
} from '../shared/project-components'
import { setFocus } from '../common/actions'
import type { CanvasStrategyIcon } from '../canvas/canvas-strategies/canvas-strategy-types'
import { isLoggedIn } from './action-types'
import type { EditorDispatch } from './action-types'
import type { InsertMenuItem } from '../canvas/ui/floating-insert-menu'
import { useGetInsertableComponents } from '../canvas/ui/floating-insert-menu'
import { RightMenuTab } from './store/editor-state'
import { useStatus, useThreads } from '../../../liveblocks.config'
import { useAllowedToEditProject, useIsMyProject } from './store/collaborative-editing'
import { useCanComment, useReadThreads } from '../../core/commenting/comment-hooks'
import { pluck } from '../../core/shared/array-utils'
import { MultiplayerWrapper } from '../../utils/multiplayer-wrapper'
import { ComponentPicker } from '../navigator/navigator-item/component-picker'
import { insertComponentPickerItem } from '../navigator/navigator-item/component-picker-context-menu'
import { useAtom } from 'jotai'
import { ActiveRemixSceneAtom } from '../canvas/remix/utopia-remix-root-component'
import * as EP from '../../core/shared/element-path'
import { NO_OP } from '../../core/shared/utils'
import type { DropdownMenuItem } from '../../uuiui/radix-components'
import { DropdownMenu, regularDropdownMenuItem } from '../../uuiui/radix-components'
import {
  TOGGLE_INSPECTOR,
  TOGGLE_CODE_EDITOR_SHORTCUT,
  TOGGLE_NAVIGATOR,
  keyToString,
  shortcutDetailsWithDefaults,
} from './shortcut-definitions'

export const InsertMenuButtonTestId = 'insert-menu-button'
export const InsertOrEditTextButtonTestId = 'insert-or-edit-text-button'
export const PlayModeButtonTestId = 'canvas-toolbar-play-mode'
export const CommentModeButtonTestId = (status: string) => `canvas-toolbar-comment-mode-${status}`
export const InsertConditionalButtonTestId = 'insert-mode-conditional'
export const CanvasToolbarId = 'canvas-toolbar'

export const CanvasToolbarSearchPortalId = 'canvas-toolbar-search-portal'

export const CanvasToolbarSearchTestID = 'canvas-toolbar-search'

export const CanvasToolbarSearch = React.memo(() => {
  const insertionActive = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode.type === 'insert' && store.editor.mode.subjects.length > 0,
    'TopMenu editorMode',
  )

  const dispatch = useDispatch()

  const target = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews[0],
    'CanvasToolbarSearch selectedView',
  )

  const insertionTarget = insertAsChildTarget()

  const allInsertableComponents = useGetInsertableComponents('insert').flatMap((group) => {
    return {
      label: group.label,
      options: group.options,
    }
  })

  const projectContentsRef = useRefEditorState((state) => state.editor.projectContents)
  const metadataRef = useRefEditorState((state) => state.editor.jsxMetadata)
  const elementPathTreesRef = useRefEditorState((state) => state.editor.elementPathTree)
  const allElementPropsRef = useRefEditorState((state) => state.editor.allElementProps)
  const propertyControlsInfoRef = useRefEditorState((state) => state.editor.propertyControlsInfo)

  const onItemClick = React.useCallback(
    (preferredChildToInsert: InsertableComponent) => (e: React.UIEvent) => {
      e.stopPropagation()
      e.preventDefault()

      insertComponentPickerItem(
        preferredChildToInsert,
        [target],
        projectContentsRef.current,
        allElementPropsRef.current,
        propertyControlsInfoRef.current,
        metadataRef.current,
        elementPathTreesRef.current,
        dispatch,
        insertionTarget,
      )
    },
    [
      target,
      projectContentsRef,
      allElementPropsRef,
      propertyControlsInfoRef,
      metadataRef,
      elementPathTreesRef,
      dispatch,
      insertionTarget,
    ],
  )

  const closePicker = React.useCallback(
    () => dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'none'))], 'everyone'),
    [dispatch],
  )

  return (
    <div
      data-testid={CanvasToolbarSearchTestID}
      style={{
        width: '100%',
      }}
    >
      <ComponentPicker
        allComponents={allInsertableComponents}
        onItemClick={onItemClick}
        closePicker={closePicker}
        shownInToolbar={true}
        insertionActive={insertionActive}
      />
    </div>
  )
})
CanvasToolbarSearch.displayName = 'CanvasToolbarSearch'

export const CanvasToolbarEditButtonID = 'canvas-toolbar-edit-button'

function switchToSelectModeCloseMenus(dispatch: EditorDispatch) {
  dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'none'))], 'everyone')
}

export const WrapInDivButtonTestId = 'wrap-in-div-button'

const UnreadThreadsIndicator = React.memo(() => {
  const canvasToolbarMode = useToolbarMode()

  const { threads } = useThreads()
  const { threads: readThreads } = useReadThreads()

  const unreadThreads = React.useMemo(() => {
    const readThreadIds = pluck(readThreads, 'id')
    return threads.filter((t) => !t.metadata.resolved && !readThreadIds.includes(t.id))
  }, [threads, readThreads])

  return (
    <div
      style={{
        width: 6,
        height: 6,
        borderRadius: 6,
        flexShrink: 0,
        flexGrow: 0,
        background: 'red',
        outline:
          canvasToolbarMode.primary === 'comment'
            ? `1.5px solid ${colorTheme.primary.value}`
            : `1.5px solid ${colorTheme.bg1.value}`,
        position: 'relative',
        top: 5,
        left: -9,
        opacity: unreadThreads.length > 0 ? 1 : 0,
      }}
    />
  )
})

export const CanvasToolbar = React.memo(() => {
  const dispatch = useDispatch()
  const theme = useColorTheme()

  const canvasToolbarMode = useToolbarMode()

  const insertDivCallback = useEnterDrawToInsertForDiv()
  const insertImgCallback = useEnterDrawToInsertForImage()
  const insertTextCallback = useEnterTextEditMode()
  const insertButtonCallback = useEnterDrawToInsertForButton()
  const insertConditionalCallback = useEnterDrawToInsertForConditional()

  // Back to select mode, close the "floating" menu and turn off the forced insert mode.
  const dispatchSwitchToSelectModeCloseMenus = React.useCallback(() => {
    switchToSelectModeCloseMenus(dispatch)
    dispatch([CanvasActions.clearInteractionSession(false)])
  }, [dispatch])

  const zoomLevel = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'CanvasToolbar zoomLevel',
  )

  const editorMode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode.type,
    'TopMenu editorMode',
  )

  const canComment = useCanComment()

  const isFollowMode = editorMode === 'follow'
  const zoom100pct = React.useCallback(() => {
    if (!isFollowMode) {
      dispatch([CanvasActions.zoom(1)])
    }
  }, [dispatch, isFollowMode])

  const isLiveMode = editorMode === 'live'
  const toggleLiveMode = React.useCallback(() => {
    if (isLiveMode) {
      dispatchSwitchToSelectModeCloseMenus()
    } else {
      dispatch([switchEditorMode(EditorModes.liveMode())])
    }
  }, [dispatch, isLiveMode, dispatchSwitchToSelectModeCloseMenus])

  const isCommentMode = editorMode === 'comment'
  const toggleCommentMode = React.useCallback(() => {
    if (isCommentMode) {
      dispatchSwitchToSelectModeCloseMenus()
    } else {
      dispatch([
        switchEditorMode(EditorModes.commentMode(null, 'not-dragging')),
        setRightMenuTab(RightMenuTab.Comments),
      ])
    }
  }, [dispatch, isCommentMode, dispatchSwitchToSelectModeCloseMenus])

  const resetCanvasCallback = React.useCallback(() => {
    dispatch([resetCanvas()])
  }, [dispatch])

  const toggleInsertButtonClicked = React.useCallback(() => {
    if (canvasToolbarMode.primary === 'insert') {
      dispatchSwitchToSelectModeCloseMenus()
    } else {
      dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'pseudo-insert'))])
    }
  }, [canvasToolbarMode.primary, dispatch, dispatchSwitchToSelectModeCloseMenus])

  const editButtonIcon: CanvasStrategyIcon = useEditorState(
    Substores.restOfStore,
    (store) => store.strategyState.currentStrategyIcon ?? { category: 'tools', type: 'pointer' },
    'SettingsPanel currentStrategyState',
  )

  const wrapInSubmenu = React.useCallback((wrapped: React.ReactNode) => {
    return (
      <FlexColumn
        data-testid='canvas-toolbar-submenu'
        style={{
          width: '100%',
          overflow: 'hidden',
          justifyContent: 'flex-end',
          backgroundColor: colorTheme.bg1subdued.value,
          borderRadius: '0 0 6px 6px',
          boxShadow: UtopiaStyles.shadowStyles.low.boxShadow,
          pointerEvents: 'initial',
          zIndex: -1, // it sits below the main menu row, but we want the main menu's shadow to cast over this one
          // to make the submenu appear visually underneath the corners of the main menu
          marginTop: -6,
          paddingTop: 6,
        }}
      >
        <FlexRow>{wrapped}</FlexRow>
      </FlexColumn>
    )
  }, [])

  const focusCanvasOnMouseDown = React.useCallback(
    (event: React.MouseEvent<Element>) => {
      stopPropagation(event)
      dispatch([setFocus('canvas')], 'everyone')
    },
    [dispatch],
  )

  const loggedIn = useEditorState(
    Substores.userState,
    (store) => isLoggedIn(store.userState.loginState),
    'TopMenu loggedIn',
  )

  const roomStatus = useStatus()
  const commentButtonDisabled = !loggedIn || roomStatus !== 'connected'
  const commentButtonTooltip = !loggedIn
    ? 'Sign in to comment'
    : roomStatus !== 'connected'
    ? 'Not connected to room'
    : 'Comment Mode'

  const commentButtonTestId =
    roomStatus === 'connected'
      ? CommentModeButtonTestId('connected')
      : CommentModeButtonTestId('disconnected')
  const allowedToEdit = useAllowedToEditProject()

  const isMyProject = useIsMyProject()

  const [activeRemixScene] = useAtom(ActiveRemixSceneAtom)
  const remixSceneIsActive = !EP.isEmptyPath(activeRemixScene)
  const showRemixNavBar =
    remixSceneIsActive &&
    ((canvasToolbarMode.primary === 'edit' && canvasToolbarMode.secondary !== 'strategy-active') ||
      canvasToolbarMode.primary === 'play')

  const { codeEditorVisible, navigatorVisible, inspectorVisible } = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return {
        codeEditorVisible: store.editor.interfaceDesigner.codePaneVisible,
        navigatorVisible: store.editor.leftMenu.visible,
        inspectorVisible: store.editor.rightMenu.visible,
      }
    },
    'storedLayoutToResolvedPanels panel visibility',
  )

  const panelPopupItems: DropdownMenuItem[] = React.useMemo(
    () => [
      regularDropdownMenuItem({
        id: 'navigator',
        label: 'Navigator',
        checked: navigatorVisible,
        shortcut: keyToString(shortcutDetailsWithDefaults[TOGGLE_NAVIGATOR].shortcutKeys[0]),
        onSelect: () => dispatch([togglePanel('leftmenu')]),
      }),
      regularDropdownMenuItem({
        id: 'rightmenu',
        label: 'Inspector',
        checked: inspectorVisible,
        shortcut: keyToString(shortcutDetailsWithDefaults[TOGGLE_INSPECTOR].shortcutKeys[0]),
        onSelect: () => dispatch([togglePanel('rightmenu')]),
      }),
      regularDropdownMenuItem({
        id: 'code-editor',
        label: 'Code Editor',
        checked: codeEditorVisible,
        shortcut: keyToString(
          shortcutDetailsWithDefaults[TOGGLE_CODE_EDITOR_SHORTCUT].shortcutKeys[0],
        ),
        onSelect: () => dispatch([togglePanel('codeEditor')]),
      }),
    ],
    [codeEditorVisible, dispatch, inspectorVisible, navigatorVisible],
  )

  const panelSelectorOpenButton = React.useCallback(
    (open: boolean) => (
      <ToolbarButton
        testid={commentButtonTestId}
        iconType={'panels'}
        iconCategory='tools'
        primary={open}
        onClick={NO_OP}
        keepActiveInLiveMode
      />
    ),
    [commentButtonTestId],
  )

  return (
    <FlexColumn
      style={{ alignItems: 'start', justifySelf: 'center' }}
      // Mouse events should never go through this component.
      onClick={stopPropagation}
      onMouseDown={focusCanvasOnMouseDown}
      onMouseUp={stopPropagation}
    >
      <div
        id={CanvasToolbarId}
        style={{
          backgroundColor: theme.inspectorBackground.value,
          borderRadius: 6,
          overflow: 'hidden',
          boxShadow: UtopiaStyles.shadowStyles.low.boxShadow,
          pointerEvents: 'initial',
          display: 'flex',
          flexDirection: 'row',
          alignItems: 'center',
          padding: 3,
          gap: 8,
        }}
      >
        <Tooltip title='Edit' placement='bottom'>
          <ToolbarButton
            iconType={editButtonIcon.type}
            iconCategory={editButtonIcon.category}
            primary={canvasToolbarMode.primary === 'edit'}
            onClick={dispatchSwitchToSelectModeCloseMenus}
            keepActiveInLiveMode
            testid={CanvasToolbarEditButtonID}
          />
        </Tooltip>
        {when(
          allowedToEdit,
          <>
            <Tooltip title='Insert or Edit Text' placement='bottom'>
              <ToolbarButton
                testid={InsertOrEditTextButtonTestId}
                iconType='text'
                iconCategory='tools'
                primary={canvasToolbarMode.primary === 'text'}
                onClick={insertTextCallback}
                keepActiveInLiveMode
              />
            </Tooltip>
            <Tooltip title='Insert' placement='bottom'>
              <ToolbarButton
                testid={InsertMenuButtonTestId}
                iconType='insert'
                iconCategory='tools'
                primary={canvasToolbarMode.primary === 'insert'}
                onClick={toggleInsertButtonClicked}
                keepActiveInLiveMode
              />
            </Tooltip>
          </>,
        )}
        <Tooltip title='Live Mode' placement='bottom'>
          <ToolbarButton
            testid={PlayModeButtonTestId}
            iconType={'play'}
            iconCategory='tools'
            primary={canvasToolbarMode.primary === 'play'}
            onClick={toggleLiveMode}
            keepActiveInLiveMode
          />
        </Tooltip>
        {when(
          canComment,
          <div style={{ display: 'flex', width: 26 }}>
            <Tooltip title={commentButtonTooltip} placement='bottom'>
              <ToolbarButton
                testid={commentButtonTestId}
                iconType={'comment'}
                iconCategory='tools'
                primary={canvasToolbarMode.primary === 'comment'}
                onClick={toggleCommentMode}
                keepActiveInLiveMode
                disabled={commentButtonDisabled}
              />
            </Tooltip>
            <MultiplayerWrapper errorFallback={null} suspenseFallback={null}>
              <UnreadThreadsIndicator />
            </MultiplayerWrapper>
          </div>,
        )}
        <Separator />
        <Tooltip title='Zoom to 100%' placement='bottom'>
          <SquareButton
            style={{
              width: 'min-content',
              padding: '0 4px',
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
          <ToolbarButton
            iconType='refresh'
            iconCategory='semantic'
            onClick={resetCanvasCallback}
            keepActiveInLiveMode
            size={16}
          />
        </Tooltip>
        {unless(isMyProject, <ViewOnlyBadge />)}
        <Separator />
        <Tooltip title='Manage panels'>
          <DropdownMenu sideOffset={6} items={panelPopupItems} opener={panelSelectorOpenButton} />
        </Tooltip>
      </div>
      {/* Edit Mode submenus */}
      {when(
        canvasToolbarMode.primary === 'edit' &&
          canvasToolbarMode.secondary === 'strategy-active' &&
          allowedToEdit,
        <StrategyIndicator />,
      )}
      {/* Insert Mode */}
      {canvasToolbarMode.primary === 'insert'
        ? wrapInSubmenu(
            <FlexColumn style={{ padding: '3px 8px 0 8px', flexGrow: 1 }}>
              <FlexRow>
                <Tooltip title='Back' placement='bottom'>
                  <ToolbarButton
                    iconCategory='semantic'
                    iconType='icon-semantic-back'
                    onClick={dispatchSwitchToSelectModeCloseMenus}
                    style={{ width: undefined }}
                  />
                </Tooltip>
                <Tooltip title='Insert div' placement='bottom'>
                  <ToolbarButton
                    iconType='view'
                    secondary={canvasToolbarMode.secondary.divInsertionActive}
                    onClick={insertDivCallback}
                  />
                </Tooltip>
                <Tooltip title='Insert image' placement='bottom'>
                  <ToolbarButton
                    iconType='image'
                    secondary={canvasToolbarMode.secondary.imageInsertionActive}
                    onClick={insertImgCallback}
                  />
                </Tooltip>
                <Tooltip title='Insert button' placement='bottom'>
                  <ToolbarButton
                    iconType='clickable'
                    secondary={canvasToolbarMode.secondary.buttonInsertionActive}
                    onClick={insertButtonCallback}
                  />
                </Tooltip>
                <Tooltip title='Insert conditional' placement='bottom'>
                  <ToolbarButton
                    testid={InsertConditionalButtonTestId}
                    iconType='conditional'
                    secondary={canvasToolbarMode.secondary.conditionalInsertionActive}
                    onClick={insertConditionalCallback}
                  />
                </Tooltip>
              </FlexRow>
              <FlexRow>
                <Tile style={{ flexGrow: 1 }}>
                  <CanvasToolbarSearch />
                </Tile>
              </FlexRow>
            </FlexColumn>,
          )
        : null}
      {/* Live Mode */}
      {showRemixNavBar ? wrapInSubmenu(<RemixNavigationBar />) : null}
    </FlexColumn>
  )
})

interface ToolbarButtonProps {
  iconType: string
  iconCategory?: string
  primary?: boolean
  secondary?: boolean
  keepActiveInLiveMode?: boolean
  style?: React.CSSProperties
  testid?: string
  onClick: (event: React.MouseEvent<Element>) => void
  size?: number
  disabled?: boolean
}
const ToolbarButton = React.memo((props: ToolbarButtonProps) => {
  const [isHovered, setIsHovered] = useState(false)
  const keepActiveInLiveMode = props.keepActiveInLiveMode ?? false
  const primary = props.primary ?? false
  const secondary = props.secondary ?? false
  const disabled = props.disabled ?? false
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
    <Button
      data-testid={props.testid}
      style={{ height: 26, width: 26, borderRadius: 3, ...props.style }}
      primary={primary}
      spotlight={secondary}
      highlight
      onClick={props.onClick}
      disabled={disabled || (canvasInLiveMode && !keepActiveInLiveMode)}
      overriddenBackground={props.primary ? undefined : 'transparent'}
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
    </Button>
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
        margin: '0 4px',
        backgroundColor: colorTheme.seperator.value,
      }}
    ></div>
  )
})

const ViewOnlyBadge = React.memo((props) => {
  return (
    <FlexRow
      style={{
        alignItems: 'center',
        justifyContent: 'center',
        height: 'min-content',
        marginLeft: 6,
        padding: '4px 7px',
        borderRadius: 6,
        fontWeight: 500,
        backgroundColor: colorTheme.primary30.value,
      }}
    >
      View Only
    </FlexRow>
  )
})
