/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React, { useState } from 'react'
import type { TooltipProps } from '../../uuiui'
import { Tile, UtopiaStyles, opacity } from '../../uuiui'
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
import { ElementsOutsideVisibleAreaIndicator } from './elements-outside-visible-area-indicator'
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

export const InsertMenuButtonTestId = 'insert-menu-button'
export const PlayModeButtonTestId = 'canvas-toolbar-play-mode'
export const CommentModeButtonTestId = (status: string) => `canvas-toolbar-comment-mode-${status}`
export const InsertConditionalButtonTestId = 'insert-mode-conditional'
export const CanvasToolbarId = 'canvas-toolbar'

export const CanvasToolbarSearchPortalId = 'canvas-toolbar-search-portal'

export const CanvasToolbarSearchTestID = 'canvas-toolbar-search'

export const CanvasToolbarSearch = React.memo(() => {
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

  const onItemClick = React.useCallback(
    (preferredChildToInsert: InsertableComponent) => (e: React.UIEvent) => {
      e.stopPropagation()
      e.preventDefault()

      insertComponentPickerItem(
        preferredChildToInsert,
        [target],
        projectContentsRef.current,
        metadataRef.current,
        elementPathTreesRef.current,
        dispatch,
        insertionTarget,
      )
    },
    [target, projectContentsRef, metadataRef, elementPathTreesRef, dispatch, insertionTarget],
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
        top: 8,
        left: -15,
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
      dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'none'))])
    } else {
      dispatch([switchEditorMode(EditorModes.liveMode())])
    }
  }, [dispatch, isLiveMode])

  const isCommentMode = editorMode === 'comment'
  const toggleCommentMode = React.useCallback(() => {
    if (isCommentMode) {
      dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'none'))])
    } else {
      dispatch([
        switchEditorMode(EditorModes.commentMode(null, 'not-dragging')),
        setRightMenuTab(RightMenuTab.Comments),
      ])
    }
  }, [dispatch, isCommentMode])

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

  const currentStrategyState = useEditorState(
    Substores.restOfStore,
    (store) => store.strategyState,
    'SettingsPanel currentStrategyState',
  )
  const editButtonIcon: CanvasStrategyIcon = React.useMemo(() => {
    return currentStrategyState.currentStrategyIcon ?? { category: 'tools', type: 'pointer' }
  }, [currentStrategyState.currentStrategyIcon])

  const wrapInSubmenu = React.useCallback((wrapped: React.ReactNode) => {
    return (
      <FlexRow
        data-testid='canvas-toolbar-submenu'
        style={{
          width: '100%',
          marginTop: -10,
          paddingTop: 10,
          backgroundColor: colorTheme.bg1subdued.value,
          borderRadius: '0px 0px 10px 10px',
          boxShadow: UtopiaStyles.shadowStyles.low.boxShadow,
          pointerEvents: 'initial',
          zIndex: -1, // it sits below the main menu row, but we want the main menu's shadow to cast over this one
        }}
      >
        {wrapped}
      </FlexRow>
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
          borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
          overflow: 'hidden',
          boxShadow: UtopiaStyles.shadowStyles.low.boxShadow,
          pointerEvents: 'initial',
          display: 'flex',
          flexDirection: 'row',
          alignItems: 'center',
          padding: '0 6px 0 8px',
        }}
      >
        <Tooltip title='Edit' placement='bottom'>
          <InsertModeButton
            iconType={editButtonIcon.type}
            iconCategory={editButtonIcon.category}
            primary={canvasToolbarMode.primary === 'edit'}
            onClick={dispatchSwitchToSelectModeCloseMenus}
            keepActiveInLiveMode
            testid={CanvasToolbarEditButtonID}
            style={{ width: 36 }}
          />
        </Tooltip>
        {when(
          allowedToEdit,
          <>
            <Tooltip title='Insert or Edit Text' placement='bottom'>
              <InsertModeButton
                iconType='text'
                iconCategory='tools'
                primary={canvasToolbarMode.primary === 'text'}
                onClick={insertTextCallback}
                keepActiveInLiveMode
                style={{ width: 36 }}
              />
            </Tooltip>
            <Tooltip title='Insert' placement='bottom'>
              <InsertModeButton
                testid={InsertMenuButtonTestId}
                iconType='insert'
                iconCategory='tools'
                primary={canvasToolbarMode.primary === 'insert'}
                onClick={toggleInsertButtonClicked}
                keepActiveInLiveMode
                style={{ width: 36 }}
              />
            </Tooltip>
          </>,
        )}
        <Tooltip title='Live Mode' placement='bottom'>
          <InsertModeButton
            testid={PlayModeButtonTestId}
            iconType={'play'}
            iconCategory='tools'
            primary={canvasToolbarMode.primary === 'play'}
            onClick={toggleLiveMode}
            keepActiveInLiveMode
            style={{ width: 36 }}
          />
        </Tooltip>
        {when(
          canComment,
          <div style={{ display: 'flex', width: 36 }}>
            <Tooltip title={commentButtonTooltip} placement='bottom'>
              <InsertModeButton
                testid={commentButtonTestId}
                iconType={'comment'}
                iconCategory='tools'
                primary={canvasToolbarMode.primary === 'comment'}
                onClick={toggleCommentMode}
                keepActiveInLiveMode
                style={{ width: 36 }}
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
              height: 32,
              width: 'min-content',
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
            keepActiveInLiveMode
            size={16}
          />
        </Tooltip>
        <ElementsOutsideVisibleAreaIndicator />
        {unless(isMyProject, <ViewOnlyBadge />)}
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
            <FlexColumn style={{ padding: '0 8px 0 8px', flexGrow: 1 }}>
              <FlexRow>
                <Tooltip title='Back' placement='bottom'>
                  <InsertModeButton
                    iconCategory='semantic'
                    iconType='icon-semantic-back'
                    onClick={dispatchSwitchToSelectModeCloseMenus}
                    style={{ width: undefined }}
                  />
                </Tooltip>
                <Tooltip title='Insert div' placement='bottom'>
                  <InsertModeButton
                    iconType='view'
                    secondary={canvasToolbarMode.secondary.divInsertionActive}
                    onClick={insertDivCallback}
                  />
                </Tooltip>
                <Tooltip title='Insert image' placement='bottom'>
                  <InsertModeButton
                    iconType='image'
                    secondary={canvasToolbarMode.secondary.imageInsertionActive}
                    onClick={insertImgCallback}
                  />
                </Tooltip>
                <Tooltip title='Insert button' placement='bottom'>
                  <InsertModeButton
                    iconType='clickable'
                    secondary={canvasToolbarMode.secondary.buttonInsertionActive}
                    onClick={insertButtonCallback}
                  />
                </Tooltip>
                <Tooltip title='Insert conditional' placement='bottom'>
                  <InsertModeButton
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
      {(canvasToolbarMode.primary === 'edit' &&
        canvasToolbarMode.secondary !== 'strategy-active') ||
      canvasToolbarMode.primary === 'play'
        ? wrapInSubmenu(<RemixNavigationBar />)
        : null}
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
  disabled?: boolean
}
const InsertModeButton = React.memo((props: InsertModeButtonProps) => {
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
    <SquareButton
      data-testid={props.testid}
      style={{ height: 32, width: 32, ...props.style }}
      primary={primary}
      spotlight={secondary}
      highlight
      onClick={props.onClick}
      disabled={disabled || (canvasInLiveMode && !keepActiveInLiveMode)}
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
