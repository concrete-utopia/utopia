/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { css, jsx } from '@emotion/react'
import type { CSSObject } from '@emotion/react'
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
import { getControlStyles } from '../../uuiui-deps'
import CanvasActions from '../canvas/canvas-actions'
import {
  applyCommandsAction,
  closeFloatingInsertMenu,
  openFloatingInsertMenu,
  resetCanvas,
  setRightMenuTab,
  showToast,
  switchEditorMode,
  wrapInElement,
} from './actions/action-creators'
import { EditorModes } from './editor-modes'
import {
  useEnterDrawToInsertForButton,
  useEnterDrawToInsertForConditional,
  useEnterDrawToInsertForDiv,
  useEnterDrawToInsertForImage,
  useEnterTextEditMode,
  useToInsert,
} from './insert-callbacks'
import { useDispatch } from './store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from './store/store-hook'
import { defaultTransparentViewElement } from './defaults'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import { useToolbarMode } from './canvas-toolbar-states'
import { unless, when } from '../../utils/react-conditionals'
import { StrategyIndicator } from '../canvas/controls/select-mode/strategy-indicator'
import { toggleAbsolutePositioningCommands } from '../inspector/inspector-common'
import { createFilter } from 'react-select'
import WindowedSelect from 'react-windowed-select'
import { InspectorInputEmotionStyle } from '../../uuiui/inputs/base-input'
import { stopPropagation } from '../inspector/common/inspector-utils'
import { useConvertTo } from './convert-callbacks'
import { useWrapInDiv } from './wrap-in-callbacks'
import { ElementsOutsideVisibleAreaIndicator } from './elements-outside-visible-area-indicator'
import { RemixNavigationBar } from './remix-navigation-bar'
import {
  fragmentComponentInfo,
  fragmentElementsDescriptors,
  insertableComponentGroupFragment,
} from '../shared/project-components'
import { setFocus } from '../common/actions'
import type { CanvasStrategyIcon } from '../canvas/canvas-strategies/canvas-strategy-types'
import { isLoggedIn } from './action-types'
import type { EditorDispatch } from './action-types'
import type { InsertMenuItem } from '../canvas/ui/floating-insert-menu'
import {
  CustomComponentOption,
  useComponentSelectorStyles,
  useGetInsertableComponents,
} from '../canvas/ui/floating-insert-menu'
import { RightMenuTab, floatingInsertMenuStateSwap } from './store/editor-state'
import { useStatus, useThreads } from '../../../liveblocks.config'
import { useAllowedToEditProject, useIsMyProject } from './store/collaborative-editing'
import { useCanComment, useReadThreads } from '../../core/commenting/comment-hooks'
import { pluck } from '../../core/shared/array-utils'
import { MultiplayerWrapper } from '../../utils/multiplayer-wrapper'
import { HEADERS, MODE } from '../../common/server'
import urljoin from 'url-join'
import { getProjectID } from '../../common/env-vars'
import { REQUEST_UPDATE_CONTEXT_GLOABAL_HACKED } from './store/remix-derived-data'
import { isLeft } from '../../core/shared/either'
import { notice } from '../common/notice'

export const InsertMenuButtonTestId = 'insert-menu-button'
export const PlayModeButtonTestId = 'canvas-toolbar-play-mode'
export const CommentModeButtonTestId = (status: string) => `canvas-toolbar-comment-mode-${status}`
export const InsertConditionalButtonTestId = 'insert-mode-conditional'
export const CanvasToolbarId = 'canvas-toolbar'

export const CanvasToolbarSearchPortalId = 'canvas-toolbar-search-portal'

export const ToolbarSearchListing = React.memo(() => {
  return <div style={{ alignSelf: 'end', width: 232 }} id={CanvasToolbarSearchPortalId} />
})
ToolbarSearchListing.displayName = 'ToolbarSearchListing'

export interface CanvasToolbarSearchProps {
  actionWith: (item: InsertMenuItem | null) => void
}

export const CanvasToolbarSearchTestID = 'canvas-toolbar-search'

export const CanvasToolbarSearch = React.memo((props: CanvasToolbarSearchProps) => {
  const dispatch = useDispatch()
  const insertMenuMode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.floatingInsertMenu.insertMenuMode,
    'CanvasToolbarSearch insertMenuMode',
  )
  const options = useGetInsertableComponents(insertMenuMode)
  const componentSelectorStyles = useComponentSelectorStyles()
  const menuPortalTarget = document.getElementById(CanvasToolbarSearchPortalId)
  const theme = useColorTheme()
  const focusedPanelRef = useRefEditorState((store) => store.editor.focusedPanel)
  const interactionSessionRef = useRefEditorState((store) => store.editor.canvas.interactionSession)

  // Focus the input when it is displayed.
  const selectRef = React.useRef<any>(null)
  React.useEffect(() => {
    // Only focus when:
    // - There's a ref we can use to focus.
    // - The canvas is focused, so that we don't unfocus the code editor.
    // - If an interaction hasn't already been started.
    if (
      selectRef.current != null &&
      focusedPanelRef.current === 'canvas' &&
      interactionSessionRef.current === null
    ) {
      selectRef.current.focus()
    }
  })

  const onKeyDown = React.useCallback(
    (e: React.KeyboardEvent) => {
      if (e.key === 'Escape') {
        switchToSelectModeCloseMenus(dispatch)
      }
    },
    [dispatch],
  )

  return (
    <div data-testid={CanvasToolbarSearchTestID}>
      <WindowedSelect
        id={CanvasToolbarSearchTestID}
        ref={selectRef}
        components={{ Option: CustomComponentOption }}
        openMenuOnFocus={true}
        openMenuOnClick={true}
        onBlur={undefined}
        onKeyDown={onKeyDown}
        onChange={props.actionWith}
        options={options}
        menuPortalTarget={menuPortalTarget}
        filterOption={createFilter({
          ignoreAccents: true,
          stringify: (c) => c.data.source + c.data.label,
        })}
        styles={{
          ...componentSelectorStyles,
          menuPortal: (styles: CSSObject): CSSObject => {
            return {
              zIndex: -2,
              padding: '0 4px',
              overflow: 'hidden',
              height: 'auto',
              backgroundColor: theme.bg1subdued.value,
              borderRadius: '0px 10px 10px 10px',
              boxShadow: UtopiaStyles.shadowStyles.low.boxShadow,
              pointerEvents: 'initial',
            }
          },
          input: (styles: CSSObject): CSSObject => {
            return {
              ...(InspectorInputEmotionStyle({
                hasLabel: false,
                controlStyles: getControlStyles('simple'),
              }) as CSSObject),
              paddingLeft: 4,
              backgroundColor: colorTheme.seperator.value,
              flexGrow: 1,
              display: 'flex',
              alignItems: 'center',
              minWidth: '200px',
              borderRadius: '10px',
              borderWidth: 1,
              borderColor: colorTheme.dynamicBlue.value,
              borderStyle: 'solid',
            }
          },
          menuList: (styles: CSSObject): CSSObject => {
            return {
              position: 'relative',
              maxHeight: 210,
              paddingLeft: 8,
              paddingRight: 8,
              overflowY: 'auto',
              display: 'flex',
              flexDirection: 'column',
              gap: 6,
            }
          },
        }}
        maxMenuHeight={138}
      />
    </div>
  )
})
CanvasToolbarSearch.displayName = 'CanvasToolbarSearch'

export const CanvasToolbarEditButtonID = 'canvas-toolbar-edit-button'

function switchToSelectModeCloseMenus(dispatch: EditorDispatch) {
  dispatch(
    [switchEditorMode(EditorModes.selectMode(null, false, 'none')), closeFloatingInsertMenu()],
    'everyone',
  )
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

  const insertMenuMode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.floatingInsertMenu.insertMenuMode,
    'CanvasToolbar insertMenuMode',
  )
  const wrapInDivCallback = useWrapInDiv()

  const convertToCallback = useConvertTo()
  const toInsertCallback = useToInsert()

  // Back to select mode, close the "floating" menu and turn off the forced insert mode.
  const dispatchSwitchToSelectModeCloseMenus = React.useCallback(() => {
    switchToSelectModeCloseMenus(dispatch)
  }, [dispatch])

  const convertToAndClose = React.useCallback(
    (convertTo: InsertMenuItem | null) => {
      convertToCallback(convertTo)
      dispatchSwitchToSelectModeCloseMenus()
    },
    [convertToCallback, dispatchSwitchToSelectModeCloseMenus],
  )

  const convertToFragment = React.useCallback(() => {
    // Should be consistent with the value that would be present in the dropdown.
    // Done like this to avoid having to actually pull the options in the dropdown
    // as that will cause a lot of extra work when rendering this toolbar.
    const convertToFragmentMenuItem: InsertMenuItem = {
      label: fragmentComponentInfo.insertMenuLabel,
      source: null,
      value: {
        importsToAdd: fragmentComponentInfo.importsToAdd,
        element: fragmentComponentInfo.elementToInsert,
        name: fragmentComponentInfo.insertMenuLabel,
        stylePropOptions: [],
        defaultSize: null,
        source: insertableComponentGroupFragment(),
        key: fragmentComponentInfo.insertMenuLabel,
        insertionCeiling: null,
        icon: fragmentElementsDescriptors.fragment.icon,
      },
    }
    convertToAndClose(convertToFragmentMenuItem)
  }, [convertToAndClose])

  const wrapInDivAndClose = React.useCallback(() => {
    wrapInDivCallback()
    dispatchSwitchToSelectModeCloseMenus()
  }, [dispatchSwitchToSelectModeCloseMenus, wrapInDivCallback])

  const toInsertAndClose = React.useCallback(
    (toInsert: InsertMenuItem | null) => {
      toInsertCallback(toInsert)
      dispatchSwitchToSelectModeCloseMenus()
    },
    [dispatchSwitchToSelectModeCloseMenus, toInsertCallback],
  )

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
          marginLeft: 8,
          height: 32,
          overflow: 'hidden',
          backgroundColor: colorTheme.bg1subdued.value,
          borderRadius: '0px 10px 10px 10px',
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

  const callUpdateMetaobjectApi = React.useCallback(() => {
    const projectId = getProjectID()
    if (projectId == null) {
      throw new Error('projectId == null')
    }

    const routeId_HARDCODED = 'routes/_index'
    const dataPath_HARDCODED = ['reviews', 0, 'rating']
    const newValue_HARDCODED = 3

    const requestUpdateData = REQUEST_UPDATE_CONTEXT_GLOABAL_HACKED[routeId_HARDCODED]
    const lastLoaderResult =
      REQUEST_UPDATE_CONTEXT_GLOABAL_HACKED[routeId_HARDCODED].lastLoaderResult
    if (lastLoaderResult == null) {
      console.error('lastLoaderResult is null')
      return
    }

    const requestUpdateResult = requestUpdateData.requestUpdateCallback?.(
      dataPath_HARDCODED,
      lastLoaderResult,
      newValue_HARDCODED,
    )
    if (requestUpdateResult == null || isLeft(requestUpdateResult)) {
      console.error('Request update failed', requestUpdateResult)
      return
    }

    void fetch(urljoin('/internal/metaobjectupdate', projectId), {
      method: 'POST',
      credentials: 'include',
      headers: HEADERS,
      mode: MODE,
      body: JSON.stringify(requestUpdateResult.value),
    })
      .then((res) => res.json())
      .then((data) => {
        // console.log(data)
        if (data.result.data.metaobjectUpdate.userErrors.length > 0) {
          dispatch([
            showToast(
              notice(
                `Metaobject update failed: ${JSON.stringify(
                  data.data.metaobjectUpdate.userErrors,
                )}`,
                'ERROR',
                false,
                'metaobject-update-error',
              ),
            ),
          ])
        } else {
          dispatch([
            showToast(
              notice(`Metaobject update requested`, 'SUCCESS', false, 'metaobject-update-success'),
            ),
          ])
        }
      })
  }, [dispatch])

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
        <Tooltip title='Call Metaobject API' placement='bottom'>
          <InsertModeButton
            iconType={editButtonIcon.type}
            iconCategory={editButtonIcon.category}
            primary={canvasToolbarMode.primary === 'edit'}
            onClick={callUpdateMetaobjectApi}
            keepActiveInLiveMode
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
          canvasToolbarMode.secondary === 'selected' &&
          allowedToEdit,
        <>
          {when(
            insertMenuMode === 'wrap',
            wrapInSubmenu(
              <FlexRow style={{ padding: '0 8px' }}>
                <Tooltip title='Back' placement='bottom'>
                  <InsertModeButton
                    iconCategory='semantic'
                    iconType='icon-semantic-back'
                    onClick={dispatchSwitchToSelectModeCloseMenus}
                    style={{ width: undefined }}
                  />
                </Tooltip>
                <Icn
                  category='tools'
                  type='wrap-action'
                  width={18}
                  height={18}
                  style={{ marginRight: 10 }}
                />

                <Tooltip title='Wrap in div' placement='bottom'>
                  <InsertModeButton
                    testid={WrapInDivButtonTestId}
                    iconType='div'
                    onClick={wrapInDivAndClose}
                  />
                </Tooltip>
                <Tile style={{ height: '100%' }}>
                  <CanvasToolbarSearch actionWith={convertToAndClose} />
                </Tile>
              </FlexRow>,
            ),
          )}
          {when(
            insertMenuMode === 'swap',
            wrapInSubmenu(
              <FlexRow style={{ padding: '0 8px' }}>
                <Tooltip title='Back' placement='bottom'>
                  <InsertModeButton
                    iconCategory='semantic'
                    iconType='icon-semantic-back'
                    onClick={dispatchSwitchToSelectModeCloseMenus}
                    style={{ width: undefined }}
                  />
                </Tooltip>
                <Icn
                  category='tools'
                  type='convert-action'
                  width={18}
                  height={18}
                  style={{ marginRight: 10 }}
                />
                <Tooltip title='Swap to Fragment' placement='bottom'>
                  <InsertModeButton iconType='fragment' onClick={convertToFragment} />
                </Tooltip>
                <Tile style={{ height: '100%' }}>
                  <CanvasToolbarSearch actionWith={convertToAndClose} />
                </Tile>
              </FlexRow>,
            ),
          )}
        </>,
      )}
      {when(
        canvasToolbarMode.primary === 'edit' &&
          canvasToolbarMode.secondary === 'strategy-active' &&
          allowedToEdit,
        <StrategyIndicator />,
      )}
      {/* Insert Mode */}
      {canvasToolbarMode.primary === 'insert'
        ? wrapInSubmenu(
            <FlexRow style={{ padding: '0 8px' }}>
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
              <Tile style={{ height: '100%' }}>
                <CanvasToolbarSearch actionWith={toInsertAndClose} />
              </Tile>
            </FlexRow>,
          )
        : null}
      {/* Live Mode */}
      {(canvasToolbarMode.primary === 'edit' && insertMenuMode === 'closed') ||
      canvasToolbarMode.primary === 'play'
        ? wrapInSubmenu(<RemixNavigationBar />)
        : null}
      <ToolbarSearchListing />
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
