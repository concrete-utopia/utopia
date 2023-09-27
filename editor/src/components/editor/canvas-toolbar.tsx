/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import type { CSSObject } from '@emotion/react'
import { jsx } from '@emotion/react'
import * as React from 'react'
import type { TooltipProps } from '../../uuiui'
import { Tile } from '../../uuiui'
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
import { togglePanel } from './actions/action-creators'
import { defaultTransparentViewElement } from './defaults'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import { useToolbarMode } from './canvas-toolbar-states'
import { when } from '../../utils/react-conditionals'
import { StrategyIndicator } from '../canvas/controls/select-mode/strategy-indicator'
import { toggleAbsolutePositioningCommands } from '../inspector/inspector-common'
import { NO_OP } from '../../core/shared/utils'
import type { InsertMenuItem } from '../canvas/ui/floating-insert-menu'
import {
  CustomComponentOption,
  useComponentSelectorStyles,
  useGetInsertableComponents,
} from '../canvas/ui/floating-insert-menu'
import { createFilter } from 'react-select'
import WindowedSelect from 'react-windowed-select'
import { InspectorInputEmotionStyle } from '../../uuiui/inputs/base-input'
import { stopPropagation } from '../inspector/common/inspector-utils'
import { useConvertTo } from './convert-callbacks'
import { useWrapInDiv } from './wrap-in-callbacks'
import { ElementsOutsideVisibleAreaIndicator } from './elements-outside-visible-area-indicator'
import { useResetRemixApps } from '../canvas/remix/remix-hooks'
import { RemixNavigationBar } from './remix-navigation-bar'
import {
  fragmentComponentInfo,
  insertableComponentGroupFragment,
} from '../shared/project-components'
import { setFocus } from '../common/actions'
import type { CanvasStrategyIcon } from '../canvas/canvas-strategies/canvas-strategy-types'

export const InsertMenuButtonTestId = 'insert-menu-button'
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

export const CanvasToolbarSearch = React.memo((props: CanvasToolbarSearchProps) => {
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

  return (
    <WindowedSelect
      id={'canvas-toolbar-search'}
      ref={selectRef}
      components={{ Option: CustomComponentOption }}
      openMenuOnFocus={true}
      openMenuOnClick={true}
      onBlur={undefined}
      onChange={props.actionWith}
      options={options}
      menuPortalTarget={menuPortalTarget}
      filterOption={createFilter({ ignoreAccents: true })}
      styles={{
        ...componentSelectorStyles,
        menuPortal: (styles: CSSObject): CSSObject => {
          return {
            zIndex: -2,
            padding: '0 8px',
            overflow: 'hidden',
            height: 'auto',
            backgroundColor: theme.bg2.value,
            borderRadius: '0px 10px 10px 10px',
            boxShadow: UtopiaTheme.panelStyles.shadows.medium,
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
            borderColor: theme.primary.value,
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
  )
})
CanvasToolbarSearch.displayName = 'CanvasToolbarSearch'

export const CanvasToolbarEditButtonID = 'canvas-toolbar-edit-button'

export const CanvasToolbar = React.memo(() => {
  const dispatch = useDispatch()
  const theme = useColorTheme()

  const canvasToolbarMode = useToolbarMode()

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
  const wrapInDivCallback = useWrapInDiv()

  const convertToCallback = useConvertTo()
  const toInsertCallback = useToInsert()

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

  // Back to select mode, close the "floating" menu and turn off the forced insert mode.
  const switchToSelectModeCloseMenus = React.useCallback(() => {
    dispatch(
      [switchEditorMode(EditorModes.selectMode(null, false, 'none')), closeFloatingInsertMenu()],
      'everyone',
    )
  }, [dispatch])

  const convertToAndClose = React.useCallback(
    (convertTo: InsertMenuItem | null) => {
      convertToCallback(convertTo)
      switchToSelectModeCloseMenus()
    },
    [convertToCallback, switchToSelectModeCloseMenus],
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
      },
    }
    convertToAndClose(convertToFragmentMenuItem)
  }, [convertToAndClose])

  const wrapInDivAndClose = React.useCallback(
    (event: React.MouseEvent<Element>) => {
      wrapInDivCallback(event)
      switchToSelectModeCloseMenus()
    },
    [switchToSelectModeCloseMenus, wrapInDivCallback],
  )

  const toInsertAndClose = React.useCallback(
    (toInsert: InsertMenuItem | null) => {
      toInsertCallback(toInsert)
      switchToSelectModeCloseMenus()
    },
    [switchToSelectModeCloseMenus, toInsertCallback],
  )

  const zoomLevel = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'CanvasToolbar zoomLevel',
  )

  const zoom100pct = React.useCallback(() => dispatch([CanvasActions.zoom(1)]), [dispatch])

  const isLiveMode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode.type === 'live',
    'TopMenu isLiveMode',
  )
  const toggleLiveMode = React.useCallback(() => {
    if (isLiveMode) {
      dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'none'))])
    } else {
      dispatch([switchEditorMode(EditorModes.liveMode())])
    }
  }, [dispatch, isLiveMode])

  const resetRemixApps = useResetRemixApps()

  const resetCanvasCallback = React.useCallback(() => {
    resetRemixApps()
    dispatch([resetCanvas()])
  }, [dispatch, resetRemixApps])

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

  const toggleInsertButtonClicked = React.useCallback(() => {
    if (canvasToolbarMode.primary === 'insert') {
      switchToSelectModeCloseMenus()
    } else {
      dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'pseudo-insert'))])
    }
  }, [canvasToolbarMode.primary, dispatch, switchToSelectModeCloseMenus])

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

  return (
    <div
      style={{
        display: 'flex',
        gap: 10,
        flexDirection: 'row',
      }}
      // Mouse events should never go through this component.
      onClick={stopPropagation}
      onMouseDown={focusCanvasOnMouseDown}
      onMouseUp={stopPropagation}
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
              iconType={editButtonIcon.type}
              iconCategory={editButtonIcon.category}
              primary={canvasToolbarMode.primary === 'edit'}
              onClick={switchToSelectModeCloseMenus}
              testid={CanvasToolbarEditButtonID}
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
              onClick={zoom100pct}
            >
              {zoomLevel}x
            </SquareButton>
          </Tooltip>
          <Tooltip title='Reset Canvas' placement='bottom'>
            <InsertModeButton
              iconType='refresh'
              iconCategory='semantic'
              onClick={resetCanvasCallback}
              keepActiveInLiveMode
            />
          </Tooltip>
          <ElementsOutsideVisibleAreaIndicator />
        </div>
        {/* Edit Mode submenus */}
        {when(
          canvasToolbarMode.primary === 'edit' && canvasToolbarMode.secondary === 'selected',
          <>
            {when(
              insertMenuMode === 'closed',
              wrapInSubmenu(
                <>
                  <Tooltip title='Wrap selection in Group (⌘G)' placement='bottom'>
                    <InsertModeButton iconType='group-open' onClick={wrapInGroupCallback} />
                  </Tooltip>
                  <Tooltip title='Wrap selection in an element' placement='bottom'>
                    <InsertModeButton
                      iconType='designtool-larger'
                      iconCategory='semantic'
                      onClick={openFloatingWrapInMenuCallback}
                    />
                  </Tooltip>
                  <Tooltip
                    title='Converts an element or component into another (C)'
                    placement='bottom'
                  >
                    <InsertModeButton
                      iconType='convertobject'
                      iconCategory='semantic'
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
                </>,
              ),
            )}
            {when(
              insertMenuMode === 'wrap',
              wrapInSubmenu(
                <>
                  <Tooltip title='Back' placement='bottom'>
                    <InsertModeButton
                      iconCategory='semantic'
                      iconType='icon-semantic-back'
                      onClick={switchToSelectModeCloseMenus}
                    />
                  </Tooltip>
                  <Tooltip title='Wrap selection' placement='bottom'>
                    <InsertModeButton
                      iconType='designtool-larger'
                      iconCategory='semantic'
                      onClick={NO_OP}
                      secondary={true}
                    />
                  </Tooltip>
                  <Tooltip title='Wrap in div' placement='bottom'>
                    <InsertModeButton iconType='div' onClick={wrapInDivAndClose} />
                  </Tooltip>
                  <Tile style={{ height: '100%' }}>
                    <CanvasToolbarSearch actionWith={convertToAndClose} />
                  </Tile>
                </>,
              ),
            )}
            {when(
              insertMenuMode === 'convert',
              wrapInSubmenu(
                <>
                  <Tooltip title='Back' placement='bottom'>
                    <InsertModeButton
                      iconCategory='semantic'
                      iconType='icon-semantic-back'
                      onClick={switchToSelectModeCloseMenus}
                    />
                  </Tooltip>
                  <Tooltip title='Convert selection' placement='bottom'>
                    <InsertModeButton
                      iconType='convertobject'
                      iconCategory='semantic'
                      onClick={NO_OP}
                      secondary={true}
                    />
                  </Tooltip>
                  <Tooltip title='Convert to Fragment' placement='bottom'>
                    <InsertModeButton iconType='fragment' onClick={convertToFragment} />
                  </Tooltip>
                  <Tile style={{ height: '100%' }}>
                    <CanvasToolbarSearch actionWith={convertToAndClose} />
                  </Tile>
                </>,
              ),
            )}
          </>,
        )}
        {when(
          canvasToolbarMode.primary === 'edit' && canvasToolbarMode.secondary === 'strategy-active',
          <StrategyIndicator />,
        )}
        {/* Insert Mode */}
        {canvasToolbarMode.primary === 'insert'
          ? wrapInSubmenu(
              <>
                <Tooltip title='Back' placement='bottom'>
                  <InsertModeButton
                    iconCategory='semantic'
                    iconType='icon-semantic-back'
                    onClick={switchToSelectModeCloseMenus}
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
              </>,
            )
          : null}
        {/* Live Mode */}
        {when(
          canvasToolbarMode.primary === 'play',
          <>
            <FlexRow
              data-testid='canvas-toolbar-submenu'
              style={{
                alignItems: 'start',
                marginLeft: 15,
                padding: '0 8px',
                height: 32,
                overflow: 'hidden',
                backgroundColor: colorTheme.bg2.value,
                borderRadius: '0px 0px 10px 10px',
                boxShadow: UtopiaTheme.panelStyles.shadows.medium,
                pointerEvents: 'initial',
                zIndex: -1, // it sits below the main menu row, but we want the main menu's shadow to cast over this one
              }}
            >
              <RemixNavigationBar />
            </FlexRow>
          </>,
        )}
        <ToolbarSearchListing />
      </FlexColumn>
    </div>
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
  const keepActiveInLiveMode = props.keepActiveInLiveMode ?? false
  const primary = props.primary ?? false
  const secondary = props.secondary ?? false
  const canvasInLiveMode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode.type === 'live',
    'CanvasToolbar canvasInLiveMode',
  )
  const iconCategory = props.iconCategory ?? 'element'

  return (
    <SquareButton
      data-testid={props.testid}
      style={{ ...props.style, height: 32, width: 32 }}
      primary={primary}
      spotlight={secondary}
      highlight
      onClick={props.onClick}
      disabled={canvasInLiveMode && !keepActiveInLiveMode}
      overriddenBackground={secondary ? colorTheme.bg5.value : undefined}
    >
      <Icn
        category={iconCategory}
        type={props.iconType}
        color={props.primary ? 'on-highlight-main' : 'main'}
        width={props.size ?? 18}
        height={props.size ?? 18}
        testId={props.testid == null ? undefined : `${props.testid}-icon`}
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
