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

export const InsertConditionalButtonTestId = 'insert-mode-conditional'

export const CanvasToolbarId = 'canvas-toolbar'

export const CanvasToolbar = React.memo(() => {
  const dispatch = useDispatch()
  const theme = useColorTheme()

  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)

  const divInsertion = useCheckInsertModeForElementType('div')
  const insertDivCallback = useEnterDrawToInsertForDiv()
  const imgInsertion = useCheckInsertModeForElementType('img')
  const insertImgCallback = useEnterDrawToInsertForImage()
  const textInsertion = useCheckInsertModeForElementType('span', { textEdit: true })
  const insertTextCallback = useEnterTextEditMode()
  const buttonInsertion = useCheckInsertModeForElementType('button')
  const insertButtonCallback = useEnterDrawToInsertForButton()
  const conditionalInsertion = useCheckInsertModeForElementType('div', { wrapInConditional: true })
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

  const wrapInDivCallback = React.useCallback(() => {
    dispatch([
      wrapInElement(selectedViewsRef.current, {
        element: defaultTransparentViewElement(
          generateUidWithExistingComponents(projectContentsRef.current),
        ),
        importsToAdd: {},
      }),
    ])
  }, [dispatch, selectedViewsRef, projectContentsRef])

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

  const toggleCodeEditorVisible = React.useCallback(
    () => dispatch([togglePanel('codeEditor')]),
    [dispatch],
  )

  const toggleInspectorVisible = React.useCallback(() => {
    dispatch([togglePanel('rightmenu')])
  }, [dispatch])

  const toggleNavigatorVisible = React.useCallback(() => {
    dispatch([togglePanel('navigator')])
  }, [dispatch])

  return (
    <FlexColumn
      id={CanvasToolbarId}
      style={{
        gap: 6,
        alignItems: 'stretch',
        width: 64,
        backgroundColor: theme.inspectorBackground.value,
        borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
        boxShadow: `3px 4px 10px 0px ${UtopiaTheme.panelStyles.panelShadowColor}`,
        pointerEvents: 'initial',
      }}
      onMouseDown={stopPropagation}
      onClick={stopPropagation}
    >
      <FlexColumn style={{ padding: 4 }}>
        {/* ------------------------------------ */}
        <header style={{ paddingLeft: 4, fontSize: 10, fontWeight: 500 }}>Tools</header>
        <FlexRow style={{ flexWrap: 'wrap', gap: 4, padding: 4 }}>
          <Tooltip title='Select' placement='bottom'>
            <InsertModeButton
              iconType='pointer'
              iconCategory='tools'
              onClick={clickSelectModeButton}
            />
          </Tooltip>
          <Tooltip title='Insert text' placement='bottom'>
            <InsertModeButton
              iconType='pure-text'
              primary={textInsertion}
              onClick={insertTextCallback}
            />
          </Tooltip>
          <Tooltip title='Zoom in' placement='bottom'>
            <InsertModeButton
              iconType='magnifyingglass-plus'
              iconCategory='semantic'
              onClick={zoomIn}
            />
          </Tooltip>
          <Tooltip title='Zoom out' placement='bottom'>
            <InsertModeButton
              iconType='magnifyingglass-minus'
              iconCategory='semantic'
              onClick={zoomOut}
            />
          </Tooltip>
          <Tooltip title='Zoom to 100%' placement='bottom'>
            <SquareButton
              highlight
              style={{ textAlign: 'center', width: '100%' }}
              onClick={zoom100pct}
            >
              {zoomLevel * 100}%
            </SquareButton>
          </Tooltip>
        </FlexRow>
      </FlexColumn>

      {/* ------------------------------------ */}
      <FlexColumn style={{ padding: 4 }}>
        <header style={{ paddingLeft: 4, fontSize: 10, fontWeight: 500 }}>Insert</header>
        <FlexRow style={{ flexWrap: 'wrap', gap: 4, padding: 4 }}>
          <Tooltip title='Insert div' placement='bottom'>
            <InsertModeButton iconType='view' primary={divInsertion} onClick={insertDivCallback} />
          </Tooltip>
          <Tooltip title='Insert image' placement='bottom'>
            <InsertModeButton iconType='image' primary={imgInsertion} onClick={insertImgCallback} />
          </Tooltip>
          <Tooltip title='Insert button' placement='bottom'>
            <InsertModeButton
              iconType='clickable'
              primary={buttonInsertion}
              onClick={insertButtonCallback}
            />
          </Tooltip>
          <Tooltip title='Choose and insert a component' placement='bottom'>
            <InsertModeButton
              iconType='componentinstance'
              primary={insertMenuMode === 'insert'}
              onClick={openFloatingInsertMenuCallback}
            />
          </Tooltip>
          <Tooltip title='Insert conditional' placement='bottom'>
            <InsertModeButton
              testid={InsertConditionalButtonTestId}
              iconType='conditional'
              primary={conditionalInsertion}
              onClick={insertConditionalCallback}
            />
          </Tooltip>
          <Tooltip title='Open insert menu' placement='bottom'>
            <InsertModeButton
              iconType='dotdotdot'
              iconCategory='semantic'
              primary={insertMenuSelected}
              onClick={selectInsertMenuPane}
            />
          </Tooltip>
        </FlexRow>
      </FlexColumn>
      {/* ------------------------------------ */}
      <FlexColumn style={{ padding: 4 }}>
        <header style={{ paddingLeft: 4, fontSize: 10, fontWeight: 500 }}>Convert</header>
        <FlexRow style={{ flexWrap: 'wrap', gap: 4, padding: 4 }}>
          <Tooltip title='Converts an element or component into another' placement='bottom'>
            <InsertModeButton
              iconType='convertobject'
              iconCategory='semantic'
              primary={insertMenuMode === 'convert'}
              onClick={openFloatingConvertMenuCallback}
            />
          </Tooltip>
        </FlexRow>
      </FlexColumn>

      {/* ------------------------------------ */}
      <FlexColumn style={{ padding: 4 }}>
        <header style={{ paddingLeft: 4, fontSize: 10, fontWeight: 500 }}>Organise</header>
        <FlexRow style={{ flexWrap: 'wrap', gap: 4, padding: 4 }}>
          <Tooltip title='Wrap selection in div' placement='bottom'>
            <InsertModeButton iconType='group-open' onClick={wrapInDivCallback} />
          </Tooltip>
          <Tooltip title='Wrap selection in an element' placement='bottom'>
            <InsertModeButton
              iconType='designtool-larger'
              iconCategory='semantic'
              primary={insertMenuMode === 'wrap'}
              onClick={openFloatingWrapInMenuCallback}
            />
          </Tooltip>
        </FlexRow>
      </FlexColumn>
      {/* ------------------------------------ */}
      <FlexColumn style={{ padding: 4 }}>
        <header style={{ paddingLeft: 4, fontSize: 10, fontWeight: 500 }}>Editor</header>
        <FlexRow style={{ flexWrap: 'wrap', gap: 4, padding: 4 }}>
          <Tooltip title='Toggle Live Mode' placement='bottom'>
            <InsertModeButton
              iconType='playbutton'
              iconCategory='semantic'
              primary={isLiveMode}
              onClick={toggleLiveMode}
              keepActiveInLiveMode
            />
          </Tooltip>
          <Tooltip title='Reset Canvas' placement='bottom'>
            <InsertModeButton
              iconType='refresh'
              iconCategory='semantic'
              onClick={resetCanvasCallback}
            />
          </Tooltip>

          <Tooltip title='Toggle Navigator (⌘⌥1)' placement='bottom'>
            <InsertModeButton
              iconType='navigator-larger'
              iconCategory='semantic'
              onClick={toggleNavigatorVisible}
            />
          </Tooltip>
          <Tooltip title='Toggle Inspector (⌘⌥2)' placement='bottom'>
            <InsertModeButton
              iconType='inspector-larger'
              iconCategory='semantic'
              onClick={toggleInspectorVisible}
            />
          </Tooltip>
          <Tooltip title='Toggle Code Editor (⌘.)' placement='bottom'>
            <InsertModeButton
              iconType='codymccodeface-larger'
              iconCategory='semantic'
              onClick={toggleCodeEditorVisible}
            />
          </Tooltip>
        </FlexRow>
      </FlexColumn>
    </FlexColumn>
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
}
const InsertModeButton = React.memo((props: InsertModeButtonProps) => {
  const keepActiveInLiveMode = props.keepActiveInLiveMode ?? false
  const primary = props.primary ?? false
  const canvasInLiveMode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode.type === 'live',
    'CanvasToolbar canvasInLiveMode',
  )
  const iconCategory = props.iconCategory ?? 'element'

  return (
    <SquareButton
      data-testid={props.testid}
      style={{ ...props.style, borderRadius: 4 }}
      primary={primary}
      highlight
      onClick={props.onClick}
      disabled={canvasInLiveMode && !keepActiveInLiveMode}
    >
      <Icn
        category={iconCategory}
        type={props.iconType}
        color={primary ? 'on-highlight-main' : 'main'}
        width={18}
        height={18}
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
