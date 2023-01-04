import * as React from 'react'
import { AlwaysTrue, usePubSubAtomReadOnly } from '../../core/shared/atom-with-pub-sub'
import {
  colorTheme,
  FlexColumn,
  FlexRow,
  Icn,
  IcnSpacer,
  SquareButton,
  Tooltip as TooltipWithoutSpanFixme,
  TooltipProps,
  useColorTheme,
  UtopiaStyles,
  UtopiaTheme,
} from '../../uuiui'
import { Utils } from '../../uuiui-deps'
import CanvasActions from '../canvas/canvas-actions'
import { stopPropagation } from '../inspector/common/inspector-utils'
import { EditorAction } from './action-types'
import {
  openFloatingInsertMenu,
  resetCanvas,
  setPanelVisibility,
  setRightMenuTab,
  switchEditorMode,
  wrapInView,
} from './actions/action-creators'
import { EditorModes } from './editor-modes'
import {
  useCheckInsertModeForElementType,
  useEnterDrawToInsertForButton,
  useEnterDrawToInsertForDiv,
  useEnterDrawToInsertForImage,
  useEnterTextEditMode,
} from './insert-callbacks'
import { FloatingInsertMenuState, NavigatorWidthAtom, RightMenuTab } from './store/editor-state'
import { useEditorState, useRefEditorState } from './store/store-hook'

export const CanvasToolbar = React.memo(() => {
  const dispatch = useEditorState((store) => store.dispatch, 'CanvasToolbar dispatch')
  const theme = useColorTheme()

  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)

  const divInsertion = useCheckInsertModeForElementType('div')
  const insertDivCallback = useEnterDrawToInsertForDiv()
  const imgInsertion = useCheckInsertModeForElementType('img')
  const insertImgCallback = useEnterDrawToInsertForImage()
  const spanInsertion = useCheckInsertModeForElementType('span')
  const insertSpanCallback = useEnterTextEditMode()
  const buttonInsertion = useCheckInsertModeForElementType('button')
  const insertButtonCallback = useEnterDrawToInsertForButton()

  const insertMenuMode = useEditorState(
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
    dispatch([wrapInView(selectedViewsRef.current, 'default-empty-div')])
  }, [dispatch, selectedViewsRef])

  const insertMenuSelected = useEditorState(
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

  const zoomLevel = useEditorState((store) => store.editor.canvas.scale, 'CanvasToolbar zoomLevel')

  const zoomIn = React.useCallback(
    () => dispatch([CanvasActions.zoom(Utils.increaseScale(zoomLevel))]),
    [dispatch, zoomLevel],
  )
  const zoomOut = React.useCallback(
    () => dispatch([CanvasActions.zoom(Utils.decreaseScale(zoomLevel))]),
    [dispatch, zoomLevel],
  )

  const isLiveMode = useEditorState(
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

  const isCodeEditorVisible = useEditorState(
    (store) => store.editor.interfaceDesigner.codePaneVisible,
    'CanvasToolbar isCodeEditorVisible',
  )
  const toggleCodeEditorVisible = React.useCallback(
    () => dispatch([setPanelVisibility('codeEditor', !isCodeEditorVisible)]),
    [dispatch, isCodeEditorVisible],
  )

  return (
    <FlexColumn
      style={{
        position: 'absolute',
        top: 48,
        left: 12,
        alignItems: 'stretch',
        width: 64,
        borderRadius: 4,
        backgroundColor: theme.bg0.value,
        boxShadow: UtopiaStyles.popup.boxShadow,
        pointerEvents: 'initial',
      }}
      onMouseDown={stopPropagation}
      onClick={stopPropagation}
    >
      <FlexColumn style={{ padding: 4 }}>
        {/* TODO is there a component for this subheading? */}
        <header style={{ paddingLeft: 4, fontSize: 10, fontWeight: 500 }}>Insert</header>
        <FlexRow style={{ flexWrap: 'wrap', gap: 4, padding: 4 }}>
          <Tooltip title='Insert div' placement='bottom'>
            <InsertModeButton iconType='view' primary={divInsertion} onClick={insertDivCallback} />
          </Tooltip>
          <Tooltip title='Insert image' placement='bottom'>
            <InsertModeButton iconType='image' primary={imgInsertion} onClick={insertImgCallback} />
          </Tooltip>
          <Tooltip title='Insert text' placement='bottom'>
            <InsertModeButton
              iconType='text'
              primary={spanInsertion}
              onClick={insertSpanCallback}
            />
          </Tooltip>
          <Tooltip title='Insert button' placement='bottom'>
            <InsertModeButton
              iconType='button'
              primary={buttonInsertion}
              onClick={insertButtonCallback}
            />
          </Tooltip>
          {/* TODO I have to find a better spacer */}
          <IcnSpacer height={0} width={'100%'} />
          <Tooltip title='Choose and insert a component' placement='bottom'>
            <InsertModeButton
              iconType='componentinstance'
              primary={insertMenuMode === 'insert'}
              onClick={openFloatingInsertMenuCallback}
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
      <Divider />
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
      <Divider />
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
      <Divider />
      {/* ------------------------------------ */}
      <FlexColumn style={{ padding: 4 }}>
        <header style={{ paddingLeft: 4, fontSize: 10, fontWeight: 500 }}>Zoom</header>
        <FlexRow style={{ flexWrap: 'wrap', gap: 4, padding: 4 }}>
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
        </FlexRow>
      </FlexColumn>
      <Divider />
      {/* ------------------------------------ */}
      <FlexColumn style={{ padding: 4 }}>
        <header style={{ paddingLeft: 4, fontSize: 10, fontWeight: 500 }}>Editor</header>
        <FlexRow style={{ flexWrap: 'wrap', gap: 4, padding: 4 }}>
          <Tooltip title='Toggle between live and edit mode' placement='bottom'>
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
          <Tooltip title='Show/hide code editor' placement='bottom'>
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
  onClick: (event: React.MouseEvent<Element>) => void
}
const InsertModeButton = React.memo((props: InsertModeButtonProps) => {
  const keepActiveInLiveMode = props.keepActiveInLiveMode ?? false
  const primary = props.primary ?? false
  const canvasInLiveMode = useEditorState(
    (store) => store.editor.mode.type === 'live',
    'CanvasToolbar canvasInLiveMode',
  )
  const iconCategory = props.iconCategory ?? 'element'

  return (
    <SquareButton
      style={{ borderRadius: 4 }}
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

const Divider = () => {
  return (
    <div
      style={{
        height: 1,
        width: '100%',
        marginTop: 8,
        backgroundColor: colorTheme.fg9.value,
      }}
    />
  )
}
