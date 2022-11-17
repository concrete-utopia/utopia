import * as React from 'react'
import { AlwaysTrue, usePubSubAtomReadOnly } from '../../core/shared/atom-with-pub-sub'
import {
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
import { EditorAction } from './action-types'
import {
  openFloatingInsertMenu,
  setPanelVisibility,
  setRightMenuTab,
} from './actions/action-creators'
import {
  useCheckInsertModeForElementType,
  useEnterDrawToInsertForButton,
  useEnterDrawToInsertForDiv,
  useEnterDrawToInsertForImage,
  useEnterDrawToInsertForSpan,
} from './insert-callbacks'
import { NavigatorWidthAtom, RightMenuTab } from './store/editor-state'
import { useEditorState } from './store/store-hook'

export const CanvasToolbar = React.memo(() => {
  const dispatch = useEditorState((store) => store.dispatch, 'CanvasToolbar dispatch')
  const theme = useColorTheme()

  const canvasInLiveMode = useEditorState(
    (store) => store.editor.mode.type === 'live',
    'CanvasToolbar canvasInLiveMode',
  )

  const navigatorWidth = usePubSubAtomReadOnly(NavigatorWidthAtom, AlwaysTrue)
  const navigatorVisible = useEditorState(
    (store) => !store.editor.navigator.minimised,
    'CanvasToolbar navigatorVisible',
  )
  const effectiveNavigatorWidth = navigatorVisible ? navigatorWidth : 0

  const topMenuHeight = UtopiaTheme.layout.rowHeight.normal

  const divInsertion = useCheckInsertModeForElementType('div')
  const insertDivCallback = useEnterDrawToInsertForDiv()
  const imgInsertion = useCheckInsertModeForElementType('img')
  const insertImgCallback = useEnterDrawToInsertForImage()
  const spanInsertion = useCheckInsertModeForElementType('span')
  const insertSpanCallback = useEnterDrawToInsertForSpan()
  const buttonInsertion = useCheckInsertModeForElementType('button')
  const insertButtonCallback = useEnterDrawToInsertForButton()

  const floatingInsertMenuOpen = useEditorState(
    (store) => store.editor.floatingInsertMenu.insertMenuMode !== 'closed',
    'CanvasToolbar floatingInsertMenuOpen',
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

  return (
    <FlexColumn
      style={{
        position: 'absolute',
        top: topMenuHeight + 8,
        left: effectiveNavigatorWidth + 8,
        alignItems: 'stretch',
        width: 64,
        padding: 4,
        gap: 4,
        borderRadius: 4,
        backgroundColor: theme.bg0.value,
        boxShadow: UtopiaStyles.popup.boxShadow,
      }}
    >
      <FlexColumn>
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
          <Tooltip title='Insert component...' placement='bottom'>
            <InsertModeButton
              iconType='componentinstance'
              primary={floatingInsertMenuOpen}
              onClick={openFloatingInsertMenuCallback}
            />
          </Tooltip>
          <Tooltip title='Open insert menu...' placement='bottom'>
            <SquareButton
              primary={insertMenuSelected}
              highlight
              style={{
                borderRadius: 4,
                color: insertMenuSelected ? theme.neutralInvertedForeground.value : theme.fg0.value,
              }}
              disabled={canvasInLiveMode}
              onClick={selectInsertMenuPane}
            >
              …
            </SquareButton>
          </Tooltip>
        </FlexRow>
      </FlexColumn>
    </FlexColumn>
  )
})

interface InsertModeButtonProps {
  iconType: string
  primary: boolean
  keepActiveInLiveMode?: boolean
  onClick: (event: React.MouseEvent<Element>) => void
}
const InsertModeButton = React.memo((props: InsertModeButtonProps) => {
  const keepActiveInLiveMode = props.keepActiveInLiveMode ?? false
  const canvasInLiveMode = useEditorState(
    (store) => store.editor.mode.type === 'live',
    'CanvasToolbar canvasInLiveMode',
  )

  return (
    <SquareButton
      style={{ borderRadius: 4 }}
      primary={props.primary}
      highlight
      onClick={props.onClick}
      disabled={canvasInLiveMode && !keepActiveInLiveMode}
    >
      <Icn
        category='element'
        type={props.iconType}
        color={props.primary ? 'on-highlight-main' : 'main'}
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
