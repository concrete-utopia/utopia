import * as React from 'react'
import { AlwaysTrue, usePubSubAtomReadOnly } from '../../core/shared/atom-with-pub-sub'
import {
  FlexColumn,
  FlexRow,
  Icn,
  IcnSpacer,
  SquareButton,
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
        <header style={{ paddingLeft: 4 }}>Insert</header>
        <FlexRow style={{ flexWrap: 'wrap', gap: 4, padding: 4 }}>
          <InsertModeButton iconType='view' primary={divInsertion} onClick={insertDivCallback} />
          <InsertModeButton iconType='image' primary={imgInsertion} onClick={insertImgCallback} />
          <InsertModeButton iconType='text' primary={spanInsertion} onClick={insertSpanCallback} />
          <InsertModeButton
            iconType='button'
            primary={buttonInsertion}
            onClick={insertButtonCallback}
          />
          <IcnSpacer height={0} width={'100%'} />
          <InsertModeButton
            iconType='componentinstance'
            primary={floatingInsertMenuOpen}
            onClick={openFloatingInsertMenuCallback}
          />
          <SquareButton
            primary={insertMenuSelected}
            highlight
            style={{
              color: insertMenuSelected ? theme.neutralInvertedForeground.value : theme.fg0.value,
            }}
            onClick={selectInsertMenuPane}
          >
            …
          </SquareButton>
        </FlexRow>
      </FlexColumn>
    </FlexColumn>
  )
})

interface InsertModeButtonProps {
  iconType: string
  primary: boolean
  onClick: (event: React.MouseEvent<Element>) => void
}
const InsertModeButton = React.memo((props: InsertModeButtonProps) => {
  return (
    <SquareButton primary={props.primary} highlight onClick={props.onClick}>
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
