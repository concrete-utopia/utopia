import React from 'react'
import {
  colorTheme,
  FlexRow,
  LargerIcons,
  MenuIcons,
  SimpleFlexRow,
  SquareButton,
  Tooltip,
  UtopiaTheme,
} from '../../uuiui'
import { useEditorState } from './store/store-hook'
import * as EditorActions from '../editor/actions/action-creators'
import { Utils } from '../../uuiui-deps'
import { FormulaBar } from '../canvas/controls/formula-bar'
import CanvasActions from '../canvas/canvas-actions'
import { EditorAction } from './action-types'
import { ComponentOrInstanceIndicator } from '../editor/component-button'
import { RightMenuTab, NavigatorWidthAtom } from './store/editor-state'
import { CanvasVector } from '../../core/shared/math-utils'
import { AlwaysTrue, usePubSubAtomReadOnly } from '../../core/shared/atom-with-pub-sub'
import { toString } from '../../core/shared/element-path'

const TopMenuLeftControls = React.memo(() => {
  const dispatch = useEditorState((store) => store.dispatch, 'TopMenuLeftControls dispatch')
  const navigatorVisible = useEditorState(
    (store) => !store.editor.navigator.minimised,
    'TopMenuLeftControls navigatorVisible',
  )

  const navigatorWidth = usePubSubAtomReadOnly(NavigatorWidthAtom, AlwaysTrue)

  const onClickNavigateTab = React.useCallback(() => {
    let actions: EditorAction[] = [EditorActions.togglePanel('navigator')]
    if (navigatorVisible) {
      actions.push(CanvasActions.scrollCanvas({ x: navigatorWidth, y: 0 } as CanvasVector))
    } else {
      actions.push(CanvasActions.scrollCanvas({ x: -navigatorWidth, y: 0 } as CanvasVector))
    }
    dispatch(actions)
  }, [dispatch, navigatorVisible, navigatorWidth])

  return (
    <React.Fragment>
      <Tooltip title={'Toggle outline'} placement={'bottom'}>
        <SquareButton spotlight={false} highlight={true} onClick={onClickNavigateTab}>
          <MenuIcons.Navigator />
        </SquareButton>
      </Tooltip>
    </React.Fragment>
  )
})

const TopMenuRightControls = React.memo(() => {
  const dispatch = useEditorState((store) => store.dispatch, 'TopMenuRightControls dispatch')
  const zoomLevel = useEditorState((store) => store.editor.canvas.scale, 'RightMenu zoomLevel')
  const zoom100pct = React.useCallback(() => dispatch([CanvasActions.zoom(1)]), [dispatch])

  const rightMenuSelectedTab = useEditorState(
    (store) => store.editor.rightMenu.selectedTab,
    'RightMenu rightMenuSelectedTab',
  )

  const isInspectorSelected = rightMenuSelectedTab === RightMenuTab.Inspector

  const onShow = React.useCallback(
    (menuTab: RightMenuTab) => {
      let actions: Array<EditorAction> = []
      if (rightMenuSelectedTab !== menuTab) {
        actions.push(EditorActions.setPanelVisibility('rightmenu', true))
      }
      actions.push(EditorActions.setRightMenuTab(menuTab))
      dispatch(actions)
    },
    [dispatch, rightMenuSelectedTab],
  )

  const onShowInspectorTab = React.useCallback(() => {
    dispatch([EditorActions.togglePanel('rightmenu')])

    onShow(RightMenuTab.Inspector)
  }, [onShow, dispatch])

  return (
    <React.Fragment>
      <FlexRow>
        <SquareButton
          highlight
          style={{ fontSize: 9, textAlign: 'center', width: 32 }}
          onClick={zoom100pct}
        >
          {zoomLevel}x
        </SquareButton>
      </FlexRow>

      <Tooltip title={'Inspector'} placement='left'>
        <span>
          <SquareButton highlight onClick={onShowInspectorTab}>
            <LargerIcons.Hamburgermenu color={isInspectorSelected ? 'primary' : 'main'} />
          </SquareButton>
        </span>
      </Tooltip>
    </React.Fragment>
  )
})

export const TopMenu = React.memo(() => {
  const selectedElementPathString = useEditorState(
    (store) => Utils.optionalMap(toString, store.editor.selectedViews[0]) ?? 'empty',
    'First selected view or default',
  )

  return (
    <SimpleFlexRow
      style={{
        width: '100%',
        gap: 12,
        paddingLeft: 8,
        paddingRight: 4,
        height: UtopiaTheme.layout.rowHeight.normal,
        borderBottom: `1px solid ${colorTheme.subduedBorder.value}`,
      }}
    >
      <TopMenuLeftControls />
      <FlexRow style={{ border: 1, flexGrow: 1 }}>
        <FormulaBar key={selectedElementPathString} style={{ flexGrow: 1 }} />
      </FlexRow>
      <TopMenuRightControls />
    </SimpleFlexRow>
  )
})
