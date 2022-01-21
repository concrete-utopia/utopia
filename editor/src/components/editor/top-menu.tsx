import React from 'react'
import {
  FlexRow,
  LargerIcons,
  MenuIcons,
  SimpleFlexRow,
  SquareButton,
  Tooltip,
  UNSAFE_getIconURL,
  UtopiaTheme,
} from '../../uuiui'
import { useEditorState } from './store/store-hook'
import * as EditorActions from '../editor/actions/action-creators'
import { Utils } from '../../uuiui-deps'
import { FormulaBar } from '../canvas/controls/formula-bar'
import CanvasActions from '../canvas/canvas-actions'
import { EditorAction } from './action-types'
import { ComponentOrInstanceIndicator } from '../editor/component-button'
import { IconToggleButton } from '../../uuiui/icon-toggle-button'
import { LeftPaneDefaultWidth, RightMenuTab, NavigatorWidthAtom } from './store/editor-state'
import { CanvasVector } from '../../core/shared/math-utils'
import { usePubSubAtomReadOnly } from '../../core/shared/atom-with-pub-sub'

function useShouldResetCanvas(invalidateCount: number): [boolean, (value: boolean) => void] {
  const [shouldResetCanvas, setShouldResetCanvas] = React.useState(false)
  const previousCanvasContentInvalidateCount = React.useRef(invalidateCount)

  if (previousCanvasContentInvalidateCount.current !== invalidateCount) {
    setShouldResetCanvas(true)
    previousCanvasContentInvalidateCount.current = invalidateCount
  }

  return [shouldResetCanvas, setShouldResetCanvas]
}

const TopMenuLeftControls = React.memo(() => {
  const dispatch = useEditorState((store) => store.dispatch, 'TopMenuLeftControls dispatch')
  const navigatorVisible = useEditorState(
    (store) => !store.editor.navigator.minimised,
    'TopMenuLeftControls navigatorVisible',
  )

  const navigatorWidth = usePubSubAtomReadOnly(NavigatorWidthAtom)

  const onClickNavigateTab = React.useCallback(() => {
    let actions: EditorAction[] = [EditorActions.togglePanel('navigator')]
    if (navigatorVisible) {
      actions.push(CanvasActions.scrollCanvas({ x: navigatorWidth, y: 0 } as CanvasVector))
    } else {
      actions.push(CanvasActions.scrollCanvas({ x: -navigatorWidth, y: 0 } as CanvasVector))
    }
    dispatch(actions)
  }, [dispatch, navigatorVisible, navigatorWidth])

  const followSelection = useEditorState(
    (store) => store.editor.config.followSelection,
    'TopMenu followSelection',
  )
  const onToggleFollow = React.useCallback(() => {
    dispatch([EditorActions.setFollowSelectionEnabled(!followSelection.enabled)])
  }, [dispatch, followSelection])

  return (
    <React.Fragment>
      <Tooltip title={'Toggle outline'} placement={'bottom'}>
        <SquareButton spotlight={false} highlight={true} onClick={onClickNavigateTab}>
          <MenuIcons.Navigator />
        </SquareButton>
      </Tooltip>
      <Tooltip title='Mirror selection between canvas and code editor' placement='bottom'>
        <IconToggleButton
          onToggle={onToggleFollow}
          value={followSelection.enabled}
          srcOn={UNSAFE_getIconURL('bracketed-pointer', 'blue', 'semantic', 18, 18)}
          srcOff={UNSAFE_getIconURL('bracketed-pointer', 'darkgray', 'semantic', 18, 18)}
        />
      </Tooltip>
      <ComponentOrInstanceIndicator />
    </React.Fragment>
  )
})

const TopMenuRightControls = React.memo(() => {
  const dispatch = useEditorState((store) => store.dispatch, 'TopMenuRightControls dispatch')
  const canvasContentInvalidateCount = useEditorState(
    (store) => store.editor.canvas.canvasContentInvalidateCount,
    'RightMenu canvasContentInvalidateCount',
  )

  const zoomLevel = useEditorState((store) => store.editor.canvas.scale, 'RightMenu zoomLevel')
  const zoomIn = React.useCallback(
    () => dispatch([CanvasActions.zoom(Utils.increaseScale(zoomLevel))]),
    [dispatch, zoomLevel],
  )
  const zoomOut = React.useCallback(
    () => dispatch([CanvasActions.zoom(Utils.decreaseScale(zoomLevel))]),
    [dispatch, zoomLevel],
  )
  const [shouldResetCanvas, setShouldResetCanvas] = useShouldResetCanvas(
    canvasContentInvalidateCount,
  )
  const resetCanvas = React.useCallback(() => {
    dispatch([EditorActions.resetCanvas()])
    setShouldResetCanvas(false)
  }, [dispatch, setShouldResetCanvas])

  const zoom100pct = React.useCallback(() => dispatch([CanvasActions.zoom(1)]), [dispatch])

  const rightMenuSelectedTab = useEditorState(
    (store) => store.editor.rightMenu.selectedTab,
    'RightMenu rightMenuSelectedTab',
  )

  const isInsertMenuSelected = rightMenuSelectedTab === RightMenuTab.Insert
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

  const onShowInsertTab = React.useCallback(() => {
    onShow(RightMenuTab.Insert)
  }, [onShow])

  const onShowInspectorTab = React.useCallback(() => {
    dispatch([EditorActions.togglePanel('rightmenu')])

    onShow(RightMenuTab.Inspector)
  }, [onShow, dispatch])

  return (
    <React.Fragment>
      <FlexRow>
        <Tooltip title='Zoom out' placement='left'>
          <span>
            <SquareButton highlight onClick={zoomOut}>
              <LargerIcons.MagnifyingGlassMinus />
            </SquareButton>
          </span>
        </Tooltip>
        <SquareButton
          highlight
          style={{ fontSize: 9, textAlign: 'center', width: 32 }}
          onClick={zoom100pct}
        >
          {zoomLevel}x
        </SquareButton>
        <Tooltip title='Zoom in' placement='left'>
          <span>
            <SquareButton highlight onClick={zoomIn}>
              <LargerIcons.MagnifyingGlassPlus />
            </SquareButton>
          </span>
        </Tooltip>
      </FlexRow>
      <Tooltip title='Reset canvas' placement='left'>
        <span>
          <SquareButton highlight onClick={resetCanvas}>
            <LargerIcons.Refresh />
          </SquareButton>
        </span>
      </Tooltip>

      <Tooltip title={'Insert'} placement='left'>
        <span>
          <SquareButton highlight onClick={onShowInsertTab}>
            <LargerIcons.PlusButton color={isInsertMenuSelected ? 'primary' : 'main'} />
          </SquareButton>
        </span>
      </Tooltip>

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
  return (
    <SimpleFlexRow
      style={{
        flexGrow: 1,
        gap: 12,
        paddingLeft: 8,
        paddingRight: 4,
        height: UtopiaTheme.layout.rowHeight.normal,
      }}
    >
      <TopMenuLeftControls />
      <FlexRow style={{ border: 1, flexGrow: 1 }}>
        <FormulaBar style={{ flexGrow: 1 }} />
      </FlexRow>
      <TopMenuRightControls />
    </SimpleFlexRow>
  )
})
