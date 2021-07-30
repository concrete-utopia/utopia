import * as React from 'react'
import { EditorState, RightMenuTab } from '../editor/store/editor-state'
import { isLiveMode } from '../editor/editor-modes'
import { useEditorState } from '../editor/store/store-hook'
import * as EditorActions from '../editor/actions/action-creators'
import { EditorAction } from '../editor/action-types'
import CanvasActions from './canvas-actions'
import {
  IcnProps,
  SimpleTile,
  UtopiaTheme,
  SquareButton,
  UtopiaStyles,
  Tooltip,
  LargerIcons,
  FlexColumn,
  useColorTheme,
} from '../../uuiui'
import { betterReactMemo, Utils } from '../../uuiui-deps'
import { MenuTile } from '../menubar/menubar'

export interface RightMenuTileProps extends React.HTMLAttributes<HTMLDivElement> {
  selected: boolean
  highlightSelected: boolean
  icon: React.ReactElement<IcnProps>
}

export const RightMenuTile: React.FunctionComponent<RightMenuTileProps> = (props) => {
  const [hovered, setHovered] = React.useState(false)

  const handleOnMouseOver = React.useCallback(() => setHovered(true), [])
  const handleOnMouseOut = React.useCallback(() => setHovered(false), [])

  var foregroundColor: IcnProps['color'] = 'main'
  if (props.highlightSelected && props.selected) {
    foregroundColor = 'on-highlight-main'
  } else if (props.selected || hovered) {
    foregroundColor = 'primary'
  }

  return (
    <SimpleTile
      style={{
        ...props.style,
        width: 38,
        height: 38,
      }}
      onMouseOver={handleOnMouseOver}
      onMouseOut={handleOnMouseOut}
      onClick={props.onClick}
    >
      <SquareButton
        highlight
        style={{
          ...props.style,
          borderRadius: 1,
          width: 24,
          height: 24,
          background:
            props.highlightSelected && props.selected
              ? UtopiaStyles.backgrounds.blue
              : 'transparent',
        }}
      >
        {React.cloneElement(props.icon, {
          color: foregroundColor,
        })}
      </SquareButton>
    </SimpleTile>
  )
}

interface RightMenuProps {
  visible: boolean
}

function useShouldResetCanvas(
  canvasContentInvalidateCount: number,
): [boolean, (value: boolean) => void] {
  const [shouldResetCanvas, setShouldResetCanvas] = React.useState(false)
  const previousCanvasContentInvalidateCount = React.useRef(canvasContentInvalidateCount)

  if (previousCanvasContentInvalidateCount.current !== canvasContentInvalidateCount) {
    setShouldResetCanvas(true)
    previousCanvasContentInvalidateCount.current = canvasContentInvalidateCount
  }

  return [shouldResetCanvas, setShouldResetCanvas]
}

export const RightMenu = betterReactMemo('RightMenu', (props: RightMenuProps) => {
  const colorTheme = useColorTheme()
  const dispatch = useEditorState((store) => store.dispatch, 'RightMenu dispatch')
  const canvasContentInvalidateCount = useEditorState(
    (store) => store.editor.canvas.canvasContentInvalidateCount,
    'RightMenu canvasContentInvalidateCount',
  )
  const interfaceDesigner = useEditorState(
    (store) => store.editor.interfaceDesigner,
    'RightMenu interfaceDesigner',
  )

  const isRightMenuExpanded = useEditorState(
    (store) => store.editor.rightMenu.expanded,
    'RightMenu isRightMenuExpanded',
  )
  const rightMenuSelectedTab = useEditorState(
    (store) => store.editor.rightMenu.selectedTab,
    'RightMenu rightMenuSelectedTab',
  )

  const isInsertMenuSelected = rightMenuSelectedTab === RightMenuTab.Insert
  const isInspectorSelected = rightMenuSelectedTab === RightMenuTab.Inspector

  const isCodePaneVisible = interfaceDesigner.codePaneVisible
  const toggleCodePaneVisible = React.useCallback(
    () => dispatch([EditorActions.toggleInterfaceDesignerCodeEditor()]),
    [dispatch],
  )

  const isCanvasLive = useEditorState(
    (store) => isLiveMode(store.editor.mode),
    'RightMenu isCanvasLive',
  )
  const toggleLiveCanvas = React.useCallback(() => dispatch([EditorActions.toggleCanvasIsLive()]), [
    dispatch,
  ])

  const [shouldResetCanvas, setShouldResetCanvas] = useShouldResetCanvas(
    canvasContentInvalidateCount,
  )
  const resetCanvas = React.useCallback(() => {
    dispatch([EditorActions.resetCanvas()])
    setShouldResetCanvas(false)
  }, [dispatch, setShouldResetCanvas])

  const isPreviewPaneVisible = useEditorState(
    (store) => store.editor.preview.visible,
    'RightMenu isPreviewPaneVisible',
  )
  const togglePreviewPaneVisible = React.useCallback(
    () => dispatch([EditorActions.setPanelVisibility('preview', !isPreviewPaneVisible)]),
    [dispatch, isPreviewPaneVisible],
  )

  const isAdditionalControlsVisible = useEditorState(
    (store) => store.editor.interfaceDesigner.additionalControls,
    'RightMenu isAdditionalControlsVisible',
  )
  const toggleAdditionalControlsVisible = React.useCallback(() => {
    dispatch([EditorActions.toggleInterfaceDesignerAdditionalControls()])
  }, [dispatch])

  const onShow = React.useCallback(
    (menuTab: RightMenuTab) => {
      let actions: Array<EditorAction> = []
      if (rightMenuSelectedTab === menuTab) {
        actions.push(EditorActions.togglePanel('rightmenu'))
      } else {
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
    onShow(RightMenuTab.Inspector)
  }, [onShow])

  const zoomLevel = useEditorState((store) => store.editor.canvas.scale, 'RightMenu zoomLevel')
  const zoomIn = React.useCallback(
    () => dispatch([CanvasActions.zoom(Utils.increaseScale(zoomLevel))]),
    [dispatch, zoomLevel],
  )
  const zoomOut = React.useCallback(
    () => dispatch([CanvasActions.zoom(Utils.decreaseScale(zoomLevel))]),
    [dispatch, zoomLevel],
  )

  const zoom100pct = React.useCallback(() => dispatch([CanvasActions.zoom(1)]), [dispatch])

  return (
    <FlexColumn
      id='canvas-menu'
      style={{
        alignSelf: 'stretch',
        borderLeft: `1px solid ${colorTheme.subduedBorder.value}`,
        width: 38,
      }}
    >
      <FlexColumn style={{ flexGrow: 1, width: 38, overflowX: 'scroll' }}>
        <Tooltip title={'Inspector'} placement='left'>
          <span>
            <MenuTile
              selected={isInspectorSelected}
              menuExpanded={isRightMenuExpanded}
              icon={<LargerIcons.Hamburgermenu />}
              onClick={onShowInspectorTab}
              size='large'
            />
          </span>
        </Tooltip>

        <Tooltip title={'Insert'} placement='left'>
          <span>
            <MenuTile
              selected={isInsertMenuSelected}
              menuExpanded={isRightMenuExpanded}
              icon={<LargerIcons.PlusButton />}
              onClick={onShowInsertTab}
              size='large'
            />
          </span>
        </Tooltip>

        <Tooltip title={'Live Preview'} placement='left'>
          <span>
            <MenuTile
              selected={isCanvasLive}
              menuExpanded={false}
              icon={<LargerIcons.PlayButton />}
              onClick={toggleLiveCanvas}
              size='large'
            />
          </span>
        </Tooltip>

        <Tooltip title='Reset canvas' placement='left'>
          <span>
            <MenuTile
              selected={shouldResetCanvas}
              menuExpanded={false}
              icon={<LargerIcons.Refresh />}
              onClick={resetCanvas}
              size='large'
            />
          </span>
        </Tooltip>

        <Tooltip title='Zoom in' placement='left'>
          <span>
            <MenuTile
              selected={false}
              menuExpanded={false}
              icon={<LargerIcons.MagnifyingGlassPlus />}
              onClick={zoomIn}
              size='large'
            />
          </span>
        </Tooltip>

        <span style={{ fontSize: 9, width: '100%', textAlign: 'center' }} onClick={zoom100pct}>
          {zoomLevel}x
        </span>

        <Tooltip title='Zoom out' placement='left'>
          <span>
            <MenuTile
              selected={false}
              menuExpanded={false}
              icon={<LargerIcons.MagnifyingGlassMinus />}
              onClick={zoomOut}
              size='large'
            />
          </span>
        </Tooltip>

        <Tooltip title={'Show or hide extra canvas controls'} placement={'left'}>
          <span>
            <MenuTile
              selected={isAdditionalControlsVisible}
              menuExpanded={false}
              icon={<LargerIcons.Canvas />}
              onClick={toggleAdditionalControlsVisible}
              size='large'
            />
          </span>
        </Tooltip>

        <Tooltip title={'Show or hide code'} placement={'left'}>
          <span>
            <MenuTile
              selected={isCodePaneVisible}
              menuExpanded={false}
              icon={<LargerIcons.Code />}
              onClick={toggleCodePaneVisible}
              size='large'
            />
          </span>
        </Tooltip>

        <Tooltip title={'Show or hide preview'} placement={'left'}>
          <span>
            <MenuTile
              selected={isPreviewPaneVisible}
              menuExpanded={false}
              icon={<LargerIcons.PreviewPane />}
              onClick={togglePreviewPaneVisible}
              size='large'
            />
          </span>
        </Tooltip>
      </FlexColumn>
    </FlexColumn>
  )
})
