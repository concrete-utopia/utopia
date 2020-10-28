import * as React from 'react'
import { EditorState } from '../editor/store/editor-state'
import {
  SimpleFlexColumn,
  Tooltip,
  SimpleTile,
  UtopiaStyles,
  SquareButton,
  UtopiaTheme,
  IcnProps,
  LargerIcons,
} from 'uuiui'
import { betterReactMemo, Utils } from 'uuiui-deps'
import { isLiveMode } from '../editor/editor-modes'
import { useEditorState } from '../editor/store/store-hook'
import * as EditorActions from '../editor/actions/actions'
import { EditorAction } from '../editor/action-types'
import CanvasActions from './canvas-actions'

export const enum RightMenuTab {
  Insert = 'insert',
  Inspector = 'inspector',
}

export function updateSelectedRightMenuTab(
  editorState: EditorState,
  tab: RightMenuTab,
): EditorState {
  return {
    ...editorState,
    rightMenu: {
      ...editorState.rightMenu,
      selectedTab: tab,
    },
  }
}

export function updateRightMenuExpanded(editorState: EditorState, expanded: boolean): EditorState {
  return {
    ...editorState,
    rightMenu: {
      ...editorState.rightMenu,
      expanded: expanded,
    },
  }
}

export interface RightMenuTileProps extends React.HTMLAttributes<HTMLDivElement> {
  selected: boolean
  highlightSelected: boolean
  icon: React.ReactElement<IcnProps>
}

export const RightMenuTile: React.FunctionComponent<RightMenuTileProps> = (props) => {
  const [hovered, setHovered] = React.useState(false)

  const handleOnMouseOver = React.useCallback(() => setHovered(true), [])
  const handleOnMouseOut = React.useCallback(() => setHovered(false), [])

  var foregroundColor: IcnProps['color'] = 'black'
  if (props.highlightSelected && props.selected) {
    foregroundColor = 'white'
  } else if (props.selected || hovered) {
    foregroundColor = 'blue'
  }

  return (
    <SimpleTile
      style={{
        ...props.style,
        width: UtopiaTheme.layout.rowHeight.mediumLarge,
        height: UtopiaTheme.layout.rowHeight.mediumLarge,
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

export const RightMenu = betterReactMemo('RightMenu', (props: RightMenuProps) => {
  const dispatch = useEditorState((store) => store.dispatch, 'RightMenu dispatch')
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
    <SimpleFlexColumn
      data-label='canvas-menu'
      style={{
        borderLeft: `1px solid #d3d3d369`,
        alignSelf: 'stretch',
        width: UtopiaTheme.layout.canvasMenuWidth,
      }}
    >
      <SimpleFlexColumn data-title='group' style={{ marginBottom: 24 }}>
        <Tooltip title={'Inspector'} placement='left'>
          <span>
            <RightMenuTile
              selected={isInspectorSelected}
              highlightSelected={isRightMenuExpanded}
              icon={<LargerIcons.Hamburgermenu />}
              onClick={onShowInspectorTab}
            />
          </span>
        </Tooltip>
        <Tooltip title={'Insert'} placement='left'>
          <span>
            <RightMenuTile
              selected={isInsertMenuSelected}
              highlightSelected={isRightMenuExpanded}
              icon={<LargerIcons.PlusButton />}
              onClick={onShowInsertTab}
            />
          </span>
        </Tooltip>
      </SimpleFlexColumn>
      <SimpleFlexColumn data-title='group' style={{ marginBottom: 24 }}>
        <Tooltip title={'Live Preview'} placement='left'>
          <span>
            <RightMenuTile
              selected={isCanvasLive}
              highlightSelected={false}
              icon={<LargerIcons.PlayButton />}
              onClick={toggleLiveCanvas}
            />
          </span>
        </Tooltip>
        <Tooltip title='Reset canvas' placement='left'>
          <span>
            <RightMenuTile
              selected={false}
              highlightSelected={false}
              icon={<LargerIcons.Refresh />}
            />
          </span>
        </Tooltip>
      </SimpleFlexColumn>

      <SimpleFlexColumn data-title='group' style={{ marginBottom: 24, flexGrow: 1 }}>
        <Tooltip title='Zoom in' placement='left'>
          <span>
            <RightMenuTile
              selected={false}
              highlightSelected={false}
              icon={<LargerIcons.MagnifyingGlassPlus />}
              onClick={zoomIn}
            />
          </span>
        </Tooltip>
        <span style={{ fontSize: 9, width: '100%', textAlign: 'center' }} onClick={zoom100pct}>
          {zoomLevel}x
        </span>
        <Tooltip title='Zoom out' placement='left'>
          <span>
            <RightMenuTile
              selected={false}
              highlightSelected={false}
              icon={<LargerIcons.MagnifyingGlassMinus />}
              onClick={zoomOut}
            />
          </span>
        </Tooltip>
      </SimpleFlexColumn>
      <SimpleFlexColumn data-title='group' style={{ marginBottom: 8 }}>
        <Tooltip title={'Show or hide extra canvas controls'} placement={'left'}>
          <span>
            <RightMenuTile
              selected={isAdditionalControlsVisible}
              highlightSelected={false}
              icon={<LargerIcons.Canvas />}
              onClick={toggleAdditionalControlsVisible}
            />
          </span>
        </Tooltip>
        <Tooltip title={'Show or hide code'} placement={'left'}>
          <span>
            <RightMenuTile
              selected={isCodePaneVisible}
              highlightSelected={false}
              icon={<LargerIcons.Code />}
              onClick={toggleCodePaneVisible}
            />
          </span>
        </Tooltip>
        <Tooltip title={'Show or hide preview'} placement={'left'}>
          <span>
            <RightMenuTile
              selected={isPreviewPaneVisible}
              highlightSelected={false}
              icon={<LargerIcons.PreviewPane />}
              onClick={togglePreviewPaneVisible}
            />
          </span>
        </Tooltip>
      </SimpleFlexColumn>
    </SimpleFlexColumn>
  )
})
