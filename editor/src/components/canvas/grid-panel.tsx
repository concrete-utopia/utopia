import React from 'react'
import { colorTheme } from '../../uuiui'
import { LeftPaneComponent } from '../navigator/left-pane'
import { CodeEditorPane, RightPane } from './design-panel-root'
import { useGridPanelDragInfo, useGridPanelDropArea } from './grid-panels-dnd'
import type { StoredPanel, LayoutUpdate, GridPanelData } from './stored-layout'
import {
  GridPanelsNumberOfRows,
  GridPanelHorizontalGapHalf,
  GridPanelVerticalGapHalf,
} from './stored-layout'
import { useDispatch } from '../editor/store/dispatch-context'
import { clearHighlightedViews } from '../editor/actions/action-creators'

interface GridPanelProps {
  onDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => void
  canDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => boolean
  pane: GridPanelData
}

export const GridPanel = React.memo<GridPanelProps>((props) => {
  if (props.pane == null) {
    return null
  }
  return <GridPanelInner {...props} />
})
GridPanel.displayName = 'GridPanel'

const GridPanelInner = React.memo<GridPanelProps>((props) => {
  const { onDrop, canDrop } = props
  const { panel, index, span, order, visible } = props.pane

  const { isDragActive, draggedPanel } = useGridPanelDragInfo()

  const dropAboveElement: LayoutUpdate = React.useMemo(
    () => ({
      type: 'before-index',
      indexInColumn: order,
      columnIndex: index,
    }),
    [order, index],
  )

  const dropBelowElement: LayoutUpdate = React.useMemo(
    () => ({
      type: 'after-index',
      indexInColumn: order,
      columnIndex: index,
    }),
    [order, index],
  )

  const canDropAbove = draggedPanel != null && canDrop(draggedPanel, dropAboveElement)
  const canDropBelow = draggedPanel != null && canDrop(draggedPanel, dropBelowElement)

  const { drop: dropBefore, isOver: isOverBefore } = useGridPanelDropArea(
    React.useCallback(
      (itemToMove: StoredPanel) => onDrop(itemToMove, dropAboveElement),
      [onDrop, dropAboveElement],
    ),
  )
  const { drop: dropAfter, isOver: isOverAfter } = useGridPanelDropArea(
    React.useCallback(
      (itemToMove: StoredPanel) => {
        onDrop(itemToMove, dropBelowElement)
      },
      [onDrop, dropBelowElement],
    ),
  )

  const draggablePanelComponent = (() => {
    switch (panel.name) {
      case 'code-editor':
        return (
          <CodeEditorPane
            panelData={props.pane.panel}
            small={props.pane.span !== GridPanelsNumberOfRows} // it's only not small if it's full-height (span === number of rows)
          />
        )
      case 'inspector':
        return <RightPane panelData={props.pane.panel} />
      case 'navigator':
        return <LeftPaneComponent panelData={props.pane.panel} />
      default:
        return null
    }
  })()

  const dispatch = useDispatch()

  const onMouseEnter = React.useCallback(() => {
    dispatch([clearHighlightedViews()], 'everyone')
  }, [dispatch])

  return (
    <div
      style={{
        pointerEvents: 'initial',
        gridColumn: `col ${index > -1 ? index + 1 : index}`,
        gridRow: `span ${span}`,
        order: order,
        display: visible ? 'flex' : 'none',
        flexDirection: 'column',
        contain: 'layout',
        paddingLeft: GridPanelHorizontalGapHalf,
        paddingRight: GridPanelHorizontalGapHalf,
        paddingTop: GridPanelVerticalGapHalf,
        paddingBottom: GridPanelVerticalGapHalf,
      }}
      onMouseEnter={onMouseEnter}
    >
      {draggablePanelComponent}
      <div
        ref={dropBefore}
        style={{
          display: isDragActive && canDropAbove ? 'block' : 'none',
          position: 'absolute',
          width: '100%',
          height: `50%`,
          top: 0,
          left: 0,
        }}
      >
        <div
          style={{
            display: isOverBefore ? 'block' : 'none',
            position: 'absolute',
            top: -1,
            height: 2,
            width: '100%',
            backgroundColor: colorTheme.primary.value,
          }}
        />
      </div>
      <div
        ref={dropAfter}
        style={{
          display: isDragActive && canDropBelow ? 'block' : 'none',
          position: 'absolute',
          width: '100%',
          height: `50%`,
          bottom: 0,
          left: 0,
        }}
      >
        <div
          style={{
            display: isOverAfter ? 'block' : 'none',
            position: 'absolute',

            bottom: -1,
            height: 2,
            width: '100%',
            backgroundColor: colorTheme.primary.value,
          }}
        />
      </div>
    </div>
  )
})
GridPanelInner.displayName = 'GridPanelInner'
