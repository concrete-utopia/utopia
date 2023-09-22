import React from 'react'
import { NO_OP } from '../../core/shared/utils'
import { UtopiaTheme, colorTheme } from '../../uuiui'
import { LeftPanelMinWidth } from '../editor/store/editor-state'
import { LeftPaneComponent } from '../navigator/left-pane'
import { CodeEditorPane, ResizableRightPane } from './design-panel-root'
import { useGridPanelDragInfo, useGridPanelDropArea } from './grid-panels-dnd'
import type { GridPanelData, LayoutUpdate, StoredPanel } from './grid-panels-state'
import { GridPanelVerticalGapHalf, GridPanelsNumberOfRows } from './grid-panels-state'

interface GridPanelProps {
  onDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => void
  canDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => boolean
  pane: GridPanelData
}

export const GridPanel = React.memo<GridPanelProps>((props) => {
  const { onDrop, canDrop } = props
  const { panel, index, span, order } = props.pane

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
        return <ResizableRightPane panelData={props.pane.panel} />
      case 'navigator':
        return <LeftPaneComponent panelData={props.pane.panel} />
      default:
        return null
    }
  })()

  return (
    <div
      style={{
        pointerEvents: 'initial',
        gridColumn: `col ${index > -1 ? index + 1 : index}`,
        gridRow: `span ${span}`,
        order: order,
        display: 'flex',
        flexDirection: 'column',
        contain: 'layout',
      }}
    >
      {draggablePanelComponent}
      <div
        ref={dropBefore}
        style={{
          display: isDragActive && canDropAbove ? 'block' : 'none',
          position: 'absolute',
          width: '100%',
          height: `calc(50% + ${GridPanelVerticalGapHalf}px)`,
          top: -GridPanelVerticalGapHalf,
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
          height: `calc(50% + ${GridPanelVerticalGapHalf}px)`,
          bottom: -GridPanelVerticalGapHalf,
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
