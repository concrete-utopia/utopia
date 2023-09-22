import React from 'react'
import { NO_OP, assertNever } from '../../core/shared/utils'
import { UtopiaTheme, colorTheme } from '../../uuiui'
import { LeftPanelMinWidth } from '../editor/store/editor-state'
import { LeftPaneComponent } from '../navigator/left-pane'
import { CodeEditorPane, ResizableRightPane } from './design-panel-root'
import { useGridPanelDragInfo, useGridPanelDropArea } from './grid-panels-dnd'
import type { GridPanelData, LayoutUpdate, StoredPanel } from './grid-panels-state'
import {
  GridPanelHorizontalGapHalf,
  GridPanelVerticalGapHalf,
  GridPanelsNumberOfRows,
} from './grid-panels-state'

interface GridPanelProps {
  onDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => void
  canDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => boolean
  onHover: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => void
  pane: GridPanelData
}

export const GridPanel = React.memo<GridPanelProps>((props) => {
  if (props.pane == null) {
    // todo make it nullable in the type
    return null
  }
  return <GridPanelInner {...props} />
})

const GridPanelInner = React.memo<GridPanelProps>((props) => {
  const { onDrop, canDrop, onHover } = props
  const { panel, index, span, order } = props.pane

  const { isDragActive, draggedPanel } = useGridPanelDragInfo()

  const isDraggingThisPanel =
    isDragActive && draggedPanel != null && draggedPanel.name === panel.name

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
    React.useCallback(
      (itemToMove: StoredPanel) => onHover(itemToMove, dropAboveElement),
      [onHover, dropAboveElement],
    ),
  )
  const { drop: dropAfter, isOver: isOverAfter } = useGridPanelDropArea(
    React.useCallback(
      (itemToMove: StoredPanel) => {
        onDrop(itemToMove, dropBelowElement)
      },
      [onDrop, dropBelowElement],
    ),
    React.useCallback(
      (itemToMove: StoredPanel) => {
        onHover(itemToMove, dropBelowElement)
      },
      [onHover, dropBelowElement],
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
        assertNever(panel.name)
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
        marginLeft: GridPanelHorizontalGapHalf,
        marginRight: GridPanelHorizontalGapHalf,
        marginTop: GridPanelVerticalGapHalf,
        marginBottom: GridPanelVerticalGapHalf,
      }}
    >
      {draggablePanelComponent}
      <div
        style={{
          position: 'absolute',
          display: isDraggingThisPanel ? 'block' : 'none',
          width: '100%',
          height: '100%',
          backgroundColor: '#f4f4f4',
          border: '1px solid rgb(236, 236, 236, 1)',
          borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
        }}
      />
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
