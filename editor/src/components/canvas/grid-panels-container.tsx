import React from 'react'
import { arrayAccumulate } from '../../core/shared/array-utils'
import { GridPanel } from './grid-panel'
import { ColumnDragTargets } from './grid-panels-drag-targets'
import type { LayoutUpdate, StoredPanel } from './grid-panels-state'
import {
  GridMenuDefaultPanels,
  GridMenuWidth,
  GridPaneWidth,
  GridPanelHorizontalGapHalf,
  GridPanelVerticalGapHalf,
  storedLayoutToResolvedPanels,
  updateLayout,
  wrapAroundColIndex,
} from './grid-panels-state'
import { CanvasFloatingToolbars } from './camvas-floating-toolbars'

export const GridPanelsContainer = React.memo(() => {
  const [panelState, setPanelState] = React.useState(GridMenuDefaultPanels)

  const orderedPanels = React.useMemo(() => {
    return storedLayoutToResolvedPanels(panelState)
  }, [panelState])

  const nonEmptyColumns = React.useMemo(() => {
    return arrayAccumulate((acc: Array<number>) => {
      panelState.forEach((column, colIndex) => {
        if (column.length > 0) {
          acc.push(wrapAroundColIndex(colIndex))
        }
      })
    })
  }, [panelState])

  const onDrop = React.useCallback(
    (itemToMove: StoredPanel, newPosition: LayoutUpdate) => {
      setPanelState((panels) => updateLayout(panels, itemToMove, newPosition))
    },
    [setPanelState],
  )

  const canDrop = React.useCallback(
    (itemToMove: StoredPanel, newPosition: LayoutUpdate) => {
      const wouldBePanelState = updateLayout(panelState, itemToMove, newPosition)
      const wouldBePanelStateEqualsCurrentPanelState = panelState.every((column, colIndex) =>
        column.every(
          (item, itemIndex) => item.name === wouldBePanelState[colIndex]?.[itemIndex]?.name,
        ),
      )

      if (wouldBePanelStateEqualsCurrentPanelState) {
        // if the drop results in no change, we don't allow it
        return false
      }

      return true
    },
    [panelState],
  )

  const columnWidths = React.useMemo(
    () =>
      panelState.map((column) => {
        if (column.length === 0 || column.some((p) => p.type === 'menu')) {
          return GridMenuWidth
        } else {
          return GridPaneWidth
        }
      }),
    [panelState],
  )

  return (
    <div
      data-testid='floating-panels-container'
      style={{
        position: 'absolute',
        pointerEvents: 'none',
        contain: 'layout',
        display: 'grid',
        width: '100%',
        height: '100%',
        gridTemplateColumns: `[col] ${columnWidths[0]}px [col] ${columnWidths[1]}px [canvas] 1fr [col] ${columnWidths[2]}px [col] ${columnWidths[3]}px [end]`,
        gridTemplateRows: 'repeat(12, 1fr)',
        gridAutoFlow: 'dense',
        columnGap: GridPanelHorizontalGapHalf * 2,
        rowGap: GridPanelVerticalGapHalf * 2,
        paddingTop: GridPanelVerticalGapHalf,
        paddingBottom: GridPanelVerticalGapHalf,
        paddingLeft: GridPanelHorizontalGapHalf + 2,
        paddingRight: GridPanelHorizontalGapHalf + 2,
      }}
    >
      <GridPanel
        key={'code-editor'}
        onDrop={onDrop}
        canDrop={canDrop}
        pane={orderedPanels['code-editor']}
      />
      <GridPanel
        key={'navigator'}
        onDrop={onDrop}
        canDrop={canDrop}
        pane={orderedPanels['navigator']}
      />
      <GridPanel
        key={'inspector'}
        onDrop={onDrop}
        canDrop={canDrop}
        pane={orderedPanels['inspector']}
      />
      <CanvasFloatingToolbars
        style={{ position: 'absolute', gridColumn: 'canvas / span 1', gridRow: '1 / -1' }}
      />
      {/* All future Panels need to be explicitly listed here */}
      {nonEmptyColumns.map((columnIndex) => (
        <ColumnDragTargets
          key={columnIndex}
          columnIndex={columnIndex}
          onDrop={onDrop}
          canDrop={canDrop}
        />
      ))}
    </div>
  )
})
