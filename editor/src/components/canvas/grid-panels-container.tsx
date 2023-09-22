import React from 'react'
import { accumulate } from '../../core/shared/array-utils'
import { GridPanel } from './grid-panel'
import { ColumnDragTargets } from './grid-panels-drag-targets'
import type { LayoutUpdate, StoredPanel } from './grid-panels-state'
import {
  GridHorizontalExtraPadding,
  GridMenuDefaultPanels,
  GridMenuWidth,
  GridPaneWidth,
  GridPanelHorizontalGapHalf,
  GridPanelVerticalGapHalf,
  GridVerticalExtraPadding,
  NumberOfColumns,
  storedLayoutToResolvedPanels,
  updateLayout,
  wrapAroundColIndex,
} from './grid-panels-state'
import { CanvasFloatingToolbars } from './canvas-floating-toolbars'

export const GridPanelsContainer = React.memo(() => {
  const [panelState, setPanelState] = React.useState(GridMenuDefaultPanels)

  const orderedPanels = React.useMemo(() => {
    return storedLayoutToResolvedPanels(panelState)
  }, [panelState])

  const nonEmptyColumns = React.useMemo(() => {
    return Array.from(
      accumulate(new Set<number>(), (acc: Set<number>) => {
        // we always include the first and last columns
        acc.add(0)
        acc.add(NumberOfColumns - 1)

        panelState.forEach((column, colIndex) => {
          if (column.length > 0) {
            acc.add(wrapAroundColIndex(colIndex))
          }
        })
      }),
    )
  }, [panelState])

  const onDrop = React.useCallback(
    (itemToMove: StoredPanel, newPosition: LayoutUpdate) => {
      setPanelState((panels) => updateLayout(panels, itemToMove, newPosition))
    },
    [setPanelState],
  )

  const onHover = React.useCallback(
    (itemToMove: StoredPanel, newPosition: LayoutUpdate) => {
      setPanelState((panels) => {
        if (
          newPosition.columnIndex !==
          panels.findIndex((c) => c.find((p) => p.name === itemToMove.name))
        ) {
          // If not within column reorder, let's bail out here
          return panels
        }
        return updateLayout(panels, itemToMove, newPosition)
      })
    },
    [setPanelState],
  )

  const canDrop = React.useCallback(
    (itemToMove: StoredPanel, newPosition: LayoutUpdate) => {
      return true // for now, just enable all drop areas while we are tweaking the behavior
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

  const columnWidths: Array<string> = React.useMemo(
    () =>
      panelState.map((column) => {
        if (column.length === 0) {
          return `0px`
        } else if (column.some((p) => p.type === 'menu')) {
          return `${GridMenuWidth + GridPanelHorizontalGapHalf * 2}px`
        } else {
          return `${GridPaneWidth + GridPanelHorizontalGapHalf * 2}px`
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
        gridTemplateColumns: `[col] ${columnWidths[0]} [col] ${columnWidths[1]} [canvas] 1fr [col] ${columnWidths[2]} [col] ${columnWidths[3]} [end]`,
        gridTemplateRows: 'repeat(12, 1fr)',
        gridAutoFlow: 'dense',
        paddingTop: GridPanelVerticalGapHalf + GridVerticalExtraPadding,
        paddingBottom: GridPanelVerticalGapHalf + GridVerticalExtraPadding,
        paddingLeft: GridPanelHorizontalGapHalf + GridHorizontalExtraPadding,
        paddingRight: GridPanelHorizontalGapHalf + GridHorizontalExtraPadding,
      }}
    >
      <GridPanel
        key={'code-editor'}
        onDrop={onDrop}
        canDrop={canDrop}
        onHover={onHover}
        pane={orderedPanels['code-editor']}
      />
      <GridPanel
        key={'navigator'}
        onDrop={onDrop}
        canDrop={canDrop}
        onHover={onHover}
        pane={orderedPanels['navigator']}
      />
      <GridPanel
        key={'inspector'}
        onDrop={onDrop}
        canDrop={canDrop}
        onHover={onHover}
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
