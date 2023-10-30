import React from 'react'
import { accumulate } from '../../core/shared/array-utils'
import { GridPanel } from './grid-panel'
import {
  CanvasPaneDragTargets,
  ColumnDragTargets,
  GridColumnResizeHandle,
} from './grid-panels-drag-targets'
import { CanvasFloatingToolbars } from './canvas-floating-toolbars'
import {
  normalizeColIndex,
  updateLayout,
  useColumnWidths,
  useGridPanelState,
  useResolvedGridPanels,
  wrapAroundColIndex,
} from './grid-panels-state'
import type { StoredPanel, LayoutUpdate } from './stored-layout'
import {
  NumberOfColumns,
  GridPanelVerticalGapHalf,
  GridVerticalExtraPadding,
  GridPanelHorizontalGapHalf,
  GridHorizontalExtraPadding,
} from './stored-layout'
import {
  getProjectStoredLayoutOrDefault,
  loadUserPreferences,
  saveUserPreferencesProjectLayout,
} from '../common/user-preferences'
import { Substores, useEditorState } from '../editor/store/store-hook'

export const GridPanelsContainer = React.memo(() => {
  const [panelStateLoaded, setPanelStateLoaded] = React.useState(false)
  const [panelState, setPanelState] = useGridPanelState()
  const projectId = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.id,
    'GridPanelsContainer projectId',
  )

  React.useEffect(() => {
    if (projectId == null || panelStateLoaded) {
      return
    }
    setPanelStateLoaded(true)
    async function loadPrefs(id: string) {
      const prefs = await loadUserPreferences()
      setPanelState(getProjectStoredLayoutOrDefault(prefs.panelsLayout, id))
    }
    void loadPrefs(projectId)
  }, [panelStateLoaded, setPanelState, projectId])

  React.useEffect(() => {
    if (projectId == null || !panelStateLoaded) {
      return
    }
    void saveUserPreferencesProjectLayout(projectId, panelState)
  }, [panelStateLoaded, panelState, projectId])

  const orderedPanels = useResolvedGridPanels()

  const nonEmptyColumns = React.useMemo(() => {
    return Array.from(
      accumulate(new Set<number>(), (acc: Set<number>) => {
        // we always include the first and last columns
        acc.add(wrapAroundColIndex(0))
        acc.add(wrapAroundColIndex(NumberOfColumns - 1))

        panelState.forEach((column, colIndex) => {
          if (column.panels.length > 0) {
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

  const canDrop = React.useCallback(
    (itemToMove: StoredPanel, newPosition: LayoutUpdate) => {
      const wouldBePanelState = updateLayout(panelState, itemToMove, newPosition)
      const wouldBePanelStateEqualsCurrentPanelState = panelState.every((column, colIndex) =>
        column.panels.every(
          (item, itemIndex) => item.name === wouldBePanelState[colIndex]?.panels[itemIndex]?.name,
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

  const [columnWidths, setColumnWidth] = useColumnWidths()

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
      <CanvasPaneDragTargets onDrop={onDrop} canDrop={canDrop} />
      {nonEmptyColumns.map((columnIndex) => (
        <React.Fragment key={columnIndex}>
          <GridColumnResizeHandle
            key={`resize-${columnIndex}`}
            columnIndex={columnIndex}
            columnWidth={columnWidths[normalizeColIndex(columnIndex)]}
            setColumnWidth={setColumnWidth}
          />
          <ColumnDragTargets
            key={`droptarget-${columnIndex}`}
            columnIndex={columnIndex}
            onDrop={onDrop}
            canDrop={canDrop}
          />
        </React.Fragment>
      ))}
    </div>
  )
})
