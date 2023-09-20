import findLastIndex from 'lodash.findlastindex'
import React from 'react'
import type { MapLike } from 'typescript'
import { v4 as UUID } from 'uuid'
import {
  accumulate,
  arrayAccumulate,
  insert,
  removeAll,
  removeIndexFromArray,
} from '../../core/shared/array-utils'
import { mod } from '../../core/shared/math-utils'
import { NO_OP, assertNever } from '../../core/shared/utils'
import { UtopiaTheme, colorTheme, width } from '../../uuiui'
import { LeftPanelMinWidth } from '../editor/store/editor-state'
import { LeftPaneComponent } from '../navigator/left-pane'
import { CodeEditorPane, ResizableRightPane } from './design-panel-root'
import { useFloatingPanelDragInfo, useFloatingPanelDropArea } from './floating-panels-dnd'
import type { Menu, Pane, PanelData } from './floating-panels-state-2'

export type PanelName = Menu | Pane

export interface StoredPanel {
  name: PanelName
  type: 'menu' | 'pane'
  uid: string
}

function storedPanel({ name, type }: { name: PanelName; type: 'menu' | 'pane' }): StoredPanel {
  return {
    name: name,
    type: type,
    uid: UUID(),
  }
}

const MenuWidth = 260
const PaneWidth = 500

type StoredLayout = Array<Array<StoredPanel>>

const NumberOfPanels = 4
const IndexOfCanvas = 2

const VerticalGapHalf = 6
const HorizontalGapHalf = 6

const ExtraHorizontalDropTargetPadding = 15

const NumberOfRows = 12

/**
 * Returns the index in the wraparound annotation, currently the values are -2, -1, 0, 1
 */
function wrapAroundColIndex(index: number): number {
  const normalized = normalizeColIndex(index)
  if (normalized >= IndexOfCanvas) {
    return normalized - NumberOfPanels
  } else {
    return normalized
  }
}

/**
 * Normalizes the index to 0,1,2,3
 */
function normalizeColIndex(index: number): number {
  return mod(index, NumberOfPanels)
}

const defaultPanels: StoredLayout = [
  [storedPanel({ name: 'code-editor', type: 'pane' })],
  [storedPanel({ name: 'navigator', type: 'menu' })],
  [],
  [storedPanel({ name: 'inspector', type: 'menu' })],
]

function storedLayoutToResolvedPanels(stored: StoredLayout): { [index in PanelName]: PanelData } {
  const panels = accumulate<{ [index in PanelName]: PanelData }>({} as any, (acc) => {
    stored.forEach((column, colIndex) => {
      const panelsForColumn = column.length
      column.forEach((panel, panelIndex) => {
        acc[panel.name] = {
          panel: panel,
          span: NumberOfRows / panelsForColumn, // TODO introduce resize function
          index: colIndex,
          order: panelIndex,
        }
      })
    })
  })

  return panels
}

type BeforeColumn = {
  type: 'before-column'
  columnIndex: number
}
type AfterColumn = {
  type: 'after-column'
  columnIndex: number
}
type ColumnUpdate = BeforeColumn | AfterColumn

type BeforeIndex = {
  type: 'before-index'
  columnIndex: number
  indexInColumn: number
}
type AfterIndex = {
  type: 'after-index'
  columnIndex: number
  indexInColumn: number
}
type RowUpdate = BeforeIndex | AfterIndex
export type LayoutUpdate = ColumnUpdate | RowUpdate

function updateLayout(
  stored: StoredLayout,
  paneToMove: StoredPanel, // must be referentially equal to the stored panel!
  update: LayoutUpdate,
): StoredLayout {
  const panelToInsert = storedPanel(paneToMove)

  function insertPanel(layout: StoredLayout) {
    if (update.type === 'before-column' || update.type === 'after-column') {
      const atLeastOneEmptyColumn = layout.some((col) => col.length === 0)
      if (!atLeastOneEmptyColumn) {
        // the user wants to create a new column and fill it with the moved Panel.
        // if there's zero empty columns, it means we cannot create a new column, so we must bail out

        return layout // BAIL OUT! TODO we should show a Toast
      }
      const newColumn: Array<StoredPanel> = [panelToInsert]

      const normalizedIndex = normalizeColIndex(update.columnIndex)

      const indexInArray = update.type === 'before-column' ? normalizedIndex : normalizedIndex + 1

      const rightHandSide = normalizedIndex >= IndexOfCanvas

      const withElementInserted = insert(indexInArray, newColumn, layout)
      const withOldPanelRemoved = removeOldPanel(withElementInserted)

      const indexOfFirstEmptyColumn = rightHandSide
        ? findLastIndex(withOldPanelRemoved, (col) => col.length === 0)
        : withOldPanelRemoved.findIndex((col) => col.length === 0)

      return removeIndexFromArray(indexOfFirstEmptyColumn, withOldPanelRemoved)
    }
    if (update.type === 'before-index') {
      const working = [...layout]

      // insert
      working[update.columnIndex] = insert(
        update.indexInColumn,
        panelToInsert,
        working[update.columnIndex],
      )

      return removeOldPanel(working)
    }
    if (update.type === 'after-index') {
      const working = [...layout]

      // insert
      working[update.columnIndex] = insert(
        update.indexInColumn + 1,
        panelToInsert,
        working[update.columnIndex],
      )

      return removeOldPanel(working)
    }

    assertNever(update)
  }

  function removeOldPanel(layout: StoredLayout) {
    return layout.map((column) => {
      return removeAll(column, [paneToMove], (l, r) => l.uid === r.uid)
    })
  }

  function floatColumnsTowardsEdges(layout: StoredLayout) {
    const leftSide = layout.slice(0, IndexOfCanvas)
    const rightSideReversed = layout.slice(IndexOfCanvas).reverse()
    const leftSideFixed = accumulate(new Array(leftSide.length).fill([]), (acc) => {
      let iterator = 0
      leftSide.forEach((column) => {
        if (column.length > 0) {
          acc[iterator] = column
          iterator++
        }
      })
    })
    const rightSideFixed = accumulate(new Array(rightSideReversed.length).fill([]), (acc) => {
      let iterator = rightSideReversed.length - 1
      rightSideReversed.forEach((column) => {
        if (column.length > 0) {
          acc[iterator] = column
          iterator--
        }
      })
    })
    return [...leftSideFixed, ...rightSideFixed]
  }

  const withPanelInserted = insertPanel(stored)
  const withEmptyColumnsInMiddle = floatColumnsTowardsEdges(withPanelInserted)

  // TODO we need to fix the sizes too!
  return withEmptyColumnsInMiddle
}

export const FloatingPanelsContainer = React.memo(() => {
  const [panelState, setPanelState] = React.useState(defaultPanels)

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
          return MenuWidth
        } else {
          return PaneWidth
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
        columnGap: HorizontalGapHalf * 2,
        rowGap: VerticalGapHalf * 2,
        paddingTop: VerticalGapHalf,
        paddingBottom: VerticalGapHalf,
        paddingLeft: HorizontalGapHalf + 2,
        paddingRight: HorizontalGapHalf + 2,
      }}
    >
      <FloatingPanel
        key={'code-editor'}
        onDrop={onDrop}
        canDrop={canDrop}
        pane={orderedPanels['code-editor']}
      />
      <FloatingPanel
        key={'navigator'}
        onDrop={onDrop}
        canDrop={canDrop}
        pane={orderedPanels['navigator']}
      />
      <FloatingPanel
        key={'inspector'}
        onDrop={onDrop}
        canDrop={canDrop}
        pane={orderedPanels['inspector']}
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

const ColumnDragTargets = React.memo(
  (props: {
    columnIndex: number
    canDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => void
    onDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => void
  }) => {
    const { columnIndex, onDrop, canDrop } = props
    const { isDragActive, draggedPanel } = useFloatingPanelDragInfo()

    const dropBeforeColumn: LayoutUpdate = React.useMemo(
      () => ({
        type: 'before-column',
        columnIndex: columnIndex,
      }),
      [columnIndex],
    )

    const dropAfterColumn: LayoutUpdate = React.useMemo(
      () => ({
        type: 'after-column',
        columnIndex: columnIndex,
      }),
      [columnIndex],
    )

    const canDropBefore = draggedPanel != null && canDrop(draggedPanel, dropBeforeColumn)
    const canDropAfter = draggedPanel != null && canDrop(draggedPanel, dropAfterColumn)

    const { drop: dropBefore, isOver: isOverBefore } = useFloatingPanelDropArea(
      columnIndex,
      9,
      React.useCallback(
        (itemToMove: StoredPanel) => onDrop(itemToMove, dropBeforeColumn),
        [onDrop, dropBeforeColumn],
      ),
    )

    const { drop: dropAfter, isOver: isOverAfter } = useFloatingPanelDropArea(
      columnIndex,
      0,
      React.useCallback(
        (itemToMove: StoredPanel) => {
          onDrop(itemToMove, dropAfterColumn)
        },
        [onDrop, dropAfterColumn],
      ),
    )

    return (
      <>
        <div
          ref={dropBefore}
          style={{
            position: 'absolute',
            pointerEvents: 'initial',
            gridRowStart: 1,
            gridRowEnd: -1,
            gridColumn: `col ${columnIndex > -1 ? columnIndex + 1 : columnIndex} / span 1`,
            display: isDragActive && canDropBefore ? 'block' : 'none',
            width: 2 * ExtraHorizontalDropTargetPadding + 2 * HorizontalGapHalf,
            height: '100%',
            left: -(ExtraHorizontalDropTargetPadding + 2 * HorizontalGapHalf),
          }}
        >
          <div
            style={{
              display: isOverBefore ? 'block' : 'none',
              position: 'absolute',
              left: `calc(50% - 1px)`,
              width: 2,
              height: '100%',
              backgroundColor: colorTheme.primary.value,
            }}
          />
        </div>
        <div
          ref={dropAfter}
          style={{
            position: 'absolute',
            pointerEvents: 'initial',
            gridRowStart: 1,
            gridRowEnd: -1,
            gridColumn: `col ${columnIndex > -1 ? columnIndex + 1 : columnIndex} / span 1`,
            display: isDragActive && canDropAfter ? 'block' : 'none',
            width: 2 * ExtraHorizontalDropTargetPadding + 2 * HorizontalGapHalf,
            height: '100%',
            right: -(ExtraHorizontalDropTargetPadding + 2 * HorizontalGapHalf),
          }}
        >
          <div
            style={{
              display: isOverAfter ? 'block' : 'none',
              position: 'absolute',
              left: `calc(50% - 1px)`,
              width: 2,
              height: '100%',
              backgroundColor: colorTheme.primary.value,
            }}
          />
        </div>
      </>
    )
  },
)

interface FloatingPanelProps {
  onDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => void
  canDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => boolean
  pane: PanelData
}

export const FloatingPanel = React.memo<FloatingPanelProps>((props) => {
  const { onDrop, canDrop } = props
  const { panel, index, span, order } = props.pane

  const { isDragActive, draggedPanel } = useFloatingPanelDragInfo()

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

  const { drop: dropBefore, isOver: isOverBefore } = useFloatingPanelDropArea(
    index,
    order,
    React.useCallback(
      (itemToMove: StoredPanel) => onDrop(itemToMove, dropAboveElement),
      [onDrop, dropAboveElement],
    ),
  )
  const { drop: dropAfter, isOver: isOverAfter } = useFloatingPanelDropArea(
    index,
    order,
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
            small={false}
            width={0}
            height={0}
            onResize={NO_OP}
            setIsResizing={NO_OP}
            resizableConfig={{
              enable: {
                right: true,
              },
            }}
          />
        )
      case 'inspector':
        return (
          <ResizableRightPane
            panelData={props.pane.panel}
            width={0}
            height={0}
            onResize={NO_OP}
            setIsResizing={NO_OP}
            resizableConfig={{
              snap: {
                x: [UtopiaTheme.layout.inspectorSmallWidth, UtopiaTheme.layout.inspectorLargeWidth],
              },
              enable: {
                left: true,
              },
            }}
          />
        )
      case 'navigator':
        return (
          <LeftPaneComponent
            panelData={props.pane.panel}
            width={0}
            height={0}
            onResize={NO_OP}
            setIsResizing={NO_OP}
            resizableConfig={{
              minWidth: LeftPanelMinWidth,
              enable: {
                right: true,
              },
            }}
          />
        )
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
          height: `calc(50% + ${VerticalGapHalf}px)`,
          top: -VerticalGapHalf,
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
          height: `calc(50% + ${VerticalGapHalf}px)`,
          bottom: -VerticalGapHalf,
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
