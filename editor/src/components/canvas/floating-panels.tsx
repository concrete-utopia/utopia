import findIndex from 'lodash.findindex'
import findLastIndex from 'lodash.findlastindex'
import React from 'react'
import type { MapLike } from 'typescript'
import { v4 as UUID } from 'uuid'
import { accumulate, insert, removeAll, removeIndexFromArray } from '../../core/shared/array-utils'
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

type StoredLayout = Array<Array<StoredPanel>>

const NumberOfPanels = 4
const IndexOfCanvas = 2

const VerticalGapHalf = 6
const HorizontalGapHalf = 6

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
  [
    storedPanel({ name: 'inspector', type: 'menu' }),
    storedPanel({ name: 'navigator', type: 'menu' }),
    storedPanel({ name: 'code-editor', type: 'pane' }),
  ],
  [],
  [],
  [],
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
type ColumnUpdate = BeforeColumn

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
    if (update.type === 'before-column') {
      const atLeastOneEmptyColumn = layout.some((col) => col.length === 0)
      if (!atLeastOneEmptyColumn) {
        // the user wants to create a new column and fill it with the moved Panel.
        // if there's zero empty columns, it means we cannot create a new column, so we must bail out

        return layout // BAIL OUT! TODO we should show a Toast
      }
      const newColumn: Array<StoredPanel> = [panelToInsert]

      if (update.columnIndex < 0) {
        // we are on the right hand side of the editor, so we apply right to left logic
        // we insert the new column at index - 1, shifting neighbors leftwards

        // insert the column at `index - 1`
        const insertionIndex = update.columnIndex - 1
        const withColumnInserted = insert(insertionIndex, newColumn, layout)
        const indexOfFirstEmptyColumnLeftOfInsertion = findLastIndex(
          withColumnInserted,
          (col) => col.length === 0,
          insertionIndex, // start the search from this index, walk backwards
        )
        const withEmptyColumnRemoved = removeIndexFromArray(
          indexOfFirstEmptyColumnLeftOfInsertion,
          withColumnInserted,
        )
        return withEmptyColumnRemoved
      } else {
        // we are on the left hand side, so we apply left to right logic
        // we insert the new column at index, shifting neightbors rightwards

        // insert the column at `index`
        const withColumnInserted = insert(update.columnIndex, newColumn, layout)
        const indexOfFirstEmptyColumnRightOfInsertion = findIndex(
          withColumnInserted,
          (col) => col.length === 0,
          update.columnIndex, // start the search from this index
        )
        const withEmptyColumnRemoved = removeIndexFromArray(
          indexOfFirstEmptyColumnRightOfInsertion,
          withColumnInserted,
        )
        return withEmptyColumnRemoved
      }
    }
    if (update.type === 'before-index') {
      const working = [...layout]

      // insert
      working[update.columnIndex] = insert(
        update.indexInColumn,
        panelToInsert,
        working[update.columnIndex],
      )

      return working
    }
    if (update.type === 'after-index') {
      const working = [...layout]

      // insert
      working[update.columnIndex] = insert(
        update.indexInColumn + 1,
        panelToInsert,
        working[update.columnIndex],
      )

      return working
    }

    assertNever(update)
  }

  function removeOldPanel(layout: StoredLayout) {
    return layout.map((column) => {
      return removeAll(column, [paneToMove], (l, r) => l.uid === r.uid)
    })
  }

  const withPanelInserted = insertPanel(stored)
  const withOldPanelRemoved = removeOldPanel(withPanelInserted)

  // TODO we need to fix the sizes too!
  return withOldPanelRemoved
}

export const FloatingPanelsContainer = React.memo(() => {
  const [panelState, setPanelState] = React.useState(defaultPanels)

  const orderedPanels = React.useMemo(() => {
    return storedLayoutToResolvedPanels(panelState)
  }, [panelState])

  const onDrop = React.useCallback(
    (itemToMove: StoredPanel, newPosition: LayoutUpdate) => {
      setPanelState((panels) => updateLayout(panels, itemToMove, newPosition))
    },
    [setPanelState],
  )

  return (
    <div
      data-testid='floating-panels-container'
      style={{
        position: 'absolute',
        display: 'grid',
        width: '100%',
        height: '100%',
        gridTemplateColumns: '[col] 260px [col] 260px [canvas] 1fr [col] 260px [col] 260px [end]',
        gridTemplateRows: 'repeat(12, 1fr)',
        gridAutoFlow: 'dense',
        columnGap: HorizontalGapHalf * 2,
        rowGap: VerticalGapHalf * 2,
        paddingTop: VerticalGapHalf,
        paddingBottom: VerticalGapHalf,
      }}
    >
      <FloatingPanel key={'code-editor'} onDrop={onDrop} pane={orderedPanels['code-editor']} />
      <FloatingPanel key={'navigator'} onDrop={onDrop} pane={orderedPanels['navigator']} />
      <FloatingPanel key={'inspector'} onDrop={onDrop} pane={orderedPanels['inspector']} />
    </div>
  )
})

interface FloatingPanelProps {
  onDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => void
  pane: PanelData
}

export const FloatingPanel = React.memo<FloatingPanelProps>((props) => {
  const { onDrop } = props
  const { panel, index, span, order } = props.pane

  const { isDragActive, draggedPanelName } = useFloatingPanelDragInfo()
  const showDragCatchArea = isDragActive && draggedPanelName !== panel.name

  const { drop: dropBefore, isOver: isOverBefore } = useFloatingPanelDropArea(
    index,
    order,
    React.useCallback(
      (itemToMove: StoredPanel, newPosition: LayoutUpdate) =>
        onDrop(itemToMove, {
          type: 'before-index',
          indexInColumn: order,
          columnIndex: index,
        }),
      [onDrop, index, order],
    ),
  )
  const { drop: dropAfter, isOver: isOverAfter } = useFloatingPanelDropArea(
    index,
    order,
    React.useCallback(
      (itemToMove: StoredPanel, newPosition: LayoutUpdate) => {
        onDrop(itemToMove, {
          type: 'after-index',
          indexInColumn: order,
          columnIndex: index,
        })
      },
      [onDrop, index, order],
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
    <>
      <div
        style={{
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
            display: showDragCatchArea ? 'block' : 'none',
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
            display: showDragCatchArea ? 'block' : 'none',
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
    </>
  )
})
