import React from 'react'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { mapDropNulls } from '../../../core/shared/array-utils'
import {
  isFiniteRectangle,
  isInfinityRectangle,
  windowPoint,
  zeroRectIfNullOrInfinity,
} from '../../../core/shared/math-utils'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'
import { useDispatch } from '../../editor/store/dispatch-context'
import {
  createInteractionViaMouse,
  gridCellHandle,
  gridResizeHandle,
  gridAxisHandle,
} from '../canvas-strategies/interaction-state'
import CanvasActions from '../canvas-actions'
import { Modifier } from '../../../utils/modifiers'
import { windowToCanvasCoordinates } from '../dom-lookup'
import type {
  ElementInstanceMetadata,
  GridAutoOrTemplateBase,
} from '../../../core/shared/element-template'
import { assertNever } from '../../../core/shared/utils'
import { printGridAutoOrTemplateBase } from '../../../components/inspector/common/css-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { useColorTheme } from '../../../uuiui'

type GridCellCoordinates = { row: number; column: number }

function emptyGridCellCoordinates(): GridCellCoordinates {
  return { row: 0, column: 0 }
}

// TODO please forgive me (hackathon code)
export let TargetGridCell = { current: emptyGridCellCoordinates() }

function getSillyCellsCount(template: GridAutoOrTemplateBase | null): number {
  if (template == null) {
    return 0
  } else {
    switch (template.type) {
      case 'DIMENSIONS':
        return template.dimensions.length
      case 'FALLBACK':
        return 0
      default:
        assertNever(template)
    }
  }
}

function getNullableAutoOrTemplateBaseString(
  template: GridAutoOrTemplateBase | null,
): string | undefined {
  if (template == null) {
    return undefined
  } else {
    return printGridAutoOrTemplateBase(template)
  }
}

export const GridControls = controlForStrategyMemoized(() => {
  const isActivelyDraggingCell = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession != null &&
      (store.editor.canvas.interactionSession.activeControl.type === 'GRID_CELL_HANDLE' ||
        store.editor.canvas.interactionSession.activeControl.type === 'GRID_RESIZE_HANDLE'),
    'GridControls isActivelyDraggingCell',
  )

  const jsxMetadata = useEditorState(
    Substores.fullStore,
    (store) => store.editor.canvas.interactionSession?.latestMetadata ?? store.editor.jsxMetadata,
    'GridControls jsxMetadata',
  )

  const grids = useEditorState(
    Substores.metadataAndPropertyControlsInfo,
    (store) => {
      return mapDropNulls((view) => {
        const element = MetadataUtils.findElementByElementPath(jsxMetadata, view)
        const parent = MetadataUtils.findElementByElementPath(jsxMetadata, EP.parentPath(view))

        const targetGridContainer = MetadataUtils.isGridLayoutedContainer(element)
          ? element
          : MetadataUtils.isGridLayoutedContainer(parent)
          ? parent
          : null

        if (
          targetGridContainer == null ||
          targetGridContainer.globalFrame == null ||
          !isFiniteRectangle(targetGridContainer.globalFrame)
        ) {
          return null
        }

        const gap = targetGridContainer.specialSizeMeasurements.gap
        const padding = targetGridContainer.specialSizeMeasurements.padding
        const gridTemplateColumns =
          targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateColumns
        const gridTemplateRows =
          targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateRows

        const columns = getSillyCellsCount(
          targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateColumns,
        )
        const rows = getSillyCellsCount(
          targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateRows,
        )

        return {
          elementPath: targetGridContainer.elementPath,
          frame: targetGridContainer.globalFrame,
          gridTemplateColumns: gridTemplateColumns,
          gridTemplateRows: gridTemplateRows,
          gap: gap,
          padding: padding,
          rows: rows,
          columns: columns,
          cells: rows * columns,
        }
      }, store.editor.selectedViews)
    },
    'GridControls grids',
  )

  const cells = React.useMemo(() => {
    return grids.flatMap((grid) => {
      const children = MetadataUtils.getChildrenUnordered(jsxMetadata, grid.elementPath)
      return mapDropNulls((cell, index) => {
        if (cell == null || cell.globalFrame == null || !isFiniteRectangle(cell.globalFrame)) {
          return null
        }
        const countedRow = Math.floor(index / grid.columns) + 1
        const countedColumn = Math.floor(index % grid.columns) + 1

        const columnFromProps = cell.specialSizeMeasurements.elementGridProperties.gridColumnStart
        const rowFromProps = cell.specialSizeMeasurements.elementGridProperties.gridRowStart
        return {
          elementPath: cell.elementPath,
          globalFrame: cell.globalFrame,
          column:
            columnFromProps == null
              ? countedColumn
              : columnFromProps === 'auto'
              ? countedColumn
              : columnFromProps.numericalPosition ?? countedColumn,
          row:
            rowFromProps == null
              ? countedRow
              : rowFromProps === 'auto'
              ? countedRow
              : rowFromProps.numericalPosition ?? countedRow,
          index: index,
        }
      }, children)
    })
  }, [grids, jsxMetadata])

  const dispatch = useDispatch()

  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
  const scaleRef = useRefEditorState((store) => store.editor.canvas.scale)

  const startInteractionWithUid = React.useCallback(
    (params: { uid: string; row: number; column: number }) => (event: React.MouseEvent) => {
      TargetGridCell.current = emptyGridCellCoordinates()

      const start = windowToCanvasCoordinates(
        scaleRef.current,
        canvasOffsetRef.current,
        windowPoint({ x: event.nativeEvent.x, y: event.nativeEvent.y }),
      )

      dispatch([
        CanvasActions.createInteractionSession(
          createInteractionViaMouse(
            start.canvasPositionRounded,
            Modifier.modifiersForEvent(event),
            gridCellHandle({ id: params.uid }),
            'zero-drag-not-permitted',
          ),
        ),
      ])
    },
    [canvasOffsetRef, dispatch, scaleRef],
  )

  React.useEffect(() => {
    function h(e: MouseEvent) {
      if (!isActivelyDraggingCell) {
        return
      }
      const cellsUnderMouse = document
        .elementsFromPoint(e.clientX, e.clientY)
        .filter((el) => el.id.startsWith(`gridcell-`))

      // TODO this sucks!
      if (cellsUnderMouse.length > 0) {
        const cellUnderMouse = cellsUnderMouse[0]
        const row = cellUnderMouse.getAttribute('data-grid-row')
        const column = cellUnderMouse.getAttribute('data-grid-column')
        TargetGridCell.current.row = row == null ? 0 : parseInt(row)
        TargetGridCell.current.column = column == null ? 0 : parseInt(column)
      }
    }
    window.addEventListener('mousemove', h)
    return function () {
      window.removeEventListener('mousemove', h)
    }
  }, [isActivelyDraggingCell])

  if (grids.length === 0) {
    return null
  }

  return (
    <CanvasOffsetWrapper>
      {/* grid lines */}
      {grids.map((grid, index) => {
        const placeholders = Array.from(Array(grid.cells).keys())

        return (
          <div
            key={`grid-${index}`}
            style={{
              position: 'absolute',
              top: grid.frame.y,
              left: grid.frame.x,
              width: grid.frame.width,
              height: grid.frame.height,
              //   backgroundColor: '#ff00ff09',
              display: 'grid',
              gridTemplateColumns: getNullableAutoOrTemplateBaseString(grid.gridTemplateColumns),
              gridTemplateRows: getNullableAutoOrTemplateBaseString(grid.gridTemplateRows),
              gap: grid.gap ?? 0,
              padding:
                grid.padding == null
                  ? 0
                  : `${grid.padding.top}px ${grid.padding.right}px ${grid.padding.bottom}px ${grid.padding.left}px`,
            }}
          >
            {placeholders.map((cell, cellIndex) => {
              const countedRow = Math.floor(cellIndex / grid.columns) + 1
              const countedColumn = Math.floor(cellIndex % grid.columns) + 1
              const id = `gridcell-${index}-${cell}`
              const edgeColor = isActivelyDraggingCell ? '#00000033' : 'transparent'
              const borderColor = isActivelyDraggingCell ? '#00000022' : '#0000000a'
              return (
                <div
                  key={id}
                  id={id}
                  style={{
                    border: `1px solid ${borderColor}`,
                    position: 'relative',
                  }}
                  data-grid-row={countedRow}
                  data-grid-column={countedColumn}
                >
                  <div
                    style={{
                      position: 'absolute',
                      top: -1,
                      bottom: -1,
                      left: -1,
                      right: -1,
                      display: 'flex',
                      alignItems: 'center',
                      justifyContent: 'center',
                    }}
                  >
                    <div style={{ width: 2, height: 2, backgroundColor: edgeColor }} />
                  </div>
                  <div
                    style={{
                      width: 2,
                      height: 2,
                      backgroundColor: edgeColor,
                      position: 'absolute',
                      top: -1,
                      left: -1,
                    }}
                  />
                  <div
                    style={{
                      width: 2,
                      height: 2,
                      backgroundColor: edgeColor,
                      position: 'absolute',
                      bottom: -1,
                      left: -1,
                    }}
                  />
                  <div
                    style={{
                      width: 2,
                      height: 2,
                      backgroundColor: edgeColor,
                      position: 'absolute',
                      top: -1,
                      right: -1,
                    }}
                  />
                  <div
                    style={{
                      width: 2,
                      height: 2,
                      backgroundColor: edgeColor,
                      position: 'absolute',
                      bottom: -1,
                      right: -1,
                    }}
                  />
                </div>
              )
            })}
          </div>
        )
      })}
      {/* cell targets */}
      {cells.map((cell) => {
        return (
          <div
            onMouseDown={startInteractionWithUid({
              uid: EP.toUid(cell.elementPath),
              row: cell.row,
              column: cell.column,
            })}
            key={`grid-cell-${EP.toString(cell.elementPath)}`}
            style={{
              position: 'absolute',
              top: cell.globalFrame.y,
              left: cell.globalFrame.x,
              width: cell.globalFrame.width,
              height: cell.globalFrame.height,
              display: 'flex',
              justifyContent: 'flex-end',
              alignItems: 'flex-end',
            }}
          />
        )
      })}
      {grids.flatMap((grid) => {
        if (grid.gridTemplateColumns == null) {
          return []
        } else {
          switch (grid.gridTemplateColumns.type) {
            case 'DIMENSIONS':
              let workingPrefix: number = grid.frame.x
              return grid.gridTemplateColumns.dimensions.flatMap((dimension, dimensionIndex) => {
                // Assumes pixels currently.
                workingPrefix += dimension.value
                if (dimensionIndex !== 0) {
                  workingPrefix += grid.gap ?? 0
                }
                function mouseDownHandler(event: React.MouseEvent): void {
                  const start = windowToCanvasCoordinates(
                    scaleRef.current,
                    canvasOffsetRef.current,
                    windowPoint({ x: event.nativeEvent.x, y: event.nativeEvent.y }),
                  )

                  dispatch([
                    CanvasActions.createInteractionSession(
                      createInteractionViaMouse(
                        start.canvasPositionRounded,
                        Modifier.modifiersForEvent(event),
                        gridAxisHandle('column', dimensionIndex),
                        'zero-drag-not-permitted',
                      ),
                    ),
                  ])
                  event.stopPropagation()
                  event.preventDefault()
                }
                return (
                  <div
                    data-testid={`grid-column-handle-${dimensionIndex}`}
                    style={{
                      position: 'absolute',
                      left: workingPrefix - 15,
                      top: grid.frame.y - 30,
                      width: 40,
                      height: '20px',
                      borderRadius: 5,
                      backgroundColor: '#f0f',
                      display: 'flex',
                      justifyContent: 'center',
                      alignItems: 'center',
                    }}
                    onMouseDown={mouseDownHandler}
                  >
                    {`${dimension.value}${dimension.unit ?? ''}`}
                  </div>
                )
              })
            case 'FALLBACK':
              return []
            default:
              assertNever(grid.gridTemplateColumns)
              return []
          }
        }
      })}
      {grids.flatMap((grid) => {
        if (grid.gridTemplateRows == null) {
          return []
        } else {
          switch (grid.gridTemplateRows.type) {
            case 'DIMENSIONS':
              let workingPrefix: number = grid.frame.y
              return grid.gridTemplateRows.dimensions.flatMap((dimension, dimensionIndex) => {
                // Assumes pixels currently.
                workingPrefix += dimension.value
                if (dimensionIndex !== 0) {
                  workingPrefix += grid.gap ?? 0
                }
                function mouseDownHandler(event: React.MouseEvent): void {
                  const start = windowToCanvasCoordinates(
                    scaleRef.current,
                    canvasOffsetRef.current,
                    windowPoint({ x: event.nativeEvent.x, y: event.nativeEvent.y }),
                  )

                  dispatch([
                    CanvasActions.createInteractionSession(
                      createInteractionViaMouse(
                        start.canvasPositionRounded,
                        Modifier.modifiersForEvent(event),
                        gridAxisHandle('row', dimensionIndex),
                        'zero-drag-not-permitted',
                      ),
                    ),
                  ])
                  event.stopPropagation()
                  event.preventDefault()
                }
                return (
                  <div
                    data-testid={`grid-row-handle-${dimensionIndex}`}
                    style={{
                      position: 'absolute',
                      left: grid.frame.x - 50,
                      top: workingPrefix - 5,
                      width: 40,
                      height: '20px',
                      borderRadius: 5,
                      backgroundColor: '#f0f',
                      display: 'flex',
                      justifyContent: 'center',
                      alignItems: 'center',
                    }}
                    onMouseDown={mouseDownHandler}
                  >
                    {`${dimension.value}${dimension.unit ?? ''}`}
                  </div>
                )
              })
            case 'FALLBACK':
              return []
            default:
              assertNever(grid.gridTemplateRows)
              return []
          }
        }
      })}
    </CanvasOffsetWrapper>
  )
})

export const GridResizeShadow = controlForStrategyMemoized(
  ({ elementPath }: { elementPath: ElementPath }) => {
    const element = useEditorState(
      Substores.metadata,
      (store) => MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, elementPath),
      'GridResizeShadow element',
    )

    const dispatch = useDispatch()
    const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
    const scaleRef = useRefEditorState((store) => store.editor.canvas.scale)

    const dragging = useEditorState(
      Substores.canvas,
      (store) =>
        store.editor.canvas.interactionSession != null &&
        store.editor.canvas.interactionSession.activeControl.type === 'GRID_RESIZE_HANDLE',
      '',
    )
    const [offset, setOffset] = React.useState<{ width: number; height: number } | null>(null)
    const onMouseMove = React.useCallback(
      (e: MouseEvent) => {
        if (!dragging) {
          return
        }

        setOffset((o) =>
          o == null ? null : { width: o.width + e.movementX, height: o.height + e.movementY },
        )
      },
      [dragging],
    )

    const onMouseUp = React.useCallback(() => setOffset(null), [])

    React.useEffect(() => {
      window.addEventListener('mousemove', onMouseMove)
      window.addEventListener('mouseup', onMouseUp)
      return () => {
        window.removeEventListener('mousemove', onMouseMove)
        window.removeEventListener('mouseup', onMouseUp)
      }
    }, [onMouseMove, onMouseUp])

    const startResizeInteractionWithUid = React.useCallback(
      (uid: string) => (event: React.MouseEvent) => {
        event.stopPropagation()
        const frame = zeroRectIfNullOrInfinity(element?.globalFrame ?? null)
        setOffset({ width: frame.width, height: frame.height })
        const start = windowToCanvasCoordinates(
          scaleRef.current,
          canvasOffsetRef.current,
          windowPoint({ x: event.nativeEvent.x, y: event.nativeEvent.y }),
        )
        dispatch([
          CanvasActions.createInteractionSession(
            createInteractionViaMouse(
              start.canvasPositionRounded,
              Modifier.modifiersForEvent(event),
              gridResizeHandle(uid),
              'zero-drag-not-permitted',
            ),
          ),
        ])
      },
      [canvasOffsetRef, dispatch, element?.globalFrame, scaleRef],
    )

    const colorTheme = useColorTheme()

    if (
      element == null ||
      element.globalFrame == null ||
      isInfinityRectangle(element.globalFrame)
    ) {
      return null
    }

    return (
      <CanvasOffsetWrapper>
        <div
          key={`grid-resize-container-${EP.toString(element.elementPath)}`}
          style={{
            pointerEvents: 'none',
            position: 'absolute',
            top: element.globalFrame.y,
            left: element.globalFrame.x,
            width: offset?.width ?? element.globalFrame.width,
            height: offset?.height ?? element.globalFrame.height,
            display: 'flex',
            justifyContent: 'flex-end',
            alignItems: 'flex-end',
            opacity: 0.3,
            background: colorTheme.denimBlue.value,
          }}
        >
          <div
            onMouseDown={startResizeInteractionWithUid(EP.toUid(element.elementPath))}
            style={{
              pointerEvents: 'initial',
              border: '1px solid white',
              width: 12,
              height: 12,
              backgroundColor: 'black',
              margin: 4,
              borderRadius: '50%',
            }}
          />
        </div>
      </CanvasOffsetWrapper>
    )
  },
)
