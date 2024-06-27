import React from 'react'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { mapDropNulls } from '../../../core/shared/array-utils'
import type { CanvasPoint, CanvasRectangle } from '../../../core/shared/math-utils'
import {
  canvasPoint,
  distance,
  getRectCenter,
  isFiniteRectangle,
  isInfinityRectangle,
  offsetPoint,
  pointDifference,
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
import { when } from '../../../utils/react-conditionals'
import { CanvasMousePositionRaw } from '../../../utils/global-positions'
import { motion, useAnimationControls } from 'framer-motion'
import { atom, useAtom } from 'jotai'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { useColorTheme } from '../../../uuiui'
import type { ElementPath } from 'utopia-shared/src/types'

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

export const defaultExperimentalGridFeatures = {
  dragLockedToCenter: false,
  dragVerbatim: false,
  dragMagnetic: false,
  dragRatio: true,
  animateSnap: true,
  dotgrid: true,
  shadow: true,
  adaptiveOpacity: true,
  activeGridColor: '#0099ff77',
  dotgridColor: '#0099ffaa',
  inactiveGridColor: '#0000000a',
  opacityBaseline: 0.25,
}

export const gridFeaturesExplained: Record<string, string> = {
  adaptiveOpacity: 'shadow opacity is proportional to the drag distance',
  dragLockedToCenter: 'drag will keep the shadow centered',
  dragVerbatim: 'drag will be verbatim',
  dragMagnetic: 'drag will magnetize to the snap regions',
  dragRatio: 'drag will keep the shadow positioned based on the drag start',
  animateSnap: 'the shadow goes *boop* when snapping',
  dotgrid: 'show dotgrid',
  shadow: 'show the shadow during drag',
  activeGridColor: 'grid lines color during drag',
  dotgridColor: 'dotgrid items color',
  inactiveGridColor: 'grid lines color when not dragging',
  opacityBaseline: 'maximum shadow opacity',
}

export const experimentalGridFeatures = atom(defaultExperimentalGridFeatures)

export const GridControls = controlForStrategyMemoized(() => {
  const [features, setFeatures] = useAtom(experimentalGridFeatures)

  React.useEffect(() => {
    setFeatures((old) => ({
      ...old,
      adaptiveOpacity: isFeatureEnabled('Grid move - adaptiveOpacity'),
      dragLockedToCenter: isFeatureEnabled('Grid move - dragLockedToCenter'),
      dragVerbatim: isFeatureEnabled('Grid move - dragVerbatim'),
      dragMagnetic: isFeatureEnabled('Grid move - dragMagnetic'),
      dragRatio: isFeatureEnabled('Grid move - dragRatio'),
      animateSnap: isFeatureEnabled('Grid move - animateSnap'),
      dotgrid: isFeatureEnabled('Grid move - dotgrid'),
      shadow: isFeatureEnabled('Grid move - shadow'),
    }))
  }, [setFeatures])

  const activelyDraggingOrResizingCell = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession != null &&
      (store.editor.canvas.interactionSession.activeControl.type === 'GRID_CELL_HANDLE' ||
        store.editor.canvas.interactionSession.activeControl.type === 'GRID_RESIZE_HANDLE')
        ? store.editor.canvas.interactionSession.activeControl.id
        : null,
    '',
  )

  const dragging = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession != null &&
      store.editor.canvas.interactionSession.activeControl.type === 'GRID_CELL_HANDLE'
        ? store.editor.canvas.interactionSession.activeControl.id
        : null,
    '',
  )

  const interactionSession = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession?.interactionData.type === 'DRAG'
        ? store.editor.canvas.interactionSession.interactionData
        : null,
    '',
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
          borderRadius: cell.specialSizeMeasurements.borderRadius,
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

  const shadow = React.useMemo(() => {
    return cells.find((cell) => EP.toUid(cell.elementPath) === dragging)
  }, [cells, dragging])

  const [shadowFrame, setShadowFrame] = React.useState<CanvasRectangle | null>(
    shadow?.globalFrame ?? null,
  )

  const startInteractionWithUid = React.useCallback(
    (params: { uid: string; row: number; column: number; frame: CanvasRectangle }) =>
      (event: React.MouseEvent) => {
        TargetGridCell.current = emptyGridCellCoordinates()

        setShadowFrame(params.frame)

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

  const [hoveringCell, setHoveringCell] = React.useState<string | null>(null)
  const [hoveringStart, setHoveringStart] = React.useState<{
    id: string
    point: CanvasPoint
  } | null>(null)
  const controls = useAnimationControls()

  React.useEffect(() => {
    if (hoveringCell == null) {
      return
    }
    if (!features.animateSnap) {
      return
    }
    void controls.start('boop')
  }, [hoveringCell, controls, features])

  React.useEffect(() => {
    function h(e: MouseEvent) {
      if (activelyDraggingOrResizingCell == null) {
        setHoveringStart(null)
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
        setHoveringCell(cellUnderMouse.id)

        setHoveringStart((start) => {
          if (start == null || start.id !== cellUnderMouse.id) {
            return { id: cellUnderMouse.id, point: canvasPoint(CanvasMousePositionRaw!) }
          }
          return start
        })
      }
    }
    window.addEventListener('mousemove', h)
    return function () {
      window.removeEventListener('mousemove', h)
    }
  }, [activelyDraggingOrResizingCell])

  if (grids.length === 0) {
    return null
  }

  return (
    <React.Fragment>
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
                display: 'grid',
                gridTemplateColumns: getNullableAutoOrTemplateBaseString(grid.gridTemplateColumns),
                gridTemplateRows: getNullableAutoOrTemplateBaseString(grid.gridTemplateRows),
                backgroundColor:
                  activelyDraggingOrResizingCell != null ? '#0099ff11' : 'transparent',
                border: `1px solid ${
                  activelyDraggingOrResizingCell != null ? '#09f' : 'transparent'
                }`,
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
                const dotgridColor =
                  activelyDraggingOrResizingCell != null ? features.dotgridColor : 'transparent'
                const borderColor =
                  activelyDraggingOrResizingCell != null
                    ? features.activeGridColor
                    : features.inactiveGridColor
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
                    {when(
                      features.dotgrid,
                      <>
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
                          <div style={{ width: 2, height: 2, backgroundColor: dotgridColor }} />
                        </div>
                        <div
                          style={{
                            width: 2,
                            height: 2,
                            backgroundColor: dotgridColor,
                            position: 'absolute',
                            top: -1,
                            left: -1,
                          }}
                        />
                        <div
                          style={{
                            width: 2,
                            height: 2,
                            backgroundColor: dotgridColor,
                            position: 'absolute',
                            bottom: -1,
                            left: -1,
                          }}
                        />
                        <div
                          style={{
                            width: 2,
                            height: 2,
                            backgroundColor: dotgridColor,
                            position: 'absolute',
                            top: -1,
                            right: -1,
                          }}
                        />
                        <div
                          style={{
                            width: 2,
                            height: 2,
                            backgroundColor: dotgridColor,
                            position: 'absolute',
                            bottom: -1,
                            right: -1,
                          }}
                        />
                      </>,
                    )}
                  </div>
                )
              })}
            </div>
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
        {/* cell targets */}
        {cells.map((cell) => {
          return (
            <div
              onMouseDown={startInteractionWithUid({
                uid: EP.toUid(cell.elementPath),
                frame: cell.globalFrame,
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
                backgroundColor:
                  activelyDraggingOrResizingCell != null &&
                  EP.toUid(cell.elementPath) !== activelyDraggingOrResizingCell
                    ? '#ffffff66'
                    : 'transparent',
                borderRadius:
                  cell.borderRadius != null
                    ? `${cell.borderRadius.top}px ${cell.borderRadius.right}px ${cell.borderRadius.bottom}px ${cell.borderRadius.left}px`
                    : 0,
              }}
            />
          )
        })}
        {/* shadow */}
        {features.shadow &&
          shadow != null &&
          shadowFrame != null &&
          interactionSession?.dragStart != null &&
          interactionSession?.drag != null &&
          hoveringStart != null && (
            <motion.div
              // key={`shadow-for-${activelyDraggingCell}`}
              initial={'normal'}
              variants={{
                normal: { scale: 1 },
                boop: {
                  scale: [1, 1.4, 1],
                  transition: { duration: 0.15, type: 'tween' },
                },
              }}
              animate={controls}
              style={{
                pointerEvents: 'none',
                position: 'absolute',
                width: shadow.globalFrame.width,
                height: shadow.globalFrame.height,
                borderRadius:
                  shadow.borderRadius != null
                    ? `${shadow.borderRadius.top}px ${shadow.borderRadius.right}px ${shadow.borderRadius.bottom}px ${shadow.borderRadius.left}px`
                    : 0,
                backgroundColor: 'black',
                opacity: features.adaptiveOpacity
                  ? features.dragLockedToCenter
                    ? Math.min(
                        (0.2 *
                          distance(getRectCenter(shadow.globalFrame), CanvasMousePositionRaw!)) /
                          Math.min(shadow.globalFrame.height, shadow.globalFrame.width) +
                          0.05,
                        features.opacityBaseline,
                      )
                    : Math.min(
                        (0.2 *
                          distance(
                            offsetPoint(
                              interactionSession.dragStart,
                              pointDifference(shadowFrame, shadow.globalFrame),
                            ),
                            CanvasMousePositionRaw!,
                          )) /
                          Math.min(shadow.globalFrame.height, shadow.globalFrame.width) +
                          0.05,
                        features.opacityBaseline,
                      )
                  : features.opacityBaseline,
                border: '1px solid white',
                top: features.dragVerbatim
                  ? shadowFrame.y + interactionSession.drag.y
                  : features.dragLockedToCenter
                  ? shadow.globalFrame.y +
                    interactionSession.drag.y -
                    (shadow.globalFrame.y - interactionSession.dragStart.y) -
                    shadow.globalFrame.height / 2
                  : features.dragMagnetic
                  ? shadow.globalFrame.y + (CanvasMousePositionRaw!.y - hoveringStart.point.y)
                  : features.dragRatio
                  ? shadow.globalFrame.y +
                    interactionSession.drag.y -
                    (shadow.globalFrame.y - interactionSession.dragStart.y) -
                    shadow.globalFrame.height *
                      ((interactionSession.dragStart.y - shadowFrame.y) / shadowFrame.height)
                  : undefined,
                left: features.dragVerbatim
                  ? shadowFrame.x + interactionSession.drag.x
                  : features.dragLockedToCenter
                  ? shadow.globalFrame.x +
                    interactionSession.drag.x -
                    (shadow.globalFrame.x - interactionSession.dragStart.x) -
                    shadow.globalFrame.width / 2
                  : features.dragMagnetic
                  ? shadow.globalFrame.x + (CanvasMousePositionRaw!.x - hoveringStart.point.x)
                  : features.dragRatio
                  ? shadow.globalFrame.x +
                    interactionSession.drag.x -
                    (shadow.globalFrame.x - interactionSession.dragStart.x) -
                    shadow.globalFrame.width *
                      ((interactionSession.dragStart.x - shadowFrame.x) / shadowFrame.width)
                  : undefined,
              }}
            />
          )}
      </CanvasOffsetWrapper>
    </React.Fragment>
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
