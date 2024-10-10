/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import type { AnimationControls } from 'framer-motion'
import { motion, useAnimationControls } from 'framer-motion'
import type { CSSProperties } from 'react'
import React from 'react'
import type { Sides } from 'utopia-api/core'
import type { ElementPath } from 'utopia-shared/src/types'
import type {
  GridDimension,
  GridDiscreteDimension,
} from '../../../components/inspector/common/css-utils'
import {
  isCSSKeyword,
  isDynamicGridRepeat,
  isGridCSSRepeat,
  isStaticGridRepeat,
  printGridAutoOrTemplateBase,
  printGridCSSNumber,
} from '../../../components/inspector/common/css-utils'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls, stripNulls, uniqBy } from '../../../core/shared/array-utils'
import { defaultEither } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  GridAutoOrTemplateDimensions,
} from '../../../core/shared/element-template'
import {
  isGridAutoOrTemplateDimensions,
  type GridAutoOrTemplateBase,
} from '../../../core/shared/element-template'
import type { CanvasPoint, CanvasRectangle } from '../../../core/shared/math-utils'
import {
  canvasPoint,
  isFiniteRectangle,
  isInfinityRectangle,
  nullIfInfinity,
  pointsEqual,
  scaleRect,
  windowPoint,
  zeroRectangle,
  zeroRectIfNullOrInfinity,
} from '../../../core/shared/math-utils'
import {
  fromArrayIndex,
  fromField,
  fromTypeGuard,
  notNull,
} from '../../../core/shared/optics/optic-creators'
import { toFirst } from '../../../core/shared/optics/optic-utilities'
import type { Optic } from '../../../core/shared/optics/optics'
import { optionalMap } from '../../../core/shared/optional-utils'
import { assertNever } from '../../../core/shared/utils'
import { Modifier } from '../../../utils/modifiers'
import { when } from '../../../utils/react-conditionals'
import { useColorTheme, UtopiaStyles } from '../../../uuiui'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import CanvasActions from '../canvas-actions'
import type {
  ControlWithProps,
  WhenToShowControl,
} from '../canvas-strategies/canvas-strategy-types'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'
import type {
  GridResizeEdge,
  GridResizeEdgeProperties,
} from '../canvas-strategies/interaction-state'
import {
  createInteractionViaMouse,
  gridAxisHandle,
  gridCellHandle,
  gridResizeEdgeProperties,
  GridResizeEdges,
  gridResizeHandle,
} from '../canvas-strategies/interaction-state'
import { resizeBoundingBoxFromSide } from '../canvas-strategies/strategies/resize-helpers'
import type { EdgePosition } from '../canvas-types'
import {
  CSSCursor,
  EdgePositionBottom,
  EdgePositionLeft,
  EdgePositionRight,
  EdgePositionTop,
} from '../canvas-types'
import { windowToCanvasCoordinates } from '../dom-lookup'
import type { Axis } from '../gap-utils'
import { useCanvasAnimation } from '../ui-jsx-canvas-renderer/animation-context'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { CanvasLabel } from './select-mode/controls-common'
import { useMaybeHighlightElement } from './select-mode/select-mode-hooks'
import type { GridCellCoordinates } from '../canvas-strategies/strategies/grid-cell-bounds'
import { gridCellTargetId } from '../canvas-strategies/strategies/grid-cell-bounds'
import {
  getGlobalFrameOfGridCell,
  getGridRelatedIndexes,
} from '../canvas-strategies/strategies/grid-helpers'
import { canResizeGridTemplate } from '../canvas-strategies/strategies/resize-grid-strategy'
import type { GridData } from './grid-controls'
import {
  getNullableAutoOrTemplateBaseString,
  GRID_RESIZE_HANDLE_CONTAINER_SIZE,
  GRID_RESIZE_HANDLE_SIZE,
  GridCellTestId,
  gridEdgeToCSSCursor,
  gridEdgeToEdgePosition,
  gridEdgeToWidthHeight,
  GridResizeEdgeTestId,
  GridResizingControl,
  useGridData,
} from './grid-controls'

const CELL_ANIMATION_DURATION = 0.15 // seconds

export interface GridControlProps {
  grid: GridData
}

export const GridControl = React.memo<GridControlProps>(({ grid }) => {
  const dispatch = useDispatch()
  const controls = useAnimationControls()
  const colorTheme = useColorTheme()

  const editorMetadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'GridControl editorMetadata',
  )

  const interactionLatestMetadata = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession?.interactionData.type === 'DRAG'
        ? store.editor.canvas.interactionSession.latestMetadata
        : null,
    'GridControl interactionLatestMetadata',
  )

  const jsxMetadata = React.useMemo(
    () => interactionLatestMetadata ?? editorMetadata,
    [interactionLatestMetadata, editorMetadata],
  )

  const activelyDraggingOrResizingCell = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession != null &&
      store.editor.canvas.interactionSession.activeControl.type === 'GRID_CELL_HANDLE' &&
      store.editor.canvas.interactionSession?.interactionData.type === 'DRAG' &&
      store.editor.canvas.interactionSession?.interactionData.modifiers.cmd !== true &&
      store.editor.canvas.interactionSession?.interactionData.drag != null
        ? store.editor.canvas.interactionSession.activeControl.id
        : null,
    'GridControl activelyDraggingOrResizingCell',
  )

  const currentHoveredCell = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.controls.gridControlData?.targetCell ?? null,
    'GridControl currentHoveredCell',
  )

  const targetsAreCellsWithPositioning = useEditorState(
    Substores.metadata,
    (store) =>
      store.editor.selectedViews.every((elementPath) =>
        MetadataUtils.isGridCellWithPositioning(store.editor.jsxMetadata, elementPath),
      ),
    'GridControl targetsAreCellsWithPositioning',
  )

  const anyTargetAbsolute = useEditorState(
    Substores.metadata,
    (store) =>
      store.editor.selectedViews.some((elementPath) =>
        MetadataUtils.isPositionAbsolute(
          MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, elementPath),
        ),
      ),
    'GridControl anyTargetAbsolute',
  )

  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'GridControl scale',
  )

  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)

  const startInteractionWithUid = React.useCallback(
    (params: { uid: string; row: number; column: number; frame: CanvasRectangle }) =>
      (event: React.MouseEvent) => {
        setInitialShadowFrame(params.frame)

        const start = windowToCanvasCoordinates(
          scale,
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
    [canvasOffsetRef, dispatch, scale],
  )

  const cells = React.useMemo(() => {
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
            : isCSSKeyword(columnFromProps)
            ? countedColumn
            : columnFromProps.numericalPosition ?? countedColumn,
        row:
          rowFromProps == null
            ? countedRow
            : isCSSKeyword(rowFromProps)
            ? countedRow
            : rowFromProps.numericalPosition ?? countedRow,
        index: index,
      }
    }, children)
  }, [grid, jsxMetadata])

  const dragging = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession != null &&
      store.editor.canvas.interactionSession.activeControl.type === 'GRID_CELL_HANDLE'
        ? store.editor.canvas.interactionSession.activeControl.id
        : null,
    'GridControl dragging',
  )

  const shadow = React.useMemo(() => {
    return cells.find((cell) => EP.toUid(cell.elementPath) === dragging) ?? null
  }, [cells, dragging])

  const [initialShadowFrame, setInitialShadowFrame] = React.useState<CanvasRectangle | null>(
    shadow?.globalFrame ?? null,
  )

  const interactionData = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession?.interactionData.type === 'DRAG'
        ? store.editor.canvas.interactionSession.interactionData
        : null,
    'GridControl interactionData',
  )

  const { hoveringStart } = useMouseMove(activelyDraggingOrResizingCell)

  // NOTE: this stuff is meant to be temporary, until we settle on the set of interaction pieces we like.
  // After that, we should get rid of this.
  const shadowPosition = React.useMemo(() => {
    const drag = interactionData?.drag
    const dragStart = interactionData?.dragStart
    if (
      initialShadowFrame == null ||
      interactionData == null ||
      drag == null ||
      dragStart == null ||
      hoveringStart == null ||
      shadow == null
    ) {
      return null
    }

    const getCoord = (axis: 'x' | 'y', dimension: 'width' | 'height') => {
      return (
        shadow.globalFrame[axis] +
        drag[axis] -
        (shadow.globalFrame[axis] - dragStart[axis]) -
        shadow.globalFrame[dimension] *
          ((dragStart[axis] - initialShadowFrame[axis]) / initialShadowFrame[dimension])
      )
    }

    // make sure the shadow is displayed only inside the grid container bounds
    function wrapCoord(c: number, min: number, max: number, shadowSize: number) {
      return Math.min(Math.max(c, min), max - shadowSize)
    }

    return {
      x: wrapCoord(
        getCoord('x', 'width') ?? 0,
        grid.frame.x,
        grid.frame.x + grid.frame.width,
        shadow.globalFrame.width,
      ),
      y: wrapCoord(
        getCoord('y', 'height') ?? 0,
        grid.frame.y,
        grid.frame.y + grid.frame.height,
        shadow.globalFrame.height,
      ),
    }
  }, [
    interactionData,
    initialShadowFrame,
    hoveringStart,
    shadow,
    grid.frame.x,
    grid.frame.width,
    grid.frame.y,
    grid.frame.height,
  ])

  const gridPath = optionalMap(EP.parentPath, shadow?.elementPath)

  const targetRootCell = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.controls.gridControlData?.rootCell ?? null,
    'GridControl targetRootCell',
  )

  useCellAnimation({
    disabled: anyTargetAbsolute,
    targetRootCell: targetRootCell,
    controls: controls,
    shadowFrame: initialShadowFrame,
    gridPath: gridPath,
  })

  const placeholders = Array.from(Array(grid.cells).keys())
  let style: CSSProperties = {
    position: 'absolute',
    top: grid.frame.y - 1, // account for border!
    left: grid.frame.x - 1, // account for border!
    width: grid.frame.width,
    height: grid.frame.height,
    display: 'grid',
    gridTemplateColumns: getNullableAutoOrTemplateBaseString(grid.gridTemplateColumns),
    gridTemplateRows: getNullableAutoOrTemplateBaseString(grid.gridTemplateRows),
    backgroundColor:
      activelyDraggingOrResizingCell != null ? colorTheme.primary10.value : 'transparent',
    border: `1px solid ${
      activelyDraggingOrResizingCell != null ? colorTheme.primary.value : 'transparent'
    }`,
    justifyContent: grid.justifyContent ?? 'initial',
    alignContent: grid.alignContent ?? 'initial',
    pointerEvents: 'none',
    padding:
      grid.padding == null
        ? 0
        : `${grid.padding.top}px ${grid.padding.right}px ${grid.padding.bottom}px ${grid.padding.left}px`,
  }

  // Gap needs to be set only if the other two are not present or we'll have rendering issues
  // due to how measurements are calculated.
  if (grid.rowGap != null && grid.columnGap != null) {
    style.rowGap = grid.rowGap
    style.columnGap = grid.columnGap
  } else {
    if (grid.gap != null) {
      style.gap = grid.gap
    }
    if (grid.rowGap != null) {
      style.rowGap = grid.rowGap
    }
    if (grid.columnGap != null) {
      style.columnGap = grid.columnGap
    }
  }

  return (
    <React.Fragment>
      {/* grid lines */}
      <GridTrackIndicators grid={grid} />
      <CanvasOffsetWrapper>
        <div
          key={gridKeyFromPath(grid.elementPath)}
          id={gridKeyFromPath(grid.elementPath)}
          data-grid-path={EP.toString(grid.elementPath)}
          style={style}
        >
          {placeholders.map((cell) => {
            const countedRow = Math.floor(cell / grid.columns) + 1
            const countedColumn = Math.floor(cell % grid.columns) + 1
            const id = gridCellTargetId(grid.elementPath, countedRow, countedColumn)
            const borderID = `${id}-border`
            const dotgridColor =
              activelyDraggingOrResizingCell != null
                ? colorTheme.blackOpacity35.value
                : 'transparent'

            const isActiveCell =
              countedColumn === currentHoveredCell?.column && countedRow === currentHoveredCell?.row

            const borderColor =
              isActiveCell && targetsAreCellsWithPositioning
                ? colorTheme.brandNeonPink.value
                : colorTheme.blackOpacity35.value
            return (
              <div
                key={id}
                id={id}
                data-testid={id}
                data-wtf={`data-wtf`}
                style={{
                  position: 'relative',
                  pointerEvents: 'initial',
                }}
                data-grid-row={countedRow}
                data-grid-column={countedColumn}
              >
                <React.Fragment>
                  <div
                    key={borderID}
                    id={borderID}
                    data-testid={borderID}
                    style={{
                      position: 'relative',
                      left: gridPlaceholderTopOrLeftPosition(scale),
                      top: gridPlaceholderTopOrLeftPosition(scale),
                      width: gridPlaceholderWidthOrHeight(scale),
                      height: gridPlaceholderWidthOrHeight(scale),
                      borderTop: gridPlaceholderBorder(borderColor, scale),
                      borderLeft: gridPlaceholderBorder(borderColor, scale),
                      borderBottom:
                        isActiveCell ||
                        countedRow >= grid.rows ||
                        (grid.rowGap != null && grid.rowGap > 0)
                          ? gridPlaceholderBorder(borderColor, scale)
                          : undefined,
                      borderRight:
                        isActiveCell ||
                        countedColumn >= grid.columns ||
                        (grid.columnGap != null && grid.columnGap > 0)
                          ? gridPlaceholderBorder(borderColor, scale)
                          : undefined,
                    }}
                  />
                  <GridDotOverlay dotgridColor={dotgridColor} />
                </React.Fragment>
              </div>
            )
          })}
        </div>
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
              key={GridCellTestId(cell.elementPath)}
              data-testid={GridCellTestId(cell.elementPath)}
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
        {!anyTargetAbsolute &&
        shadow != null &&
        initialShadowFrame != null &&
        interactionData?.dragStart != null &&
        interactionData?.drag != null &&
        hoveringStart != null ? (
          <motion.div
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
              opacity: 0.1,
              border: '1px solid white',
              top: shadowPosition?.y,
              left: shadowPosition?.x,
            }}
          />
        ) : null}
      </CanvasOffsetWrapper>
    </React.Fragment>
  )
})
GridControl.displayName = 'GridControl'

export const GridDotOverlay = React.memo(({ dotgridColor }: { dotgridColor: string }) => {
  return (
    <React.Fragment>
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
    </React.Fragment>
  )
})

function gridKeyFromPath(path: ElementPath): string {
  return `grid-${EP.toString(path)}`
}

const placeholderBorderBaseWidth = 2

function gridPlaceholderBorder(color: string, scale: number): string {
  return `${placeholderBorderBaseWidth / scale}px solid ${color}`
}

function gridPlaceholderTopOrLeftPosition(scale: number): string {
  return `${-placeholderBorderBaseWidth / scale}px`
}

function gridPlaceholderWidthOrHeight(scale: number): string {
  return `calc(100% + ${(placeholderBorderBaseWidth * 2) / scale}px)`
}

function useCellAnimation(params: {
  disabled: boolean
  gridPath: ElementPath | null
  shadowFrame: CanvasRectangle | null
  targetRootCell: GridCellCoordinates | null
  controls: AnimationControls
}) {
  const { gridPath, targetRootCell, controls, shadowFrame, disabled } = params

  const [lastTargetRootCellId, setLastTargetRootCellId] = React.useState(targetRootCell)
  const [lastSnapPoint, setLastSnapPoint] = React.useState<CanvasPoint | null>(shadowFrame)

  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'useSnapAnimation selectedViews',
  )

  const animate = useCanvasAnimation(selectedViews)

  const gridMetadata = useEditorState(
    Substores.metadata,
    (store) => MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, gridPath),
    'useCellAnimation gridMetadata',
  )

  const moveFromPoint = React.useMemo(() => {
    return lastSnapPoint ?? shadowFrame
  }, [lastSnapPoint, shadowFrame])

  const snapPoint = React.useMemo(() => {
    if (gridMetadata == null || targetRootCell == null) {
      return null
    }

    return getGlobalFrameOfGridCell(gridMetadata, targetRootCell)
  }, [gridMetadata, targetRootCell])

  React.useEffect(() => {
    if (disabled) {
      return
    }

    if (targetRootCell != null && snapPoint != null && moveFromPoint != null) {
      const snapPointsDiffer = lastSnapPoint == null || !pointsEqual(snapPoint, lastSnapPoint)
      const hasMovedToANewCell = lastTargetRootCellId != null
      const shouldAnimate = snapPointsDiffer && hasMovedToANewCell
      if (shouldAnimate) {
        void animate(
          {
            scale: [0.97, 1.02, 1], // a very subtle boop
            x: [moveFromPoint.x - snapPoint.x, 0],
            y: [moveFromPoint.y - snapPoint.y, 0],
          },
          {
            duration: CELL_ANIMATION_DURATION,
            type: 'tween',
            ease: 'easeInOut',
          },
        )
      }
    }
    setLastSnapPoint(snapPoint)
    setLastTargetRootCellId(targetRootCell)
  }, [
    targetRootCell,
    controls,
    lastSnapPoint,
    snapPoint,
    animate,
    moveFromPoint,
    lastTargetRootCellId,
    disabled,
  ])
}

function useMouseMove(activelyDraggingOrResizingCell: string | null) {
  const [hoveringStart, setHoveringStart] = React.useState<{
    point: CanvasPoint
  } | null>(null)
  const [mouseCanvasPosition, setMouseCanvasPosition] = React.useState<CanvasPoint>(
    canvasPoint({ x: 0, y: 0 }),
  )

  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'useHoveringCell canvasScale',
  )

  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'useHoveringCell canvasOffset',
  )

  React.useEffect(() => {
    function handleMouseMove(e: MouseEvent) {
      if (activelyDraggingOrResizingCell == null) {
        setHoveringStart(null)
        return
      }

      const newMouseCanvasPosition = windowToCanvasCoordinates(
        canvasScale,
        canvasOffset,
        windowPoint({ x: e.clientX, y: e.clientY }),
      ).canvasPositionRaw
      setMouseCanvasPosition(newMouseCanvasPosition)

      setHoveringStart((start) => {
        if (start == null) {
          return {
            point: canvasPoint(newMouseCanvasPosition),
          }
        }
        return start
      })
    }
    window.addEventListener('mousemove', handleMouseMove)
    return function () {
      window.removeEventListener('mousemove', handleMouseMove)
    }
  }, [activelyDraggingOrResizingCell, canvasOffset, canvasScale])

  return { hoveringStart, mouseCanvasPosition }
}

export const GridTrackIndicators = React.memo(({ grid }: { grid: GridData }) => {
  const targetPath = grid.elementPath
  const metadata = useEditorState(
    Substores.metadata,
    (store) => MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, targetPath),
    'GridTrackIndicators metadata',
  )
  const columnPositions = mapDropNulls(
    (cell) => ({ start: cell.x, length: cell.width }),
    metadata?.specialSizeMeasurements.gridCellGlobalFrames?.[0] ?? [], // this is the first row
  )
  const rowPositions = mapDropNulls(
    (row) => ({ start: row[0].y, length: row[0].height }), // this is the first column
    metadata?.specialSizeMeasurements.gridCellGlobalFrames ?? [],
  )

  return (
    <React.Fragment>
      <CanvasOffsetWrapper limitAxis='x'>
        {columnPositions.map((column, i) => {
          return (
            <React.Fragment key={`column-${i}`}>
              <div
                style={{
                  position: 'absolute',
                  top: 0,
                  left: column.start,
                  width: 1,
                  height: 20,
                  backgroundColor: '#999',
                }}
              />
              <div
                style={{
                  position: 'absolute',
                  top: 0,
                  left: column.start + column.length,
                  width: 1,
                  height: 20,
                  backgroundColor: '#999',
                }}
              />
            </React.Fragment>
          )
        })}
      </CanvasOffsetWrapper>
      <CanvasOffsetWrapper limitAxis='y'>
        {rowPositions.map((row, i) => {
          return (
            <React.Fragment key={`row-${i}`}>
              <div
                style={{
                  position: 'absolute',
                  left: 0,
                  top: row.start,
                  height: 1,
                  width: 20,
                  backgroundColor: '#999',
                }}
              />
              <div
                style={{
                  position: 'absolute',
                  left: 0,
                  top: row.start + row.length,
                  height: 1,
                  width: 20,
                  backgroundColor: '#999',
                }}
              />
            </React.Fragment>
          )
        })}
      </CanvasOffsetWrapper>
    </React.Fragment>
  )
})
GridTrackIndicators.displayName = 'GridTrackIndicators'
