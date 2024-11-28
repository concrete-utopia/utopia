/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { colorTheme as ColorTheme, useColorTheme } from '../../../uuiui'
import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type {
  GridContainerProperties,
  GridPositionOrSpan,
} from '../../../core/shared/element-template'
import { isGridPositionValue, isGridSpan } from '../../../core/shared/element-template'
import type { CanvasPoint, CanvasRectangle } from '../../../core/shared/math-utils'
import { canvasRectangle, offsetPoint, windowPoint } from '../../../core/shared/math-utils'
import { Modifier } from '../../../utils/modifiers'
import { useDispatch } from '../../editor/store/dispatch-context'
import { useEditorState, Substores, useRefEditorState } from '../../editor/store/store-hook'
import CanvasActions from '../canvas-actions'
import type { GridResizeEdge } from '../canvas-strategies/interaction-state'
import {
  createInteractionViaMouse,
  gridResizeRulerHandle,
} from '../canvas-strategies/interaction-state'
import { getGridChildCellCoordBoundsFromCanvas } from '../canvas-strategies/strategies/grid-cell-bounds'
import {
  findOriginalGrid,
  printPin,
  isAutoGridPin,
} from '../canvas-strategies/strategies/grid-helpers'
import { windowToCanvasCoordinates } from '../dom-lookup'
import { when } from '../../../utils/react-conditionals'
import * as EP from '../../../core/shared/element-path'
import { assertNever } from '../../../core/shared/utils'
import { isCSSKeyword } from '../../inspector/common/css-utils'

export type RulerMarkerType = 'span-start' | 'span-end' | 'auto' | 'pinned'

const upFacingTriangle = (
  <svg width='11' height='11' xmlns='http://www.w3.org/2000/svg'>
    <polygon
      points='5,1 10,10 0,10'
      fill={ColorTheme.primary.value}
      stroke={ColorTheme.white.value}
      strokeWidth='0.5'
    />
  </svg>
)

const rightFacingTriangle = (
  <svg width='11' height='11' xmlns='http://www.w3.org/2000/svg'>
    <polygon
      points='10,5 0,0 0,10'
      fill={ColorTheme.primary.value}
      stroke={ColorTheme.white.value}
      strokeWidth='0.5'
    />
  </svg>
)

const downFacingTriangle = (
  <svg width='11' height='11' xmlns='http://www.w3.org/2000/svg'>
    <polygon
      points='5,10 0,0 10,0'
      fill={ColorTheme.primary.value}
      stroke={ColorTheme.white.value}
      strokeWidth='0.5'
    />
  </svg>
)

const leftFacingTriangle = (
  <svg width='11' height='11' xmlns='http://www.w3.org/2000/svg'>
    <polygon
      points='0,5 10,0 10,10'
      fill={ColorTheme.primary.value}
      stroke={ColorTheme.white.value}
      strokeWidth='0.5'
    />
  </svg>
)

const verticalPipe = (
  <svg width='4' height='11' xmlns='http://www.w3.org/2000/svg'>
    <rect
      x='0.25'
      y='0.25'
      width='3'
      height='10'
      fill={ColorTheme.primary.value}
      stroke={ColorTheme.white.value}
      strokeWidth='0.5'
    />
  </svg>
)

const horizontalPipe = (
  <svg width='11' height='11' xmlns='http://www.w3.org/2000/svg'>
    <rect
      x='0.25'
      y='3.50'
      width='10'
      height='3'
      fill={ColorTheme.primary.value}
      stroke={ColorTheme.white.value}
      strokeWidth='0.5'
    />
  </svg>
)

export const rulerMarkerIcons: {
  [key in RulerMarkerType]: { column: React.ReactNode; row: React.ReactNode }
} = {
  'span-start': {
    column: rightFacingTriangle,
    row: downFacingTriangle,
  },
  'span-end': {
    column: leftFacingTriangle,
    row: upFacingTriangle,
  },
  auto: {
    column: verticalPipe,
    row: horizontalPipe,
  },
  pinned: {
    column: downFacingTriangle,
    row: rightFacingTriangle,
  },
}

export const RulerMarkers = React.memo((props: { path: ElementPath }) => {
  const dispatch = useDispatch()

  const [frozenMarkers, setFrozenMarkers] = React.useState<RulerMarkerData | null>(null)

  const markers: RulerMarkerData | null = useEditorState(
    Substores.metadata,
    (store) => {
      const elementMetadata = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        props.path,
      )
      if (elementMetadata == null) {
        return null
      }

      const elementGridProperties = elementMetadata.specialSizeMeasurements.elementGridProperties
      if (elementGridProperties == null) {
        return null
      }

      const parentGrid = elementMetadata.specialSizeMeasurements.parentContainerGridProperties

      const originalGrid = findOriginalGrid(store.editor.jsxMetadata, EP.parentPath(props.path))
      if (originalGrid == null) {
        return null
      }

      const parentGridCellGlobalFrames =
        elementMetadata.specialSizeMeasurements.parentGridCellGlobalFrames
      if (parentGridCellGlobalFrames == null) {
        return null
      }

      const cellBounds = getGridChildCellCoordBoundsFromCanvas(
        elementMetadata,
        parentGridCellGlobalFrames,
      )
      if (cellBounds == null) {
        return null
      }

      if (parentGridCellGlobalFrames.length === 0) {
        return null
      }
      const firstRow = parentGridCellGlobalFrames[0]
      const cellBoundsColumnIndex = cellBounds.column - 1
      const left = firstRow[cellBoundsColumnIndex].x
      const width = getCellCanvasWidthFromBounds(
        parentGridCellGlobalFrames,
        cellBoundsColumnIndex,
        cellBounds.width,
      )

      const cellBoundsRowIndex = cellBounds.row - 1
      if (
        parentGridCellGlobalFrames.length <= cellBoundsRowIndex ||
        parentGridCellGlobalFrames[cellBoundsRowIndex].length === 0
      ) {
        return null
      }
      const firstColumn = parentGridCellGlobalFrames[cellBoundsRowIndex][0]
      const top = firstColumn.y
      const height = getCellCanvasHeightFromBounds(
        parentGridCellGlobalFrames,
        cellBoundsRowIndex,
        cellBounds.height,
      )

      const gridRect = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
        originalGrid,
        store.editor.jsxMetadata,
      )

      const cellRect = parentGridCellGlobalFrames[cellBounds.row - 1][cellBounds.column - 1]
      const cellRectResized = canvasRectangle({
        x: cellRect.x,
        y: cellRect.y,
        width: width,
        height: height,
      })

      return {
        columns: parentGridCellGlobalFrames[0],
        rows: parentGridCellGlobalFrames.map((r) => r[0]),
        parentGrid: parentGrid,
        cellRect: cellRectResized,
        gridRect: gridRect,
        columnStart: {
          top: gridRect.y,
          left: left,
          position: elementGridProperties.gridColumnStart,
          bound: 'start',
        },
        columnEnd: {
          top: gridRect.y,
          left: left + width,
          position: elementGridProperties.gridColumnEnd,
          bound: 'end',
        },
        rowStart: {
          top: top,
          left: gridRect.x,
          position: elementGridProperties.gridRowStart,
          bound: 'start',
        },
        rowEnd: {
          top: top + height,
          left: gridRect.x,
          position: elementGridProperties.gridRowEnd,
          bound: 'end',
        },
      }
    },
    'RulerMarkers markers',
  )

  const dragRef = useRefEditorState((store) =>
    store.editor.canvas.interactionSession?.activeControl.type === 'GRID_RESIZE_RULER_HANDLE' &&
    store.editor.canvas.interactionSession?.interactionData.type === 'DRAG' &&
    store.editor.canvas.interactionSession?.interactionData.drag != null
      ? offsetPoint(
          store.editor.canvas.interactionSession?.interactionData.dragStart,
          store.editor.canvas.interactionSession?.interactionData.drag,
        )
      : null,
  )

  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'RulerMarkers canvasScale',
  )

  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)

  const startResizeInteraction = React.useCallback(
    (uid: string, edge: GridResizeEdge) => (e: React.MouseEvent) => {
      e.stopPropagation()

      const start = windowToCanvasCoordinates(
        canvasScale,
        canvasOffsetRef.current,
        windowPoint({ x: e.nativeEvent.x, y: e.nativeEvent.y }),
      )
      dispatch([
        CanvasActions.createInteractionSession(
          createInteractionViaMouse(
            start.canvasPositionRounded,
            Modifier.modifiersForEvent(e),
            gridResizeRulerHandle(uid, edge),
            'zero-drag-not-permitted',
          ),
        ),
      ])
    },
    [canvasOffsetRef, dispatch, canvasScale],
  )

  const onMouseDownMarker = React.useCallback(
    (edge: GridResizeEdge) => (e: React.MouseEvent) => {
      e.preventDefault()
      e.stopPropagation()

      setFrozenMarkers(markers)

      startResizeInteraction(EP.toUid(props.path), edge)(e)

      const mouseup = () => {
        setFrozenMarkers(null)

        window.removeEventListener('mouseup', mouseup)
      }

      window.addEventListener('mouseup', mouseup)
    },
    [markers, props.path, startResizeInteraction],
  )

  const resizeControlRef = useRefEditorState((store) =>
    store.editor.canvas.interactionSession?.activeControl.type !== 'GRID_RESIZE_RULER_HANDLE'
      ? null
      : store.editor.canvas.interactionSession.activeControl,
  )

  if (markers == null) {
    return null
  }

  return (
    <React.Fragment>
      {/* Indicators */}
      <RulerMarkerIndicator
        path={props.path}
        cellRect={markers.cellRect}
        parentGrid={markers.parentGrid}
        marker={frozenMarkers?.columnStart ?? markers.columnStart}
        axis={'column'}
        containerSize={markers.gridRect.height}
        onMouseDown={onMouseDownMarker('column-start')}
      />
      <RulerMarkerIndicator
        path={props.path}
        cellRect={markers.cellRect}
        parentGrid={markers.parentGrid}
        marker={frozenMarkers?.columnEnd ?? markers.columnEnd}
        axis={'column'}
        containerSize={markers.gridRect.height}
        onMouseDown={onMouseDownMarker('column-end')}
      />
      <RulerMarkerIndicator
        path={props.path}
        cellRect={markers.cellRect}
        parentGrid={markers.parentGrid}
        marker={frozenMarkers?.rowStart ?? markers.rowStart}
        axis={'row'}
        containerSize={markers.gridRect.width}
        onMouseDown={onMouseDownMarker('row-start')}
      />
      <RulerMarkerIndicator
        path={props.path}
        cellRect={markers.cellRect}
        parentGrid={markers.parentGrid}
        marker={frozenMarkers?.rowEnd ?? markers.rowEnd}
        axis={'row'}
        containerSize={markers.gridRect.width}
        onMouseDown={onMouseDownMarker('row-end')}
      />

      {/* Offset lines */}
      <GridCellOffsetLine
        top={markers.columnStart.top}
        left={markers.columnStart.left}
        size={markers.cellRect.y - markers.gridRect.y}
        orientation='vertical'
      />
      <GridCellOffsetLine
        top={markers.columnEnd.top}
        left={markers.columnEnd.left}
        size={markers.cellRect.y - markers.gridRect.y}
        orientation='vertical'
      />
      <GridCellOffsetLine
        top={markers.rowStart.top}
        left={markers.rowStart.left}
        size={markers.cellRect.x - markers.gridRect.x}
        orientation='horizontal'
      />
      <GridCellOffsetLine
        top={markers.rowEnd.top}
        left={markers.rowEnd.left}
        size={markers.cellRect.x - markers.gridRect.x}
        orientation='horizontal'
      />

      {/* Cell outline */}
      <GridCellOutline
        top={markers.cellRect.y}
        left={markers.cellRect.x}
        width={markers.cellRect.width + 1}
        height={markers.cellRect.height + 1}
      />

      {/* Snap line during resize */}
      <SnapLine
        gridTemplate={markers.parentGrid}
        container={markers.gridRect}
        edge={resizeControlRef.current?.edge ?? null}
        target={markers.cellRect}
        markers={markers}
        frozenMarkers={frozenMarkers}
      />

      {/* Offset line during resize, following the mouse */}
      <ResizeOffsetLine
        edge={resizeControlRef.current?.edge ?? null}
        drag={dragRef.current}
        container={markers.gridRect}
      />
    </React.Fragment>
  )
})
RulerMarkers.displayName = 'RulerMarkers'

const ResizeOffsetLine = React.memo(
  (props: {
    edge: GridResizeEdge | null
    drag: CanvasPoint | null
    container: CanvasRectangle
  }) => {
    const colorTheme = useColorTheme()

    if (props.edge == null || props.drag == null) {
      return null
    }
    const isColumn = props.edge === 'column-start' || props.edge === 'column-end'

    return (
      <div
        style={{
          position: 'absolute',
          width: isColumn ? 1 : props.container.width,
          height: !isColumn ? 1 : props.container.height,
          top: isColumn ? props.container.y : props.drag.y,
          left: !isColumn ? props.container.x : props.drag.x,
          borderLeft: isColumn ? `1px dashed ${colorTheme.primary.value}` : undefined,
          borderTop: !isColumn ? `1px dashed ${colorTheme.primary.value}` : undefined,
        }}
      />
    )
  },
)
ResizeOffsetLine.displayName = 'LiveOffsetLine'

const SnapLine = React.memo(
  (props: {
    gridTemplate: GridContainerProperties
    edge: GridResizeEdge | null
    container: CanvasRectangle
    target: CanvasRectangle
    markers: RulerMarkerData
    frozenMarkers: RulerMarkerData | null
  }) => {
    const colorTheme = useColorTheme()

    const [targetMarker, targetFrozenMarker] = React.useMemo(() => {
      if (props.edge == null || props.frozenMarkers == null) {
        return []
      }
      switch (props.edge) {
        case 'column-end':
          return [props.markers.columnEnd, props.frozenMarkers.columnEnd]
        case 'column-start':
          return [props.markers.columnStart, props.frozenMarkers.columnStart]
        case 'row-end':
          return [props.markers.rowEnd, props.frozenMarkers.rowEnd]
        case 'row-start':
          return [props.markers.rowStart, props.frozenMarkers.rowStart]
        default:
          assertNever(props.edge)
      }
    }, [props.edge, props.markers, props.frozenMarkers])

    const axis = props.edge === 'column-end' || props.edge === 'column-start' ? 'column' : 'row'

    if (
      props.edge == null ||
      targetMarker == null ||
      targetMarker.position == null ||
      targetFrozenMarker == null ||
      targetFrozenMarker.position == null
    ) {
      return null
    }

    const showLabel = !gridPositionOrSpanEquals(targetMarker.position, targetFrozenMarker.position)

    const isColumn = props.edge === 'column-start' || props.edge === 'column-end'
    return (
      <div
        style={{
          position: 'absolute',
          width: isColumn ? 1 : props.container.width,
          height: !isColumn ? 1 : props.container.height,
          top: isColumn
            ? props.container.y
            : props.target.y + (props.edge === 'row-end' ? props.target.height : 0),
          left: !isColumn
            ? props.container.x
            : props.target.x + (props.edge === 'column-end' ? props.target.width : 0),
          backgroundColor: colorTheme.brandNeonPink.value,
        }}
      >
        {when(
          showLabel,
          <div
            style={{
              position: 'absolute',
              top: axis === 'column' ? -22 : -10,
              left: axis === 'row' ? -(50 + 10) : 0,
              color: colorTheme.brandNeonPink.value,
              fontWeight: 700,
              textAlign: axis === 'row' ? 'right' : undefined,
              width: 50,
            }}
          >
            {printPin(props.gridTemplate, targetMarker.position, axis)}
          </div>,
        )}
      </div>
    )
  },
)
SnapLine.displayName = 'SnapLine'

const RulerMarkerIndicator = React.memo(
  (props: {
    path: ElementPath
    parentGrid: GridContainerProperties
    cellRect: CanvasRectangle
    marker: RulerMarkerPositionData
    axis: 'row' | 'column'
    containerSize: number
    onMouseDown: (e: React.MouseEvent) => void
  }) => {
    const colorTheme = useColorTheme()

    const canvasScale = useEditorState(
      Substores.canvasOffset,
      (store) => store.editor.canvas.scale,
      'RulerMarkerIndicator canvasScale',
    )

    const resizeControlRef = useRefEditorState((store) =>
      store.editor.canvas.interactionSession?.activeControl.type !== 'GRID_RESIZE_RULER_HANDLE'
        ? null
        : store.editor.canvas.interactionSession.activeControl,
    )

    const labelClass = 'ruler-marker-label'

    const edge: GridResizeEdge =
      props.axis === 'column'
        ? props.marker.bound === 'start'
          ? 'column-start'
          : 'column-end'
        : props.marker.bound === 'start'
        ? 'row-start'
        : 'row-end'

    const labelText = React.useMemo(() => {
      if (props.marker.position == null) {
        return null
      }
      return printPin(props.parentGrid, props.marker.position, props.axis)
    }, [props.marker, props.parentGrid, props.axis])

    const markerType = getRulerMarkerType({
      position: props.marker.position,
      bound: props.marker.bound,
    })
    const markerIcon = rulerMarkerIcons[markerType][props.axis]

    const scaledTop = props.marker.top * canvasScale
    const top =
      scaledTop -
      skewMarkerPosition(props.axis === 'column', props.axis, props.marker.bound, markerType)

    const scaledLeft = props.marker.left * canvasScale
    const left =
      scaledLeft -
      skewMarkerPosition(props.axis === 'row', props.axis, props.marker.bound, markerType)

    return (
      <div
        style={{
          position: 'absolute',
          top: top,
          left: left,
          color: colorTheme.primary.value,
          maxHeight: rulerMarkerIconSize,
          maxWidth: rulerMarkerIconSize,
          display: 'flex',
          zoom: 1 / canvasScale,
          pointerEvents: resizeControlRef.current != null ? 'none' : undefined,
        }}
        onMouseDown={props.onMouseDown}
        css={{
          [`> .${labelClass}`]: {
            visibility:
              resizeControlRef.current != null && resizeControlRef.current.edge === edge
                ? 'visible'
                : 'hidden',
          },
          ':hover': {
            [`> .${labelClass}`]: {
              visibility: 'visible',
            },
          },
        }}
      >
        {markerIcon}
        {when(
          labelText != null,
          <div
            className={labelClass}
            style={{
              position: 'absolute',
              background: colorTheme.primary.value,
              borderRadius: 2,
              padding: '3px 6px',
              color: colorTheme.white.value,
              height: 20,
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
              top: props.axis === 'column' ? -23 : 0,
              left: props.axis === 'column' ? 0 : undefined,
              right: props.axis === 'row' ? rulerMarkerIconSize + 1 : undefined,
            }}
          >
            {labelText}
          </div>,
        )}
      </div>
    )
  },
)
RulerMarkerIndicator.displayName = 'RulerMarkerIndicator'

function getRulerMarkerType(props: {
  position: GridPositionOrSpan | null
  bound: 'start' | 'end'
}): RulerMarkerType {
  const isAuto = isAutoGridPin(props.position)
  const isSpanStart = props.bound === 'start' && isGridSpan(props.position)
  const isSpanEnd = props.bound === 'end' && isGridSpan(props.position)

  if (isSpanStart) {
    return 'span-start'
  } else if (isSpanEnd) {
    return 'span-end'
  } else if (isAuto) {
    return 'auto'
  } else {
    return 'pinned'
  }
}

function getCellCanvasWidthFromBounds(
  grid: CanvasRectangle[][],
  index: number,
  cells: number,
): number {
  if (grid.length === 0) {
    return 0
  }

  const currentRow = grid[0]
  if (currentRow.length <= index) {
    return 0
  }
  if (cells <= 1) {
    return currentRow[index].width
  }

  function getPadding() {
    if (currentRow.length <= 1) {
      return 0
    }
    return currentRow[1].x - (currentRow[0].x + currentRow[0].width)
  }
  const padding = getPadding()

  return currentRow.slice(index + 1, index + cells).reduce((acc, curr) => {
    return acc + curr.width + padding
  }, currentRow[index].width)
}

function getCellCanvasHeightFromBounds(
  grid: CanvasRectangle[][],
  index: number,
  cells: number,
): number {
  const columns = grid.map((row) => row[0])
  if (columns.length <= index) {
    return 0
  }

  const currentColumn = columns[index]

  if (cells <= 1) {
    return currentColumn.height
  }

  function getPadding() {
    if (grid.length <= 1) {
      return 0
    }
    return grid[1][0].y - (grid[0][0].y + grid[0][0].height)
  }
  const padding = getPadding()

  return columns.slice(index + 1, index + cells).reduce((acc, curr) => {
    return acc + curr.height + padding
  }, currentColumn.height)
}

// This function returns the amount of pixels used to adjust the position of
// individual ruler markers, which need specific skews based on their shape.
function skewMarkerPosition(
  isOnTheSameAxis: boolean,
  axis: 'column' | 'row',
  bound: 'start' | 'end',
  markerType: RulerMarkerType,
): number {
  if (isOnTheSameAxis) {
    return rulerMarkerIconSize
  }

  // span-end triangle, on the column
  const spanEndColumn = axis === 'column' && markerType === 'span-end'
  if (spanEndColumn) {
    return 9
  }
  // pinned triangle, on the column
  const pinnedEndColumn = axis === 'column' && markerType === 'pinned' && bound === 'end'
  if (pinnedEndColumn) {
    return 5
  }
  // any other ending marker, on the column
  const endColumn = bound === 'end' && axis === 'column'
  if (endColumn) {
    return 1
  }

  // span-end triangle, on the row
  const spanEndRow = axis === 'row' && markerType === 'span-end'
  if (spanEndRow) {
    return 9
  }
  // any other ending marker, on the row
  const endRow = bound === 'end' && axis === 'row'
  if (endRow) {
    return 4
  }

  // span-start triangle, on the column
  const spanStartColumn = axis === 'column' && markerType === 'span-start'
  if (spanStartColumn) {
    return 0
  }
  const pinnedStartColumn = axis === 'column' && markerType === 'pinned' && bound === 'start'
  if (pinnedStartColumn) {
    return 5
  }
  // any starting marker, on the column
  const startColumn = bound === 'start' && axis === 'column'
  if (startColumn) {
    return 1
  }

  // span-start starting triangle, on the row
  const spanStartRow = axis === 'row' && markerType === 'span-start'
  if (spanStartRow) {
    return 0
  }
  // any other starting marker, on the row
  const startRow = bound === 'start' && axis === 'row'
  if (startRow) {
    return 4
  }

  return 0
}

const rulerMarkerIconSize = 12 // px

type RulerMarkerData = {
  parentGrid: GridContainerProperties
  cellRect: CanvasRectangle
  gridRect: CanvasRectangle
  columnStart: RulerMarkerPositionData
  columnEnd: RulerMarkerPositionData
  rowStart: RulerMarkerPositionData
  rowEnd: RulerMarkerPositionData
  columns: CanvasRectangle[]
  rows: CanvasRectangle[]
}

type RulerMarkerPositionData = {
  top: number
  left: number
  position: GridPositionOrSpan | null
  bound: 'start' | 'end'
}

const GridCellOffsetLine = React.memo(
  (props: { top: number; left: number; size: number; orientation: 'vertical' | 'horizontal' }) => {
    const colorTheme = useColorTheme()

    return (
      <div
        style={{
          position: 'absolute',
          top: props.top,
          left: props.left,
          width: props.orientation === 'horizontal' ? props.size : 1,
          height: props.orientation === 'vertical' ? props.size : 1,
          borderLeft:
            props.orientation === 'vertical' ? `1px dashed ${colorTheme.primary.value}` : undefined,
          borderTop:
            props.orientation === 'horizontal'
              ? `1px dashed ${colorTheme.primary.value}`
              : undefined,
          pointerEvents: 'none',
        }}
      />
    )
  },
)
GridCellOffsetLine.displayName = 'GridCellOffsetLine'

const GridCellOutline = React.memo(
  (props: { top: number; left: number; width: number; height: number }) => {
    const colorTheme = useColorTheme()

    return (
      <div
        style={{
          position: 'absolute',
          top: props.top,
          left: props.left,
          width: props.width,
          height: props.height,
          border: `1px dashed ${colorTheme.primary.value}`,
          pointerEvents: 'none',
        }}
      />
    )
  },
)
GridCellOutline.displayName = 'GridCellOutline'

function gridPositionOrSpanEquals(a: GridPositionOrSpan, b: GridPositionOrSpan): boolean {
  if (isGridSpan(a)) {
    if (!isGridSpan(b)) {
      return false
    }
    return a.value === b.value
  }
  if (isCSSKeyword(a)) {
    if (!isCSSKeyword(b)) {
      return false
    }
    return a.value === b.value
  }
  if (!isGridPositionValue(b)) {
    return false
  }
  return a.numericalPosition === b.numericalPosition
}
