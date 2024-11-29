/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import type {
  GridContainerProperties,
  GridPositionOrSpan,
} from '../../../core/shared/element-template'
import { isGridPositionValue, isGridSpan } from '../../../core/shared/element-template'
import type { CanvasPoint, CanvasRectangle } from '../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  canvasRectangle,
  offsetPoint,
  windowPoint,
} from '../../../core/shared/math-utils'
import { assertNever } from '../../../core/shared/utils'
import { when } from '../../../utils/react-conditionals'
import type { UtopiColor } from '../../../uuiui'
import { useColorTheme } from '../../../uuiui'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { getGridChildCellCoordBoundsFromCanvas } from '../canvas-strategies/strategies/grid-cell-bounds'
import type { GridCellGlobalFrames } from '../canvas-strategies/strategies/grid-helpers'
import { printPin, isAutoGridPin } from '../canvas-strategies/strategies/grid-helpers'
import type { ThemeObject } from '../../../uuiui/styles/theme/theme-helpers'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Modifier } from '../../../utils/modifiers'
import CanvasActions from '../canvas-actions'
import type { GridResizeEdge } from '../canvas-strategies/interaction-state'
import {
  createInteractionViaMouse,
  gridResizeRulerHandle,
} from '../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../dom-lookup'
import { isCSSKeyword } from '../../inspector/common/css-utils'

export type RulerMarkerType = 'span-start' | 'span-end' | 'auto' | 'auto-short' | 'pinned'

export const RulerMarkerIconSize = 11 // px

interface MarkerSVGProps {
  scale: number
}

function MarkerSVG({ scale, children }: React.PropsWithChildren<MarkerSVGProps>) {
  return (
    <svg
      width={`${RulerMarkerIconSize / scale}`}
      height={`${RulerMarkerIconSize / scale}`}
      viewBox={`0 0 ${RulerMarkerIconSize} ${RulerMarkerIconSize}`}
      xmlns='http://www.w3.org/2000/svg'
    >
      {children}
    </svg>
  )
}

function upFacingTriangle(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <MarkerSVG scale={scale}>
      <polygon
        points={`${
          RulerMarkerIconSize / 2
        },0 ${RulerMarkerIconSize},${RulerMarkerIconSize} 0,${RulerMarkerIconSize}`}
        fill={fillColor.value}
      />
    </MarkerSVG>
  )
}

function rightFacingTriangle(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <MarkerSVG scale={scale}>
      <polygon
        points={`${RulerMarkerIconSize},${RulerMarkerIconSize / 2} 0,0 0,${RulerMarkerIconSize}`}
        fill={fillColor.value}
      />
    </MarkerSVG>
  )
}

function downFacingTriangle(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <MarkerSVG scale={scale}>
      <polygon
        points={`${RulerMarkerIconSize / 2},${RulerMarkerIconSize} 0,0 ${RulerMarkerIconSize},0`}
        fill={fillColor.value}
      />
    </MarkerSVG>
  )
}

function leftFacingTriangle(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <MarkerSVG scale={scale}>
      <polygon
        points={`0,${
          RulerMarkerIconSize / 2
        } ${RulerMarkerIconSize},0 ${RulerMarkerIconSize},${RulerMarkerIconSize}`}
        fill={fillColor.value}
      />
    </MarkerSVG>
  )
}

function regularVerticalPipe(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <MarkerSVG scale={scale}>
      <rect x='4' y='0' width='3' height={`${RulerMarkerIconSize}`} fill={fillColor.value} />
    </MarkerSVG>
  )
}

function regularHorizontalPipe(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <MarkerSVG scale={scale}>
      <rect x='0' y='4' width={`${RulerMarkerIconSize}`} height='3' fill={fillColor.value} />
    </MarkerSVG>
  )
}

function shortVerticalPipe(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <MarkerSVG scale={scale}>
      <rect x='4' y='2' width='3' height='9' fill={fillColor.value} />
    </MarkerSVG>
  )
}

function shortHorizontalPipe(fillColor: UtopiColor, scale: number): React.ReactNode {
  return (
    <MarkerSVG scale={scale}>
      <rect x='2' y='4' width='9' height='3' fill={fillColor.value} />
    </MarkerSVG>
  )
}

type ColorToReactNode = (fillColor: UtopiColor, scale: number) => React.ReactNode

export const rulerMarkerIcons: {
  [key in RulerMarkerType]: { column: ColorToReactNode; row: ColorToReactNode }
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
    column: regularVerticalPipe,
    row: regularHorizontalPipe,
  },
  'auto-short': {
    column: shortVerticalPipe,
    row: shortHorizontalPipe,
  },
  pinned: {
    column: downFacingTriangle,
    row: rightFacingTriangle,
  },
}

interface RulerMarkersProps {
  setShowGridCellOutlines: (show: boolean) => void
  path: ElementPath
}

type RulerMarkerData = {
  parentGrid: GridContainerProperties
  cellRect: CanvasRectangle
  gridRect: CanvasRectangle
  otherColumnMarkers: Array<RulerMarkerPositionData>
  otherRowMarkers: Array<RulerMarkerPositionData>
  columnStart: RulerMarkerPositionData
  columnEnd: RulerMarkerPositionData
  rowStart: RulerMarkerPositionData
  rowEnd: RulerMarkerPositionData
}

type RulerMarkerPositionData = {
  markerType: 'selected' | 'target' | 'other'
  rowOrColumn: 'row' | 'column'
  top: number
  left: number
  position: GridPositionOrSpan | null
  bound: 'start' | 'end'
}

export const RulerMarkers = React.memo((props: RulerMarkersProps) => {
  const dispatch = useDispatch()

  const [frozenMarkers, setFrozenMarkers] = React.useState<RulerMarkerData | null>(null)

  const gridRect: CanvasRectangle | null = useEditorState(
    Substores.metadata,
    (store) => {
      return (
        MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, props.path)
          ?.specialSizeMeasurements.parentGridFrame ?? null
      )
    },
    'RulerMarkers gridRect',
  )

  const parentGridCellGlobalFrames = useEditorState(
    Substores.metadata,
    (store) => {
      const elementMetadata = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        props.path,
      )
      if (elementMetadata == null) {
        return null
      }

      return elementMetadata.specialSizeMeasurements.parentGridCellGlobalFrames
    },
    'RulerMarkers parentGridCellGlobalFrames',
  )

  const rulerMarkerData: RulerMarkerData | null = useEditorState(
    Substores.metadata,
    (store) => {
      if (gridRect == null) {
        return null
      }

      if (parentGridCellGlobalFrames == null) {
        return null
      }

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

      const cellRect = parentGridCellGlobalFrames[cellBounds.row - 1][cellBounds.column - 1]
      const cellRectResized = canvasRectangle({
        x: cellRect.x,
        y: cellRect.y,
        width: width,
        height: height,
      })

      let otherColumnMarkers: Array<RulerMarkerPositionData> = []
      let otherRowMarkers: Array<RulerMarkerPositionData> = []

      function addOtherMarker(
        rowOrColumn: RulerMarkerPositionData['rowOrColumn'],
        bound: RulerMarkerPositionData['bound'],
        rulerLeft: number,
        rulerTop: number,
      ): void {
        const otherMarker: RulerMarkerPositionData = {
          markerType: 'other',
          rowOrColumn: rowOrColumn,
          top: rulerTop,
          left: rulerLeft,
          position: null,
          bound: bound,
        }
        const addTo = rowOrColumn === 'row' ? otherRowMarkers : otherColumnMarkers
        addTo.push(otherMarker)
      }

      // Add the additional markers for columns.
      const lastColumnIndex = parentGridCellGlobalFrames[0].length - 1
      for (let columnIndex = 0; columnIndex <= lastColumnIndex; columnIndex++) {
        const cell = parentGridCellGlobalFrames[0][columnIndex]
        if (left !== cell.x) {
          addOtherMarker('column', 'start', cell.x, gridRect.y)
        }
        if (left + width !== cell.x + cell.width) {
          addOtherMarker('column', 'end', cell.x + cell.width, gridRect.y)
        }
      }

      // Add the additional markers for rows.
      const lastRowIndex = parentGridCellGlobalFrames.length - 1
      for (let rowIndex = 0; rowIndex <= lastRowIndex; rowIndex++) {
        const cell = parentGridCellGlobalFrames[rowIndex][0]
        if (top !== cell.y) {
          addOtherMarker('row', 'start', gridRect.x, cell.y)
        }
        if (top + height !== cell.y + cell.height) {
          addOtherMarker('row', 'end', gridRect.x, cell.y + cell.height)
        }
      }

      const columnStart: RulerMarkerPositionData = {
        markerType: 'selected',
        rowOrColumn: 'column',
        top: gridRect.y,
        left: left,
        position: elementGridProperties.gridColumnStart,
        bound: 'start',
      }
      const columnEnd: RulerMarkerPositionData = {
        markerType: 'selected',
        rowOrColumn: 'column',
        top: gridRect.y,
        left: left + width,
        position: elementGridProperties.gridColumnEnd,
        bound: 'end',
      }
      const rowStart: RulerMarkerPositionData = {
        markerType: 'selected',
        rowOrColumn: 'row',
        top: top,
        left: gridRect.x,
        position: elementGridProperties.gridRowStart,
        bound: 'start',
      }
      const rowEnd: RulerMarkerPositionData = {
        markerType: 'selected',
        rowOrColumn: 'row',
        top: top + height,
        left: gridRect.x,
        position: elementGridProperties.gridRowEnd,
        bound: 'end',
      }

      return {
        parentGrid: parentGrid,
        cellRect: cellRectResized,
        gridRect: gridRect,
        otherColumnMarkers: otherColumnMarkers,
        otherRowMarkers: otherRowMarkers,
        columnStart: columnStart,
        columnEnd: columnEnd,
        rowStart: rowStart,
        rowEnd: rowEnd,
      }
    },
    'RulerMarkers markers',
  )

  const [showExtraMarkers, setShowExtraMarkers] = React.useState<'row' | 'column' | null>(null)

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

  const markerMouseUp = React.useCallback(
    (event: MouseEvent) => {
      event.preventDefault()
      event.stopPropagation()
      setShowExtraMarkers(null)
      setFrozenMarkers(null)
      props.setShowGridCellOutlines(false)

      window.removeEventListener('mouseup', markerMouseUp)
    },
    [props],
  )

  const rowMarkerMouseDown = React.useCallback(
    (edge: GridResizeEdge) => (event: React.MouseEvent) => {
      event.preventDefault()
      event.stopPropagation()

      setShowExtraMarkers('row')
      props.setShowGridCellOutlines(true)
      setFrozenMarkers(rulerMarkerData)
      startResizeInteraction(EP.toUid(props.path), edge)(event)

      window.addEventListener('mouseup', markerMouseUp)
    },
    [markerMouseUp, props, startResizeInteraction, rulerMarkerData],
  )

  const columnMarkerMouseDown = React.useCallback(
    (edge: GridResizeEdge) => (event: React.MouseEvent) => {
      event.preventDefault()
      event.stopPropagation()

      setShowExtraMarkers('column')
      props.setShowGridCellOutlines(true)
      setFrozenMarkers(rulerMarkerData)
      startResizeInteraction(EP.toUid(props.path), edge)(event)

      window.addEventListener('mouseup', markerMouseUp)
    },
    [markerMouseUp, props, startResizeInteraction, rulerMarkerData],
  )

  const resizeControlRef = useRefEditorState((store) =>
    store.editor.canvas.interactionSession?.activeControl.type !== 'GRID_RESIZE_RULER_HANDLE'
      ? null
      : store.editor.canvas.interactionSession.activeControl,
  )

  if (rulerMarkerData == null || gridRect == null) {
    return null
  }

  return (
    <React.Fragment>
      <GridRuler
        axis='column'
        gridRect={gridRect}
        cellFrames={parentGridCellGlobalFrames}
        rulerVisible={showExtraMarkers === 'column' ? 'visible' : 'not-visible'}
      >
        {/* Other markers for unselected tracks */}
        {rulerMarkerData.otherColumnMarkers.map((marker, index) => {
          return (
            <RulerMarkerIndicator
              key={`ruler-marker-${index}`}
              gridRect={rulerMarkerData.gridRect}
              parentGrid={rulerMarkerData.parentGrid}
              marker={marker}
              axis={marker.rowOrColumn}
              testID={`ruler-marker-${marker.rowOrColumn}-${index}`}
              visible={showExtraMarkers == marker.rowOrColumn ? 'visible' : 'not-visible'}
            />
          )
        })}
        {/* Selected item markers */}
        <RulerMarkerIndicator
          gridRect={rulerMarkerData.gridRect}
          parentGrid={rulerMarkerData.parentGrid}
          marker={frozenMarkers?.columnStart ?? rulerMarkerData.columnStart}
          axis={'column'}
          visible={'visible'}
          onMouseDown={columnMarkerMouseDown('column-start')}
          testID={'ruler-marker-column-start'}
        />
        <RulerMarkerIndicator
          gridRect={rulerMarkerData.gridRect}
          parentGrid={rulerMarkerData.parentGrid}
          marker={frozenMarkers?.columnEnd ?? rulerMarkerData.columnEnd}
          axis={'column'}
          visible={'visible'}
          onMouseDown={columnMarkerMouseDown('column-end')}
          testID={'ruler-marker-column-end'}
        />
      </GridRuler>

      <GridRuler
        axis='row'
        gridRect={gridRect}
        cellFrames={parentGridCellGlobalFrames}
        rulerVisible={showExtraMarkers === 'row' ? 'visible' : 'not-visible'}
      >
        {/* Other markers for unselected tracks */}
        {rulerMarkerData.otherRowMarkers.map((marker, index) => {
          return (
            <RulerMarkerIndicator
              key={`ruler-marker-${index}`}
              gridRect={rulerMarkerData.gridRect}
              parentGrid={rulerMarkerData.parentGrid}
              marker={marker}
              axis={marker.rowOrColumn}
              testID={`ruler-marker-${marker.rowOrColumn}-${index}`}
              visible={showExtraMarkers == marker.rowOrColumn ? 'visible' : 'not-visible'}
            />
          )
        })}
        {/* Selected item markers */}
        <RulerMarkerIndicator
          gridRect={rulerMarkerData.gridRect}
          parentGrid={rulerMarkerData.parentGrid}
          marker={frozenMarkers?.rowStart ?? rulerMarkerData.rowStart}
          axis={'row'}
          visible={'visible'}
          onMouseDown={rowMarkerMouseDown('row-start')}
          testID={'ruler-marker-row-start'}
        />
        <RulerMarkerIndicator
          gridRect={rulerMarkerData.gridRect}
          parentGrid={rulerMarkerData.parentGrid}
          marker={frozenMarkers?.rowEnd ?? rulerMarkerData.rowEnd}
          axis={'row'}
          visible={'visible'}
          onMouseDown={rowMarkerMouseDown('row-end')}
          testID={'ruler-marker-row-end'}
        />
      </GridRuler>

      {/* Offset lines */}
      <GridCellOffsetLine
        top={rulerMarkerData.columnStart.top}
        left={rulerMarkerData.columnStart.left}
        size={rulerMarkerData.cellRect.y - rulerMarkerData.gridRect.y}
        orientation='vertical'
      />
      <GridCellOffsetLine
        top={rulerMarkerData.columnEnd.top}
        left={rulerMarkerData.columnEnd.left}
        size={rulerMarkerData.cellRect.y - rulerMarkerData.gridRect.y}
        orientation='vertical'
      />
      <GridCellOffsetLine
        top={rulerMarkerData.rowStart.top}
        left={rulerMarkerData.rowStart.left}
        size={rulerMarkerData.cellRect.x - rulerMarkerData.gridRect.x}
        orientation='horizontal'
      />
      <GridCellOffsetLine
        top={rulerMarkerData.rowEnd.top}
        left={rulerMarkerData.rowEnd.left}
        size={rulerMarkerData.cellRect.x - rulerMarkerData.gridRect.x}
        orientation='horizontal'
      />

      {/* Cell outline */}
      <GridCellOutline
        top={rulerMarkerData.cellRect.y}
        left={rulerMarkerData.cellRect.x}
        width={rulerMarkerData.cellRect.width + 1}
        height={rulerMarkerData.cellRect.height + 1}
      />

      {/* Snap line during resize */}
      <SnapLine
        gridTemplate={rulerMarkerData.parentGrid}
        container={rulerMarkerData.gridRect}
        edge={resizeControlRef.current?.edge ?? null}
        target={rulerMarkerData.cellRect}
        markers={rulerMarkerData}
        frozenMarkers={frozenMarkers}
      />

      {/* Offset line during resize, following the mouse */}
      <ResizeOffsetLine
        edge={resizeControlRef.current?.edge ?? null}
        drag={dragRef.current}
        container={rulerMarkerData.gridRect}
      />
    </React.Fragment>
  )
})
RulerMarkers.displayName = 'RulerMarkers'

interface GridRulerProps {
  axis: 'row' | 'column'
  gridRect: CanvasRectangle
  cellFrames: GridCellGlobalFrames | null
  rulerVisible: 'visible' | 'not-visible'
}

const GridRuler = React.memo<React.PropsWithChildren<GridRulerProps>>(
  ({ axis, gridRect, rulerVisible, cellFrames, children }) => {
    const colorTheme = useColorTheme()
    const scale = useEditorState(
      Substores.canvas,
      (store) => store.editor.canvas.scale,
      'GridRuler scale',
    )

    // Make sure the ruler extends to cover all the cells and not just the grid dimensions.
    const cellMaxRect = React.useMemo(() => {
      return boundingRectangleArray([gridRect, ...(cellFrames?.flat() ?? [])]) ?? gridRect
    }, [cellFrames, gridRect])

    const columnLeft = gridRect.x
    const rowLeft = gridRect.x - RulerMarkerIconSize / scale
    const left = axis === 'row' ? rowLeft : columnLeft

    const columnTop = gridRect.y - RulerMarkerIconSize / scale
    const rowTop = gridRect.y
    const top = axis === 'row' ? rowTop : columnTop

    const width = axis === 'row' ? RulerMarkerIconSize / scale : cellMaxRect.width
    const height = axis === 'row' ? cellMaxRect.height : RulerMarkerIconSize / scale

    return (
      <div
        data-testid={`ruler-${axis}`}
        style={{
          position: 'absolute',
          left: left,
          top: top,
          width: width,
          height: height,
          backgroundColor: rulerVisible === 'visible' ? colorTheme.white.value : 'transparent',
          outlineWidth: '0.2px',
          outlineStyle: 'solid',
          outlineColor: rulerVisible === 'visible' ? colorTheme.black.value : 'transparent',
        }}
      >
        {children}
      </div>
    )
  },
)
GridRuler.displayName = 'GridRuler'

const RulerMarkerIndicator = React.memo(
  (props: {
    gridRect: CanvasRectangle
    parentGrid: GridContainerProperties
    marker: RulerMarkerPositionData
    axis: 'row' | 'column'
    testID: string
    visible: 'visible' | 'not-visible'
    onMouseDown?: (event: React.MouseEvent) => void
  }) => {
    const colorTheme = useColorTheme()

    const canvasScale = useEditorState(
      Substores.canvasOffset,
      (store) => store.editor.canvas.scale,
      'RulerMarkerIndicator canvasScale',
    )

    const markerType = getRulerMarkerType(props.marker)
    const markerColor = getRulerMarkerColor(colorTheme, props.marker)
    const markerIcon = rulerMarkerIcons[markerType][props.axis](markerColor, canvasScale)

    const scaledTop =
      props.axis === 'row'
        ? props.marker.top - props.gridRect.y - RulerMarkerIconSize / canvasScale / 2 + 0.5
        : 0

    const scaledLeft =
      props.axis === 'column'
        ? props.marker.left - props.gridRect.x - RulerMarkerIconSize / canvasScale / 2 + 0.5
        : 0

    const labelText = React.useMemo(() => {
      if (props.marker.position == null) {
        return null
      }
      return printPin(props.parentGrid, props.marker.position, props.axis)
    }, [props.marker, props.parentGrid, props.axis])

    const labelClass = 'ruler-marker-label'

    const resizeControlRef = useRefEditorState((store) =>
      store.editor.canvas.interactionSession?.activeControl.type !== 'GRID_RESIZE_RULER_HANDLE'
        ? null
        : store.editor.canvas.interactionSession.activeControl,
    )
    const edge: GridResizeEdge =
      props.axis === 'column'
        ? props.marker.bound === 'start'
          ? 'column-start'
          : 'column-end'
        : props.marker.bound === 'start'
        ? 'row-start'
        : 'row-end'

    return (
      <div
        data-testid={props.testID}
        style={{
          position: 'absolute',
          top: scaledTop,
          left: scaledLeft,
          color: colorTheme.primary.value,
          height: RulerMarkerIconSize / canvasScale,
          width: RulerMarkerIconSize / canvasScale,
          display: props.visible === 'visible' ? 'flex' : 'none',
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
              borderRadius: 2 / canvasScale,
              padding: `${3 / canvasScale}px ${6 / canvasScale}px`,
              color: colorTheme.white.value,
              height: 20 / canvasScale,
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
              top: props.axis === 'column' ? -23 / canvasScale : 0,
              left: props.axis === 'column' ? 0 : undefined,
              right: props.axis === 'row' ? (RulerMarkerIconSize + 1) / canvasScale : undefined,
              fontSize: 11 / canvasScale,
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

    const canvasScale = useEditorState(
      Substores.canvasOffset,
      (store) => store.editor.canvas.scale,
      'SnapLine canvasScale',
    )

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

    const labelWidth = 50
    const labelHeight = 20

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
              top: axis === 'column' ? -labelHeight - RulerMarkerIconSize - 5 : -10,
              left: axis === 'row' ? -(labelWidth - RulerMarkerIconSize + 30) : 0,
              color: colorTheme.brandNeonPink.value,
              fontWeight: 700,
              textAlign: axis === 'row' ? 'right' : undefined,
              width: labelWidth,
              height: labelHeight,
              zoom: 1 / canvasScale,
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

function getRulerMarkerType(marker: RulerMarkerPositionData): RulerMarkerType {
  const isAuto = isAutoGridPin(marker.position)
  const isSpanStart = marker.bound === 'start' && isGridSpan(marker.position)
  const isSpanEnd = marker.bound === 'end' && isGridSpan(marker.position)

  if (marker.markerType === 'other' || marker.markerType === 'target') {
    return 'auto-short'
  } else if (isSpanStart) {
    return 'span-start'
  } else if (isSpanEnd) {
    return 'span-end'
  } else if (isAuto) {
    return 'auto'
  } else {
    return 'pinned'
  }
}

function getRulerMarkerColor(colorTheme: ThemeObject, marker: RulerMarkerPositionData): UtopiColor {
  switch (marker.markerType) {
    case 'selected':
      return colorTheme.primary
    case 'target':
      return colorTheme.brandPurple
    case 'other':
      return colorTheme.grey65
    default:
      assertNever(marker.markerType)
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
