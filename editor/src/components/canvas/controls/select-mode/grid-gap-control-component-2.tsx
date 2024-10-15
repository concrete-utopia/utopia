import type { CSSProperties } from 'react'
import React from 'react'
import { createArrayWithLength, interleaveArray } from '../../../../core/shared/array-utils'
import type { GridAutoOrTemplateBase } from '../../../../core/shared/element-template'
import { NO_OP } from '../../../../core/shared/utils'
import { useColorTheme } from '../../../../uuiui'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import type { CSSNumber } from '../../../inspector/common/css-utils'
import {
  cssNumber,
  printGridAutoOrTemplateBase,
  stringifyGridDimension,
} from '../../../inspector/common/css-utils'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import type { GridData } from '../grid-controls-for-strategies'
import { getNullableAutoOrTemplateBaseString, useGridData } from '../grid-controls-for-strategies'
import { getStyleMatchingTargetGrid } from '../grid-controls-helpers'
import {
  GridGapHandle,
  startInteraction,
  type GridGapControlProps,
} from './grid-gap-control-component'
import { useDispatch } from '../../../editor/store/dispatch-context'
import type { Axis } from '../../gap-utils'

export const GridGapControlComponent2 = React.memo<GridGapControlProps>((props) => {
  const { selectedElement, updatedGapValueRow, updatedGapValueColumn } = props

  const dispatch = useDispatch()
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'GridGapControlComponent2 scale',
  )

  const grid = useGridData([selectedElement]).at(0)

  const activeDraggingAxis = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession?.activeControl.type === 'GRID_GAP_HANDLE'
        ? store.editor.canvas.interactionSession?.activeControl.axis
        : null,
    'GridGapControl isDragging',
  )

  const canvasOffset = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)

  const axisMouseDownHandler = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>, axis: Axis) => {
      startInteraction(e, dispatch, canvasOffset.current, scale, axis)
    },
    [canvasOffset, dispatch, scale],
  )
  const rowMouseDownHandler = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => axisMouseDownHandler(e, 'row'),
    [axisMouseDownHandler],
  )

  const columnMouseDownHandler = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => axisMouseDownHandler(e, 'column'),
    [axisMouseDownHandler],
  )

  if (grid == null) {
    return null
  }

  return (
    <CanvasOffsetWrapper>
      <GridPaddingOutlineForDimension
        grid={grid}
        dimension={'rows'}
        onMouseDown={rowMouseDownHandler}
        beingDragged={activeDraggingAxis === 'row'}
      />
      <GridPaddingOutlineForDimension
        grid={grid}
        dimension={'columns'}
        onMouseDown={columnMouseDownHandler}
        beingDragged={activeDraggingAxis === 'column'}
      />
    </CanvasOffsetWrapper>
  )
})

const GridPaddingOutlineForDimension = (props: {
  grid: GridData
  dimension: 'rows' | 'columns'
  onMouseDown: (e: React.MouseEvent<HTMLDivElement>) => void
  beingDragged: boolean
}) => {
  const { grid, dimension, onMouseDown, beingDragged } = props

  let style: CSSProperties = {
    ...getStyleMatchingTargetGrid(grid),
    gap: undefined,
    rowGap: undefined,
    columnGap: undefined,
    gridTemplateRows:
      dimension === 'rows'
        ? tweakTrackListByInsertingGap(grid.gridTemplateRows, grid.rowGap ?? grid.gap)
        : '1fr',
    gridTemplateColumns:
      dimension === 'columns'
        ? tweakTrackListByInsertingGap(grid.gridTemplateColumns, grid.columnGap ?? grid.gap)
        : '1fr',
    overflow: 'hidden',
  }

  const length = 2 * (dimension === 'rows' ? grid.rows : grid.columns) - 1

  return (
    <div style={style}>
      {createArrayWithLength(length, (index) => {
        const hide = index === 0 || index === length - 1 || index % 2 === 0
        return (
          <GridRowHighlight
            key={index}
            hide={hide} // we only want to show the divs that fall in where the gaps are in the original grid
            gapId={`${dimension}-${index}`}
            onGapHover={NO_OP}
            onHandleHoverEndInner={NO_OP}
            onMouseDown={onMouseDown}
            axis={dimension === 'rows' ? 'row' : 'column'}
            template={getNullableAutoOrTemplateBaseString(
              dimension === 'rows' ? grid.gridTemplateColumns : grid.gridTemplateRows,
            )}
            numberOfHandles={hide ? 0 : dimension === 'rows' ? grid.columns : grid.rows}
            gap={dimension === 'columns' ? grid.rowGap ?? grid.gap : grid.columnGap ?? grid.gap}
            gapValue={cssNumber(1, 'fr')} // FIXME
            beingDragged={beingDragged}
          />
        )
      })}
    </div>
  )
}

const GridRowHighlight = (props: {
  gapId: string
  onMouseDown: React.MouseEventHandler
  onGapHover: () => void
  onHandleHoverEndInner: () => void
  hide: boolean
  axis: 'row' | 'column'
  template: string | undefined
  numberOfHandles: number
  gap: number | null
  gapValue: CSSNumber
  beingDragged: boolean
}) => {
  const {
    gapId,
    onMouseDown,
    onGapHover,
    onHandleHoverEndInner,
    hide,
    axis,
    template,
    gap,
    gapValue,
    numberOfHandles,
    beingDragged,
  } = props

  const colorTheme = useColorTheme()
  const canvasScale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'GridRowHighlight canvasScale',
  )

  const lineWidth = 1 / canvasScale

  const outlineColor = beingDragged ? colorTheme.brandNeonOrange.value : 'transparent'

  return (
    <div
      key={gapId}
      onMouseEnter={onGapHover}
      onMouseLeave={onHandleHoverEndInner}
      data-testid={`gap-control-segment-${gapId}`}
      style={{
        display: 'grid',
        position: 'relative',
        boxShadow: `inset 0 0 0 ${lineWidth}px ${outlineColor}`,
        opacity: hide ? 0 : 1,

        alignItems: 'center',
        justifyContent: 'center',
        placeItems: 'center',

        gap: gap ?? 0,
        gridTemplateColumns: axis === 'row' ? template : '1fr',
        gridTemplateRows: axis === 'column' ? template : '1fr',
      }}
    >
      {createArrayWithLength(numberOfHandles, (i) => (
        <GridGapHandle
          gapId={gapId}
          index={i}
          scale={canvasScale}
          gapValue={gapValue}
          axis={axis}
          onMouseDown={onMouseDown}
          isDragging={beingDragged}
          onHandleHoverStartInner={NO_OP}
          indicatorShown={null}
          elementHovered={true}
          gapIsHovered={true}
          backgroundShown={true}
        />
      ))}
    </div>
  )
}

function tweakTrackListByInsertingGap(
  trackList: GridAutoOrTemplateBase | null,
  gap: number | null,
): string | undefined {
  if (trackList == null) {
    return undefined
  }

  if (trackList.type === 'FALLBACK') {
    throw new Error('Cannot insert gap into fallback')
  }

  if (gap == null) {
    return printGridAutoOrTemplateBase(trackList)
  }

  return interleaveArray(trackList.dimensions.map(stringifyGridDimension), `${gap}px`).join(' ')
}
