import type { CSSProperties } from 'react'
import React from 'react'
import { createArrayWithLength, interleaveArray } from '../../../../core/shared/array-utils'
import type { GridAutoOrTemplateBase } from '../../../../core/shared/element-template'
import type { Size } from '../../../../core/shared/math-utils'
import { size } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { when } from '../../../../utils/react-conditionals'
import type { UtopiColor } from '../../../../uuiui'
import { useColorTheme, UtopiaStyles } from '../../../../uuiui'
import { CSSCursor } from '../../../../uuiui-deps'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import {
  cssNumber,
  printCSSNumber,
  stringifyGridDimension,
} from '../../../inspector/common/css-utils'
import type { Axis } from '../../gap-utils'
import { maybeGridGapData } from '../../gap-utils'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import type { GridData } from '../grid-controls-for-strategies'
import { getNullableAutoOrTemplateBaseString, useGridData } from '../grid-controls-for-strategies'
import { getGridHelperStyleMatchingTargetGrid } from '../grid-controls-helpers'
import type { CSSNumberWithRenderedValue } from './controls-common'
import { CanvasLabel, PillHandle, useHoverWithDelay } from './controls-common'
import { startGapControlInteraction } from './grid-gap-control-helpers'
import type { AlignContent, FlexJustifyContent } from '../../../inspector/inspector-common'

export interface GridGapControlProps {
  selectedElement: ElementPath
  updatedGapValueRow: CSSNumberWithRenderedValue | null
  updatedGapValueColumn: CSSNumberWithRenderedValue | null
}

export const GridGapControlTestId = 'grid-gap-control'
export const GridGapControlHandleTestId = 'grid-gap-control-handle'
// background delay when hovering the gap
export const GridGapBackgroundHoverDelay = 1500
// background delay when hovering the handle itself
const GapHandleBackgroundHoverDelay = 750
// px threshold for showing the gap handles even without hovering the gap itself
// (for narrow gaps)
const GapHandleGapWidthThreshold = 10

const DefaultGapControlSizeConstants: GapControlSizeConstants = {
  borderWidth: 1,
  paddingIndicatorOffset: 10,
  hitAreaPadding: 5,
}

export const GridGapControlComponent = React.memo<GridGapControlProps>((props) => {
  const { selectedElement } = props

  const dispatch = useDispatch()
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'GridGapControlComponent scale',
  )

  const elementHovered =
    useEditorState(
      Substores.highlightedHoveredViews,
      (store) => store.editor.hoveredViews.includes(selectedElement),
      'GridGapControlComponent elementHovered',
    ) ?? false

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
      startGapControlInteraction(e, dispatch, canvasOffset.current, scale, axis)
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

  const [hoveredAxis, setHoveredAxis] = React.useState<'row' | 'column'>('row')
  const onMouseOverRow = React.useCallback(() => setHoveredAxis('row'), [])
  const onMouseOverColumn = React.useCallback(() => setHoveredAxis('column'), [])

  const gridGap = useEditorState(
    Substores.metadata,
    (store) => maybeGridGapData(store.editor.jsxMetadata, selectedElement),
    'GridGapControlComponent gridGap',
  )

  if (grid == null || gridGap == null) {
    return null
  }

  return (
    <CanvasOffsetWrapper>
      <GridPaddingOutlineForDimension
        grid={grid}
        dimension={'rows'}
        onMouseDown={rowMouseDownHandler}
        beingDragged={activeDraggingAxis === 'row'}
        onMouseOver={onMouseOverRow}
        zIndexPriority={hoveredAxis === 'row' ? true : false}
        gridGap={gridGap.row}
        elementHovered={elementHovered}
      />
      <GridPaddingOutlineForDimension
        grid={grid}
        dimension={'columns'}
        onMouseDown={columnMouseDownHandler}
        beingDragged={activeDraggingAxis === 'column'}
        onMouseOver={onMouseOverColumn}
        zIndexPriority={hoveredAxis === 'column' ? true : false}
        gridGap={gridGap.column}
        elementHovered={elementHovered}
      />
    </CanvasOffsetWrapper>
  )
})

export const GridPaddingOutlineForDimension = (props: {
  grid: GridData
  dimension: 'rows' | 'columns'
  onMouseDown: (e: React.MouseEvent<HTMLDivElement>) => void
  beingDragged: boolean
  onMouseOver: () => void
  zIndexPriority: boolean
  gridGap: CSSNumberWithRenderedValue
  elementHovered: boolean
  draggedOutlineColor?: UtopiColor
}) => {
  const {
    grid,
    gridGap,
    dimension,
    onMouseDown,
    beingDragged,
    onMouseOver,
    zIndexPriority,
    elementHovered,
    draggedOutlineColor,
  } = props

  let style: CSSProperties = {
    ...getGridHelperStyleMatchingTargetGrid(grid),
    zIndex: zIndexPriority ? 1 : undefined,
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
          <GridRowOrColumnHighlight
            key={index}
            hide={hide} // we only want to show the divs that fall in where the gaps are in the original grid
            gapId={`${dimension}-${index}`}
            onMouseDown={onMouseDown}
            axis={dimension === 'rows' ? 'row' : 'column'}
            template={getNullableAutoOrTemplateBaseString(
              dimension === 'rows' ? grid.gridTemplateColumns : grid.gridTemplateRows,
            )}
            numberOfHandles={hide ? 0 : dimension === 'rows' ? grid.columns : grid.rows}
            gap={dimension === 'columns' ? grid.rowGap ?? grid.gap : grid.columnGap ?? grid.gap}
            gapValue={gridGap}
            beingDragged={beingDragged}
            onMouseOver={onMouseOver}
            elementHovered={elementHovered}
            gridJustifyContent={grid.justifyContent}
            gridAlignContent={grid.alignContent}
            draggedOutlineColor={draggedOutlineColor}
          />
        )
      })}
    </div>
  )
}

const GridRowOrColumnHighlight = (props: {
  gapId: string
  onMouseDown: React.MouseEventHandler
  hide: boolean
  axis: 'row' | 'column'
  template: string | undefined
  numberOfHandles: number
  gap: number | null
  gapValue: CSSNumberWithRenderedValue | null
  beingDragged: boolean
  onMouseOver: () => void
  elementHovered: boolean
  gridJustifyContent: FlexJustifyContent | null
  gridAlignContent: AlignContent | null
  draggedOutlineColor?: UtopiColor
}) => {
  const {
    gapId,
    onMouseDown,
    hide,
    axis,
    template,
    gap,
    gapValue,
    numberOfHandles,
    beingDragged,
    onMouseOver,
    elementHovered,
    gridJustifyContent,
    gridAlignContent,
    draggedOutlineColor,
  } = props

  const colorTheme = useColorTheme()
  const canvasScale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'GridRowHighlight canvasScale',
  )

  const lineWidth = 1 / canvasScale

  const outlineColor = beingDragged
    ? (draggedOutlineColor ?? colorTheme.brandNeonOrange).value
    : 'transparent'

  const [backgroundShown, setBackgroundShown] = React.useState<boolean>(false)

  const [gapIsHovered, setGapIsHovered] = React.useState(false)
  const [handleIsHovered, setHandleIsHovered] = React.useState<number | null>(null)
  const [hoverStart, hoverEnd] = useHoverWithDelay(GridGapBackgroundHoverDelay, setBackgroundShown)

  const onGapHover = React.useCallback(
    (e: React.MouseEvent) => {
      onMouseOver()
      setGapIsHovered(true)
    },
    [onMouseOver],
  )

  const onHandleHover = React.useCallback(
    (e: React.MouseEvent, index: number) => {
      hoverStart(e)
      setHandleIsHovered(index)
    },
    [hoverStart],
  )

  const onGapHoverEnd = React.useCallback(
    (e: React.MouseEvent) => {
      hoverEnd(e)
      setGapIsHovered(false)
      setHandleIsHovered(null)
    },
    [hoverEnd],
  )

  const shouldShowBackground = !beingDragged && backgroundShown

  if (gapValue == null) {
    return null
  }

  return (
    <div
      key={gapId}
      onMouseEnter={onGapHover}
      onMouseLeave={onGapHoverEnd}
      data-testid={`gap-control-segment-${gapId}`}
      style={{
        pointerEvents: hide ? undefined : 'all',
        display: 'grid',
        position: 'relative',
        boxShadow: `inset 0 0 0 ${lineWidth}px ${outlineColor}`,
        opacity: hide ? 0 : 1,

        alignContent: axis === 'row' ? undefined : gridAlignContent ?? undefined,
        justifyContent: axis === 'row' ? gridJustifyContent ?? undefined : undefined,
        placeItems: 'center',

        gap: gap ?? 0,
        gridTemplateColumns: axis === 'row' ? template : '1fr',
        gridTemplateRows: axis === 'column' ? template : '1fr',

        ...(shouldShowBackground
          ? UtopiaStyles.backgrounds.stripedBackground(
              colorTheme.brandNeonOrange.value,
              canvasScale,
            )
          : {}),
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
          onHandleHoverStartInner={onHandleHover}
          indicatorShown={handleIsHovered}
          elementHovered={elementHovered}
          gapIsHovered={gapIsHovered}
          backgroundShown={backgroundShown}
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

  const gapTrack = gap == null ? `0px` : `${gap}px`

  return interleaveArray(trackList.dimensions.map(stringifyGridDimension), gapTrack).join(' ')
}

interface GapControlSizeConstants {
  paddingIndicatorOffset: number
  hitAreaPadding: number
  borderWidth: number
}

const gapControlSizeConstants = (
  constants: GapControlSizeConstants,
  scale: number,
): GapControlSizeConstants => ({
  borderWidth: constants.borderWidth / scale,
  paddingIndicatorOffset: constants.paddingIndicatorOffset / scale,
  hitAreaPadding: constants.hitAreaPadding / scale,
})

type GridGapHandleProps = {
  gapId: string
  index: number
  scale: number
  gapValue: CSSNumberWithRenderedValue
  axis: Axis
  onMouseDown: React.MouseEventHandler
  isDragging: boolean
  onHandleHoverStartInner: (e: React.MouseEvent, index: number) => void
  indicatorShown: number | null
  elementHovered: boolean
  gapIsHovered: boolean
  backgroundShown: boolean
}
export function GridGapHandle({
  gapId,
  index,
  scale,
  gapValue,
  axis,
  onMouseDown,
  onHandleHoverStartInner,
  isDragging,
  indicatorShown,
  elementHovered,
  gapIsHovered,
  backgroundShown,
}: GridGapHandleProps) {
  const { width, height } = handleDimensions(axis, scale)
  const { hitAreaPadding, paddingIndicatorOffset, borderWidth } = gapControlSizeConstants(
    DefaultGapControlSizeConstants,
    scale,
  )
  const colorTheme = useColorTheme()
  const shouldShowIndicator = !isDragging && indicatorShown === index
  let shouldShowHandle = !isDragging && gapIsHovered
  // show the handle also if the gap is too narrow to hover
  if (!gapIsHovered && !backgroundShown) {
    shouldShowHandle = elementHovered && gapValue.renderedValuePx <= GapHandleGapWidthThreshold
  }
  const handleOpacity = gapIsHovered ? 1 : 0.3

  const onHandleHoverStart = React.useCallback(
    (e: React.MouseEvent) => {
      onHandleHoverStartInner(e, index)
    },
    [onHandleHoverStartInner, index],
  )

  const rowGapStyles =
    axis === 'row'
      ? ({
          left: '50%',
          top: '50%',
          transform: 'translate(-50%, -50%)',
          position: 'absolute',
          gridArea: `1/${index + 1}/2/${index + 2}`,
        } as const)
      : {}
  return (
    <div
      data-testid={`${GridGapControlHandleTestId}-${gapId}`}
      style={{
        visibility: shouldShowHandle ? 'visible' : 'hidden',
        pointerEvents: 'all',
        padding: hitAreaPadding,
        cursor: axis === 'row' ? CSSCursor.GapNS : CSSCursor.GapEW,
        opacity: handleOpacity,
        ...rowGapStyles,
      }}
      onMouseDown={onMouseDown}
      onMouseEnter={onHandleHoverStart}
    >
      <div
        style={{
          position: 'absolute',
          paddingTop: paddingIndicatorOffset,
          paddingLeft: paddingIndicatorOffset,
          pointerEvents: 'none',
        }}
      >
        {when(
          shouldShowIndicator,
          <CanvasLabel
            value={printCSSNumber(
              gapValue.value ?? cssNumber(gapValue.renderedValuePx, 'px'),
              null,
            )}
            scale={scale}
            color={colorTheme.brandNeonOrange.value}
            textColor={colorTheme.white.value}
          />,
        )}
      </div>
      <PillHandle
        width={width}
        height={height}
        pillColor={colorTheme.brandNeonOrange.value}
        borderWidth={borderWidth}
      />
    </div>
  )
}

function handleDimensions(axis: Axis, scale: number): Size {
  if (axis === 'row') {
    return size(12 / scale, 4 / scale)
  }
  if (axis === 'column') {
    return size(4 / scale, 12 / scale)
  }
  assertNever(axis)
}
