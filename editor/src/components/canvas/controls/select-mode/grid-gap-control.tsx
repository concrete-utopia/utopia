import React, { useState } from 'react'
import type { CanvasRectangle, CanvasVector, Size } from '../../../../core/shared/math-utils'
import { size, windowPoint } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { Modifier } from '../../../../utils/modifiers'
import { when } from '../../../../utils/react-conditionals'
import { useColorTheme, UtopiaStyles } from '../../../../uuiui'
import type { EditorDispatch } from '../../../editor/action-types'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import type { CSSNumber } from '../../../inspector/common/css-utils'
import { printCSSNumber } from '../../../inspector/common/css-utils'
import CanvasActions from '../../canvas-actions'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { createInteractionViaMouse, gridGapHandle } from '../../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import type { Axis } from '../../gap-utils'
import { maybeGridGapData, gridGapControlBoundsFromMetadata } from '../../gap-utils'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import type { CSSNumberWithRenderedValue } from './controls-common'
import { CanvasLabel, fallbackEmptyValue, PillHandle, useHoverWithDelay } from './controls-common'
import { CSSCursor } from '../../../../uuiui-deps'
import { useBoundingBox } from '../bounding-box-hooks'
import { isZeroSizedElement } from '../outline-utils'
import { createArrayWithLength } from '../../../../core/shared/array-utils'
import { useGridData } from '../grid-controls'

interface GridGapControlProps {
  selectedElement: ElementPath
  updatedGapValueRow: CSSNumberWithRenderedValue | null
  updatedGapValueColumn: CSSNumberWithRenderedValue | null
}

export const GridGapControlTestId = 'grid-gap-control'
export const GridGapControlHandleTestId = 'grid-gap-control-handle'

export const GridGapControl = controlForStrategyMemoized<GridGapControlProps>((props) => {
  const { selectedElement, updatedGapValueRow, updatedGapValueColumn } = props
  const colorTheme = useColorTheme()
  const accentColor = colorTheme.gapControlsBg.value

  const hoveredViews = useEditorState(
    Substores.highlightedHoveredViews,
    (store) => store.editor.hoveredViews,
    'GridGapControl hoveredViews',
  )

  const [elementHovered, setElementHovered] = useState<boolean>(false)

  const [rowBackgroundShown, setRowBackgroundShown] = React.useState<boolean>(false)
  const [columnBackgroundShown, setColumnBackgroundShown] = React.useState<boolean>(false)

  const [rowControlHoverStart, rowControlHoverEnd] = useHoverWithDelay(0, setRowBackgroundShown)
  const [columnControlHoverStart, columnControlHoverEnd] = useHoverWithDelay(
    0,
    setColumnBackgroundShown,
  )

  const timeoutRef = React.useRef<NodeJS.Timeout | null>(null)
  React.useEffect(() => {
    const timeoutHandle = timeoutRef.current
    if (timeoutHandle != null) {
      clearTimeout(timeoutHandle)
    }

    if (hoveredViews.includes(selectedElement)) {
      timeoutRef.current = setTimeout(() => setElementHovered(true), 200)
    } else {
      setElementHovered(false)
    }
  }, [hoveredViews, selectedElement])

  const dispatch = useDispatch()
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'GridGapControl scale',
  )
  const metadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'GridGapControl metadata',
  )

  const isDragging = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.interactionSession?.activeControl.type === 'GRID_GAP_HANDLE',
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

  const gridGap = maybeGridGapData(metadata, selectedElement)
  if (gridGap == null) {
    return null
  }

  const controlRef = useBoundingBox(
    [selectedElement],
    (ref, safeGappedBoundingBox, realBoundingBox) => {
      if (isZeroSizedElement(realBoundingBox)) {
        ref.current.style.display = 'none'
      } else {
        ref.current.style.display = 'block'
        ref.current.style.left = safeGappedBoundingBox.x + 'px'
        ref.current.style.top = safeGappedBoundingBox.y + 'px'
        ref.current.style.width = safeGappedBoundingBox.width + 'px'
        ref.current.style.height = safeGappedBoundingBox.height + 'px'
      }
    },
  )

  const gridGapRow = updatedGapValueRow ?? gridGap.row
  const gridGapColumn = updatedGapValueColumn ?? gridGap.column

  const gridRowColumnInfo = useGridData([selectedElement])

  const controlBounds = gridGapControlBoundsFromMetadata(
    selectedElement,
    gridRowColumnInfo[0],
    {
      row: fallbackEmptyValue(gridGapRow),
      column: fallbackEmptyValue(gridGapColumn),
    },
    scale,
  )

  return (
    <CanvasOffsetWrapper>
      <div
        data-testid={GridGapControlTestId}
        style={{ pointerEvents: 'none', position: 'absolute' }}
        ref={controlRef}
      >
        {controlBounds.gaps.map(({ gap, bounds, axis, gapId }) => {
          const gapControlProps = {
            mouseDownHandler: axisMouseDownHandler,
            gapId: gapId,
            bounds: bounds,
            accentColor: accentColor,
            scale: scale,
            isDragging: isDragging,
            axis: axis,
            gapValue: gap,
            internalGrid: {
              gridTemplateRows: controlBounds.gridTemplateRows,
              gridTemplateColumns: controlBounds.gridTemplateColumns,
              gap: axis === 'row' ? controlBounds.gapValues.column : controlBounds.gapValues.row,
            },
            elementHovered: elementHovered,
            handles: axis === 'row' ? controlBounds.columns : controlBounds.rows,
          }
          if (axis === 'row') {
            return (
              <GapControlSegment
                {...gapControlProps}
                key={gapId}
                onMouseDown={rowMouseDownHandler}
                hoverStart={rowControlHoverStart}
                hoverEnd={rowControlHoverEnd}
                backgroundShown={rowBackgroundShown}
              />
            )
          }
          return (
            <GapControlSegment
              {...gapControlProps}
              key={gapId}
              onMouseDown={columnMouseDownHandler}
              hoverStart={columnControlHoverStart}
              hoverEnd={columnControlHoverEnd}
              backgroundShown={columnBackgroundShown}
            />
          )
        })}
      </div>
    </CanvasOffsetWrapper>
  )
})

interface GapControlSizeConstants {
  dragBorderWidth: number
  paddingIndicatorOffset: number
  hitAreaPadding: number
  borderWidth: number
}

const DefaultGapControlSizeConstants: GapControlSizeConstants = {
  dragBorderWidth: 1,
  borderWidth: 1,
  paddingIndicatorOffset: 10,
  hitAreaPadding: 5,
}

const gapControlSizeConstants = (
  constants: GapControlSizeConstants,
  scale: number,
): GapControlSizeConstants => ({
  dragBorderWidth: constants.dragBorderWidth / scale,
  borderWidth: constants.borderWidth / scale,
  paddingIndicatorOffset: constants.paddingIndicatorOffset / scale,
  hitAreaPadding: constants.hitAreaPadding / scale,
})

interface GridGapControlSegmentProps {
  onMouseDown: React.MouseEventHandler
  hoverStart: React.MouseEventHandler
  hoverEnd: React.MouseEventHandler
  bounds: CanvasRectangle
  axis: Axis
  gapValue: CSSNumber
  elementHovered: boolean
  gapId: string
  accentColor: string
  scale: number
  isDragging: boolean
  backgroundShown: boolean
  handles: number
  internalGrid: {
    gridTemplateRows: string
    gridTemplateColumns: string
    gap: CSSNumber
  }
}

const GapControlSegment = React.memo<GridGapControlSegmentProps>((props) => {
  const {
    hoverStart,
    hoverEnd,
    bounds,
    isDragging,
    accentColor: accentColor,
    scale,
    gapId,
    backgroundShown,
    axis,
    handles,
    internalGrid,
  } = props

  const [indicatorShown, setIndicatorShown] = React.useState<number | null>(null)

  const { dragBorderWidth } = gapControlSizeConstants(DefaultGapControlSizeConstants, scale)

  const handleHoverStartInner = React.useCallback((indicatorIndex: number) => {
    setIndicatorShown(indicatorIndex)
  }, [])

  const handleHoverEndInner = React.useCallback(
    (e: React.MouseEvent) => {
      hoverEnd(e)
      setIndicatorShown(null)
    },
    [hoverEnd],
  )

  const shouldShowBackground = !isDragging && backgroundShown

  // Invert the direction for the handle.
  const segmentFlexDirection = axis === 'row' ? 'column' : 'row'

  return (
    <div
      key={gapId}
      onMouseEnter={hoverStart}
      onMouseLeave={handleHoverEndInner}
      data-testid={`gap-control-segment-${gapId}`}
      style={{
        pointerEvents: 'all',
        position: 'absolute',
        left: bounds.x,
        top: bounds.y,
        width: bounds.width,
        height: bounds.height,
        display: 'flex',
        flexDirection: segmentFlexDirection,
        border: isDragging ? `${dragBorderWidth}px solid ${accentColor}` : undefined,
        ...(shouldShowBackground
          ? UtopiaStyles.backgrounds.stripedBackground(accentColor, scale)
          : {}),
      }}
    >
      <div
        style={{
          width: bounds.width,
          height: bounds.height,
          display: 'grid',
          alignItems: 'center',
          justifyContent: 'center',
          placeItems: 'center',
          gap: internalGrid.gap.value,
          gridTemplateColumns: axis === 'row' ? internalGrid.gridTemplateColumns : '1fr',
          gridTemplateRows: axis === 'column' ? internalGrid.gridTemplateRows : '1fr',
          position: 'relative',
        }}
      >
        {createArrayWithLength(handles, (i) => (
          <GridGapHandler
            key={i}
            index={i}
            {...props}
            handleHoverStartInner={handleHoverStartInner}
            indicatorShown={indicatorShown}
          />
        ))}
      </div>
    </div>
  )
})

type GridGapHandlerProps = {
  gapId: string
  index: number
  scale: number
  gapValue: CSSNumber
  axis: Axis
  onMouseDown: React.MouseEventHandler
  isDragging: boolean
  handleHoverStartInner: (index: number) => void
  indicatorShown: number | null
  elementHovered: boolean
}
function GridGapHandler({
  gapId,
  index,
  scale,
  gapValue,
  axis,
  onMouseDown,
  handleHoverStartInner,
  isDragging,
  indicatorShown,
  elementHovered,
}: GridGapHandlerProps) {
  const { width, height } = handleDimensions(axis, scale)
  const { hitAreaPadding, paddingIndicatorOffset, borderWidth } = gapControlSizeConstants(
    DefaultGapControlSizeConstants,
    scale,
  )
  const colorTheme = useColorTheme()
  const shouldShowIndicator = !isDragging && indicatorShown === index
  const shouldShowHandle = !isDragging && elementHovered

  const handleHoverStart = React.useCallback(() => {
    handleHoverStartInner(index)
  }, [handleHoverStartInner, index])

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
        padding: hitAreaPadding,
        cursor: axis === 'row' ? CSSCursor.GapNS : CSSCursor.GapEW,
        ...rowGapStyles,
      }}
      onMouseDown={onMouseDown}
      onMouseEnter={handleHoverStart}
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
            value={printCSSNumber(gapValue, null)}
            scale={scale}
            color={colorTheme.brandNeonPink.value}
            textColor={colorTheme.white.value}
          />,
        )}
      </div>
      <PillHandle
        width={width}
        height={height}
        pillColor={colorTheme.brandNeonPink.value}
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
    return size(3 / scale, 12 / scale)
  }
  assertNever(axis)
}

function startInteraction(
  event: React.MouseEvent<HTMLDivElement>,
  dispatch: EditorDispatch,
  canvasOffset: CanvasVector,
  scale: number,
  axis: Axis,
) {
  if (event.buttons === 1 && event.button !== 2) {
    event.stopPropagation()
    const canvasPositions = windowToCanvasCoordinates(
      scale,
      canvasOffset,
      windowPoint({ x: event.nativeEvent.x, y: event.nativeEvent.y }),
    )
    dispatch([
      CanvasActions.createInteractionSession(
        createInteractionViaMouse(
          canvasPositions.canvasPositionRaw,
          Modifier.modifiersForEvent(event),
          gridGapHandle(axis),
          'zero-drag-not-permitted',
        ),
      ),
    ])
  }
}
