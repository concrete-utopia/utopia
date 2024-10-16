/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import type { AnimationControls } from 'framer-motion'
import { motion, useAnimationControls } from 'framer-motion'
import type { CSSProperties } from 'react'
import React from 'react'
import type { Sides } from 'utopia-api/core'
import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls, range, stripNulls, uniqBy } from '../../../core/shared/array-utils'
import { defaultEither } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import type {
  ElementInstanceMetadataMap,
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
import { assertNever, NO_OP } from '../../../core/shared/utils'
import { Modifier } from '../../../utils/modifiers'
import { when } from '../../../utils/react-conditionals'
import { useColorTheme, UtopiaStyles } from '../../../uuiui'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import type { GridDimension, GridDiscreteDimension } from '../../inspector/common/css-utils'
import {
  isCSSKeyword,
  isDynamicGridRepeat,
  isGridCSSRepeat,
  printGridCSSNumber,
} from '../../inspector/common/css-utils'
import CanvasActions from '../canvas-actions'
import type { GridResizeEdge } from '../canvas-strategies/interaction-state'
import {
  createInteractionViaMouse,
  gridAxisHandle,
  gridCellHandle,
  gridResizeHandle,
} from '../canvas-strategies/interaction-state'
import type { GridCellCoordinates } from '../canvas-strategies/strategies/grid-cell-bounds'
import { gridCellTargetId } from '../canvas-strategies/strategies/grid-cell-bounds'
import {
  getGlobalFrameOfGridCell,
  getGridRelatedIndexes,
} from '../canvas-strategies/strategies/grid-helpers'
import { canResizeGridTemplate } from '../canvas-strategies/strategies/resize-grid-strategy'
import { resizeBoundingBoxFromSide } from '../canvas-strategies/strategies/resize-helpers'
import type { EdgePosition } from '../canvas-types'
import { CSSCursor } from '../canvas-types'
import { windowToCanvasCoordinates } from '../dom-lookup'
import type { Axis } from '../gap-utils'
import { useCanvasAnimation } from '../ui-jsx-canvas-renderer/animation-context'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import type {
  GridControlsProps,
  GridData,
  GridMeasurementHelperData,
} from './grid-controls-for-strategies'
import {
  edgePositionToGridResizeEdge,
  getNullableAutoOrTemplateBaseString,
  GridCellTestId,
  GridControlKey,
  gridEdgeToEdgePosition,
  GridMeasurementHelperKey,
  GridMeasurementHelpersKey,
  useGridData,
  useGridMeasurentHelperData,
} from './grid-controls-for-strategies'
import { useMaybeHighlightElement } from './select-mode/select-mode-hooks'
import { useResizeEdges } from './select-mode/use-resize-edges'
import { getGridHelperStyleMatchingTargetGrid } from './grid-controls-helpers'

const CELL_ANIMATION_DURATION = 0.15 // seconds

function getFromPropsOptic(index: number): Optic<GridAutoOrTemplateBase | null, GridDimension> {
  return notNull<GridAutoOrTemplateBase>()
    .compose(fromTypeGuard(isGridAutoOrTemplateDimensions))
    .compose(fromField('dimensions'))
    .compose(fromArrayIndex(index))
}

function gridCSSNumberToLabel(gridCSSNumber: GridDimension): string {
  return printGridCSSNumber(gridCSSNumber)
}

function getLabelForAxis(
  fromDOM: GridDimension,
  index: number,
  fromProps: GridAutoOrTemplateDimensions | null,
): string {
  const fromPropsAtIndex = toFirst(getFromPropsOptic(index), fromProps)
  return gridCSSNumberToLabel(defaultEither(fromDOM, fromPropsAtIndex))
}

const GRID_RESIZE_HANDLE_CONTAINER_SIZE = 30 // px
const GRID_RESIZE_HANDLE_SIZE = 15 // px

interface GridResizingControlProps {
  dimension: GridDimension
  dimensionIndex: number
  axis: Axis
  containingFrame: CanvasRectangle
  fromPropsAxisValues: GridAutoOrTemplateDimensions | null
  padding: number
  resizing: 'resize-target' | 'resize-generated' | 'not-resizing'
  setResizingIndex: (v: number | null) => void
  resizeLocked: boolean
  stripedAreaLength: number | null
}

const GridResizingControl = React.memo((props: GridResizingControlProps) => {
  const { setResizingIndex } = props

  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'GridResizingControl canvasOffset',
  )
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'GridResizingControl scale',
  )
  const dispatch = useDispatch()
  const colorTheme = useColorTheme()

  const mouseDownHandler = React.useCallback(
    (event: React.MouseEvent): void => {
      function mouseUpHandler() {
        setResizingIndex(null)
        window.removeEventListener('mouseup', mouseUpHandler)
      }
      window.addEventListener('mouseup', mouseUpHandler)

      const start = windowToCanvasCoordinates(
        scale,
        canvasOffset,
        windowPoint({ x: event.nativeEvent.x, y: event.nativeEvent.y }),
      )
      setResizingIndex(props.dimensionIndex)

      dispatch([
        CanvasActions.createInteractionSession(
          createInteractionViaMouse(
            start.canvasPositionRounded,
            Modifier.modifiersForEvent(event),
            gridAxisHandle(props.axis, props.dimensionIndex),
            'zero-drag-not-permitted',
          ),
        ),
      ])
      event.stopPropagation()
      event.preventDefault()
    },
    [canvasOffset, dispatch, props.axis, props.dimensionIndex, scale, setResizingIndex],
  )

  const { maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()

  const onMouseMove = React.useCallback(
    (e: React.MouseEvent) => {
      maybeClearHighlightsOnHoverEnd()
      e.stopPropagation()
    },
    [maybeClearHighlightsOnHoverEnd],
  )

  const labelId = `grid-${props.axis}-handle-${props.dimensionIndex}`
  const containerId = `${labelId}-container`

  const shadowSize = React.useMemo(() => {
    return props.axis === 'column'
      ? props.containingFrame.height + GRID_RESIZE_HANDLE_CONTAINER_SIZE
      : props.containingFrame.width + GRID_RESIZE_HANDLE_CONTAINER_SIZE
  }, [props.containingFrame, props.axis])

  const stripedAreaSkew = React.useMemo(
    () => GRID_RESIZE_HANDLE_CONTAINER_SIZE / scale + props.padding,
    [scale, props.padding],
  )

  return (
    <div
      key={containerId}
      data-testid={containerId}
      style={{
        display: 'flex',
        alignItems: props.axis === 'column' ? 'flex-start' : 'center',
        justifyContent: props.axis === 'column' ? 'center' : 'flex-start',
        height: props.axis === 'column' && props.resizing !== 'not-resizing' ? shadowSize : '100%',
        width: props.axis === 'row' && props.resizing !== 'not-resizing' ? shadowSize : '100%',
        position: 'relative',
      }}
    >
      <div
        data-testid={labelId}
        style={{
          zoom: 1 / scale,
          height: GRID_RESIZE_HANDLE_SIZE,
          borderRadius: 3,
          padding: '0 4px',
          border: `.1px solid ${colorTheme.white.value}`,
          background: colorTheme.primary.value,
          color: colorTheme.white.value,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          cursor: gridEdgeToCSSCursor(props.axis === 'column' ? 'column-start' : 'row-start'),
          pointerEvents: 'initial',
        }}
        onMouseDown={mouseDownHandler}
        onMouseMove={onMouseMove}
      >
        {getLabelForAxis(props.dimension, props.dimensionIndex, props.fromPropsAxisValues)}
      </div>
      {when(
        props.resizing !== 'not-resizing',
        <div
          style={{
            position: 'absolute',
            top: props.axis === 'column' ? stripedAreaSkew : 0,
            left: props.axis === 'row' ? stripedAreaSkew : 0,
            right: props.axis === 'row' || props.stripedAreaLength == null ? undefined : 0,
            width:
              props.axis === 'row' && props.stripedAreaLength != null
                ? props.stripedAreaLength
                : undefined,
            bottom: props.axis === 'column' || props.stripedAreaLength == null ? undefined : 0,
            height:
              props.axis === 'column' && props.stripedAreaLength != null
                ? props.stripedAreaLength
                : undefined,
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            border: `1px solid ${
              props.resizeLocked
                ? colorTheme.primary10.value
                : props.resizing === 'resize-target'
                ? colorTheme.primary.value
                : colorTheme.primary50.value
            }`,
            ...(props.resizeLocked
              ? UtopiaStyles.backgrounds.stripedBackground(colorTheme.primary10.value, scale)
              : props.resizing === 'resize-target'
              ? UtopiaStyles.backgrounds.stripedBackground(colorTheme.primary50.value, scale)
              : UtopiaStyles.backgrounds.stripedBackground(colorTheme.primary10.value, scale)),
          }}
        >
          {when(
            props.dimension.areaName != null,
            <div
              style={{
                position: 'absolute',
                color: colorTheme.primary.value,
                background: colorTheme.white.value,
                top: 0,
                left: 0,
                padding: '0 4px',
                borderRadius: '0 0 3px 0',
              }}
            >
              {props.dimension.areaName}
            </div>,
          )}
        </div>,
      )}
    </div>
  )
})
GridResizingControl.displayName = 'GridResizingControl'

interface GridResizingProps {
  axisValues: GridAutoOrTemplateBase | null
  fromPropsAxisValues: GridAutoOrTemplateBase | null
  stripedAreaLength: number | null
  containingFrame: CanvasRectangle
  axis: Axis
  gap: number | null
  padding: Sides | null
  justifyContent: string | null
  alignContent: string | null
}

const GridResizing = React.memo((props: GridResizingProps) => {
  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'GridResizing canvasScale',
  )

  const fromProps = React.useMemo((): GridAutoOrTemplateDimensions | null => {
    if (props.fromPropsAxisValues?.type !== 'DIMENSIONS') {
      return null
    }
    if (!canResizeGridTemplate(props.fromPropsAxisValues)) {
      return null
    }
    return {
      type: 'DIMENSIONS',
      dimensions: props.fromPropsAxisValues.dimensions.reduce(
        (acc, cur): GridDiscreteDimension[] => {
          if (isGridCSSRepeat(cur)) {
            if (isDynamicGridRepeat(cur)) {
              return acc
            }
            let expanded: GridDiscreteDimension[] = []
            for (let i = 0; i < cur.times; i++) {
              expanded.push(...cur.value.filter((v) => v.type !== 'REPEAT'))
            }
            return acc.concat(...expanded)
          } else {
            return acc.concat(cur)
          }
        },
        [] as GridDiscreteDimension[],
      ),
    }
  }, [props.fromPropsAxisValues])

  const resizeLocked = React.useMemo(() => {
    return fromProps == null || !canResizeGridTemplate(fromProps)
  }, [fromProps])

  const [resizingIndex, setResizingIndex] = React.useState<number | null>(null)

  // These are the indexes of the elements that will resize too alongside the one at the index of
  // `resizingIndex`.
  const coresizingIndexes: number[] = React.useMemo(() => {
    if (props.fromPropsAxisValues?.type !== 'DIMENSIONS' || resizingIndex == null) {
      return []
    }
    return getGridRelatedIndexes({
      template: props.fromPropsAxisValues.dimensions,
      index: resizingIndex,
    })
  }, [props.fromPropsAxisValues, resizingIndex])

  if (props.axisValues == null) {
    return null
  }
  switch (props.axisValues.type) {
    case 'DIMENSIONS':
      const size = GRID_RESIZE_HANDLE_CONTAINER_SIZE / canvasScale
      const dimensions = props.axisValues.dimensions

      return (
        <div
          style={{
            position: 'absolute',
            top: props.containingFrame.y - (props.axis === 'column' ? size : 0),
            left: props.containingFrame.x - (props.axis === 'row' ? size : 0),
            width: props.axis === 'column' ? props.containingFrame.width : size,
            height: props.axis === 'row' ? props.containingFrame.height : size,
            display: 'grid',
            gridTemplateColumns:
              props.axis === 'column'
                ? dimensions.map((dim) => printGridCSSNumber(dim)).join(' ')
                : undefined,
            gridTemplateRows:
              props.axis === 'row'
                ? dimensions.map((dim) => printGridCSSNumber(dim)).join(' ')
                : undefined,
            gap: props.gap ?? 0,
            paddingLeft:
              props.axis === 'column' && props.padding != null
                ? `${props.padding.left}px`
                : undefined,
            paddingTop:
              props.axis === 'row' && props.padding != null ? `${props.padding.top}px` : undefined,
            paddingRight:
              props.axis === 'column' && props.padding != null
                ? `${props.padding.right}px`
                : undefined,
            paddingBottom:
              props.axis === 'row' && props.padding != null
                ? `${props.padding.bottom}px`
                : undefined,
            justifyContent: props.justifyContent ?? undefined,
            alignContent: props.alignContent ?? undefined,
          }}
        >
          {dimensions.flatMap((dimension, dimensionIndex) => {
            return (
              <GridResizingControl
                key={`grid-resizing-control-${dimensionIndex}`}
                dimensionIndex={dimensionIndex}
                dimension={dimension}
                fromPropsAxisValues={fromProps}
                stripedAreaLength={props.stripedAreaLength}
                axis={props.axis}
                containingFrame={props.containingFrame}
                resizing={
                  resizingIndex === dimensionIndex
                    ? 'resize-target'
                    : coresizingIndexes.includes(dimensionIndex)
                    ? 'resize-generated'
                    : 'not-resizing'
                }
                resizeLocked={resizeLocked}
                setResizingIndex={setResizingIndex}
                padding={
                  props.padding == null
                    ? 0
                    : props.axis === 'column'
                    ? props.padding.top ?? 0
                    : props.padding.left ?? 0
                }
              />
            )
          })}
        </div>
      )
    case 'FALLBACK':
      return null
    default:
      assertNever(props.axisValues)
  }
})
GridResizing.displayName = 'GridResizing'

interface GridRowColumnResizingControlsProps {
  target: ElementPath
}

export const GridRowColumnResizingControlsComponent = ({
  target,
}: GridRowColumnResizingControlsProps) => {
  const grids = useGridData([target])

  function getStripedAreaLength(template: GridAutoOrTemplateBase | null, gap: number) {
    if (template?.type !== 'DIMENSIONS') {
      return null
    }
    return template.dimensions.reduce((acc, curr, index) => {
      if (curr.type === 'NUMBER') {
        return acc + curr.value.value + (index > 0 ? gap : 0)
      }
      return acc
    }, 0)
  }

  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'GridRowColumnResizingControls scale',
  )

  const gridsWithVisibleResizeControls = React.useMemo(() => {
    return grids.filter((grid) => {
      if (
        grid.gridTemplateColumns?.type !== 'DIMENSIONS' ||
        grid.gridTemplateRows?.type !== 'DIMENSIONS'
      ) {
        return false
      }

      // returns whether the rendered dimensions are too crowded, as in there are two cols/rows that are closer than the handle sizes
      function tooCrowded(dimensions: GridDimension[]): boolean {
        const visualSizes = dimensions.map(
          (dim) => (dim.type === 'NUMBER' ? dim.value.value : 0) * scale,
        )
        return visualSizes.some((dim, index) => {
          if (index < visualSizes.length - 1) {
            const next = visualSizes[index + 1]
            if (dim + next < GRID_RESIZE_HANDLE_SIZE * 2) {
              return true
            }
          }
          return false
        })
      }

      return (
        !tooCrowded(grid.gridTemplateColumns.dimensions) &&
        !tooCrowded(grid.gridTemplateRows.dimensions)
      )
    })
  }, [scale, grids])

  return (
    <CanvasOffsetWrapper>
      {gridsWithVisibleResizeControls.flatMap((grid) => {
        return (
          <GridResizing
            key={`grid-resizing-column-${EP.toString(grid.elementPath)}`}
            axisValues={grid.gridTemplateColumns}
            fromPropsAxisValues={grid.gridTemplateColumnsFromProps}
            containingFrame={grid.frame}
            axis={'column'}
            gap={grid.columnGap ?? grid.gap}
            padding={grid.padding}
            stripedAreaLength={getStripedAreaLength(grid.gridTemplateRows, grid.gap ?? 0)}
            alignContent={grid.justifyContent}
            justifyContent={grid.alignContent}
          />
        )
      })}
      {gridsWithVisibleResizeControls.flatMap((grid) => {
        return (
          <GridResizing
            key={`grid-resizing-row-${EP.toString(grid.elementPath)}`}
            axisValues={grid.gridTemplateRows}
            fromPropsAxisValues={grid.gridTemplateRowsFromProps}
            containingFrame={grid.frame}
            axis={'row'}
            gap={grid.rowGap ?? grid.gap}
            padding={grid.padding}
            stripedAreaLength={getStripedAreaLength(grid.gridTemplateColumns, grid.gap ?? 0)}
            alignContent={grid.alignContent}
            justifyContent={grid.justifyContent}
          />
        )
      })}
    </CanvasOffsetWrapper>
  )
}

interface GridControlProps {
  grid: GridData
  controlsVisible: 'visible' | 'not-visible'
}

const GridControl = React.memo<GridControlProps>(({ grid, controlsVisible }) => {
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
        ? EP.toUid(store.editor.canvas.interactionSession.activeControl.path)
        : null,
    'GridControl activelyDraggingOrResizingCell',
  )

  const currentHoveredCell = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.controls.gridControlData?.targetCell ?? null,
    'GridControl currentHoveredCell',
  )

  const currentHoveredGrid = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.controls.gridControlData?.grid ?? null,
    'GridControl currentHoveredGrid',
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
    (params: { path: ElementPath; row: number; column: number; frame: CanvasRectangle }) =>
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
              gridCellHandle({ path: params.path }),
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
        ? store.editor.canvas.interactionSession.activeControl.path
        : null,
    'GridControl dragging',
  )

  const shadow = React.useMemo(() => {
    return (
      cells.find(
        (cell) => EP.toUid(cell.elementPath) === (dragging == null ? null : EP.toUid(dragging)),
      ) ?? null
    )
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

  const placeholders = controlsVisible === 'visible' ? range(0, grid.cells) : []
  const baseStyle = getGridHelperStyleMatchingTargetGrid(grid)
  const style = {
    ...baseStyle,
    backgroundColor:
      activelyDraggingOrResizingCell == null || controlsVisible === 'not-visible'
        ? 'transparent'
        : colorTheme.primary10.value,
    outline: `1px solid ${
      activelyDraggingOrResizingCell == null || controlsVisible === 'not-visible'
        ? 'transparent'
        : colorTheme.primary.value
    }`,
  }

  return (
    <React.Fragment>
      {/* grid lines */}
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

          const isActiveGrid =
            (dragging != null && EP.isParentOf(grid.elementPath, dragging)) ||
            (currentHoveredGrid != null && EP.pathsEqual(grid.elementPath, currentHoveredGrid))
          const isActiveCell =
            isActiveGrid &&
            countedColumn === currentHoveredCell?.column &&
            countedRow === currentHoveredCell?.row

          const activePositioningTarget = isActiveCell && targetsAreCellsWithPositioning

          const borderColor = activePositioningTarget
            ? colorTheme.brandNeonPink.value
            : colorTheme.grey65.value
          return (
            <div
              key={id}
              id={id}
              data-testid={id}
              style={{
                position: 'relative',
                zIndex: activePositioningTarget ? 1 : undefined,
              }}
              data-grid-row={countedRow}
              data-grid-column={countedColumn}
            >
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
                  borderBottom: gridPlaceholderBorder(borderColor, scale),
                  borderRight: gridPlaceholderBorder(borderColor, scale),
                }}
              />
            </div>
          )
        })}
      </div>
      {/* cell targets */}
      {cells.map((cell) => {
        return (
          <div
            onMouseDown={startInteractionWithUid({
              path: cell.elementPath,
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
                EP.toUid(cell.elementPath) !== activelyDraggingOrResizingCell &&
                controlsVisible === 'visible'
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
      hoveringStart != null &&
      controlsVisible === 'visible' ? (
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
    </React.Fragment>
  )
})
GridControl.displayName = 'GridControl'

export const GridMeasurementHelpers = React.memo(() => {
  const metadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'GridMeasurementHelpers metadata',
  )

  const grids = useAllGrids(metadata)

  return (
    <CanvasOffsetWrapper>
      {grids.map((grid) => {
        return <GridMeasurementHelper key={GridMeasurementHelpersKey(grid)} elementPath={grid} />
      })}
    </CanvasOffsetWrapper>
  )
})
GridMeasurementHelpers.displayName = 'GridMeasurementHelpers'

export interface GridMeasurementHelperProps {
  elementPath: ElementPath
}

const GridMeasurementHelper = React.memo<{ elementPath: ElementPath }>(({ elementPath }) => {
  const gridData = useGridMeasurentHelperData(elementPath)

  if (gridData == null) {
    return null
  }

  const placeholders = range(0, gridData.cells)

  const style: CSSProperties = {
    ...getGridHelperStyleMatchingTargetGrid(gridData),
    opacity: 1,
  }

  return (
    <div
      id={GridMeasurementHelperKey(elementPath)}
      data-grid-path={EP.toString(elementPath)}
      style={style}
    >
      {placeholders.map((cell) => {
        const countedRow = Math.floor(cell / gridData.columns) + 1
        const countedColumn = Math.floor(cell % gridData.columns) + 1
        const id = `${GridMeasurementHelperKey(elementPath)}-${countedRow}-${countedColumn}`
        return (
          <div
            key={id}
            style={{
              position: 'relative',
              pointerEvents: 'none',
            }}
            data-grid-row={countedRow}
            data-grid-column={countedColumn}
          />
        )
      })}
    </div>
  )
})
GridMeasurementHelper.displayName = 'GridMeasurementHelper'

export const GridControlsComponent = ({ targets }: GridControlsProps) => {
  const ancestorPaths = React.useMemo(() => {
    return targets.flatMap((target) => EP.getAncestors(target))
  }, [targets])
  const ancestorGrids: Array<ElementPath> = useEditorState(
    Substores.metadata,
    (store) => {
      return ancestorPaths.filter((ancestorPath) => {
        const ancestorMetadata = MetadataUtils.findElementByElementPath(
          store.editor.jsxMetadata,
          ancestorPath,
        )
        return MetadataUtils.isGridLayoutedContainer(ancestorMetadata)
      })
    },
    'GridControlsComponent ancestorGrids',
  )

  const targetRootCell = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.controls.gridControlData?.rootCell ?? null,
    'GridControlsComponent targetRootCell',
  )

  const hoveredGrids = useEditorState(
    Substores.canvas,
    (store) => stripNulls([store.editor.canvas.controls.gridControlData?.grid]),
    'GridControlsComponent hoveredGrids',
  )

  const gridsWithVisibleControls: Array<ElementPath> = [...targets, ...hoveredGrids]

  // Uniqify the grid paths, and then sort them by depth.
  // With the lowest depth grid at the end so that it renders on top and catches the events
  // before those above it in the hierarchy.
  const grids = useGridData(
    uniqBy([...gridsWithVisibleControls, ...ancestorGrids], (a, b) => EP.pathsEqual(a, b)).sort(
      (a, b) => {
        return EP.fullDepth(a) - EP.fullDepth(b)
      },
    ),
  )

  if (grids.length === 0) {
    return null
  }

  return (
    <div id={'grid-controls'}>
      <CanvasOffsetWrapper>
        {grids.map((grid) => {
          const shouldHaveVisibleControls = EP.containsPath(
            grid.elementPath,
            gridsWithVisibleControls,
          )
          return (
            <GridControl
              key={GridControlKey(grid.elementPath)}
              grid={grid}
              controlsVisible={shouldHaveVisibleControls ? 'visible' : 'not-visible'}
            />
          )
        })}
        <AbsoluteDistanceIndicators targetRootCell={targetRootCell} />
      </CanvasOffsetWrapper>
    </div>
  )
}

const MIN_INDICATORS_DISTANCE = 32 // px

const AbsoluteDistanceIndicators = React.memo(
  (props: { targetRootCell: GridCellCoordinates | null }) => {
    const colorTheme = useColorTheme()

    const gridMetadata = useEditorState(
      Substores.metadata,
      (store) => {
        if (store.editor.selectedViews.length !== 1) {
          return null
        }

        return MetadataUtils.findElementByElementPath(
          store.editor.jsxMetadata,
          store.editor.selectedViews[0],
        )
      },
      'AbsoluteDistanceIndicators cellFrame',
    )

    const cellFrame = !MetadataUtils.isPositionAbsolute(gridMetadata)
      ? null
      : nullIfInfinity(gridMetadata?.globalFrame)

    const targetCellBoundingBox = React.useMemo(() => {
      if (gridMetadata == null || props.targetRootCell == null) {
        return null
      }
      return getGlobalFrameOfGridCell(gridMetadata, props.targetRootCell)
    }, [props.targetRootCell, gridMetadata])

    const distanceTop =
      targetCellBoundingBox == null || cellFrame == null ? 0 : cellFrame.y - targetCellBoundingBox.y

    const distanceLeft =
      targetCellBoundingBox == null || cellFrame == null ? 0 : cellFrame.x - targetCellBoundingBox.x

    const positioning = React.useMemo(() => {
      if (cellFrame == null || targetCellBoundingBox == null) {
        return null
      }

      function position(
        wantedCoord: 'x' | 'y',
        cell: CanvasRectangle,
        root: CanvasRectangle,
        dominantDistance: number,
        otherDistance: number,
      ): { left: number; top: number } {
        const otherCoord = wantedCoord === 'x' ? 'y' : 'x'
        const dimension = wantedCoord === 'x' ? 'width' : 'height'
        const dominant =
          cell[wantedCoord] < root[wantedCoord] ||
          dominantDistance < MIN_INDICATORS_DISTANCE ||
          otherDistance < 0
            ? root[wantedCoord] + root[dimension] / 2
            : Math.max(root[wantedCoord], cell[wantedCoord])
        const other = otherDistance < 0 ? cell[otherCoord] : root[otherCoord]
        if (wantedCoord === 'x') {
          return {
            left: dominant,
            top: other,
          }
        } else {
          return {
            left: other,
            top: dominant,
          }
        }
      }

      function compensationNegative(
        wantedCoord: 'x' | 'y',
        cell: CanvasRectangle,
        root: CanvasRectangle,
        dist: number,
      ): { width: number; height: number; left: number; top: number } | null {
        const otherCoord = wantedCoord === 'x' ? 'y' : 'x'
        const dimension = wantedCoord === 'x' ? 'width' : 'height'

        const shouldCompensate =
          dist < 0 && cell[wantedCoord] > root[wantedCoord] + root[dimension] / 2
        if (!shouldCompensate) {
          return null
        }

        const size = Math.abs(root[wantedCoord] + root[dimension] / 2 - cell[wantedCoord])
        const dominant = root[wantedCoord] + root[dimension] / 2
        const other = cell[otherCoord]

        return wantedCoord === 'x'
          ? {
              width: size,
              height: 1,
              top: other,
              left: dominant,
            }
          : {
              width: 1,
              height: size,
              top: dominant,
              left: other,
            }
      }

      function compensationPositive(
        wantedCoord: 'x' | 'y',
        cell: CanvasRectangle,
        root: CanvasRectangle,
        dist: number,
      ): { width: number; height: number; left: number; top: number } | null {
        const otherCoord = wantedCoord === 'x' ? 'y' : 'x'
        const dimension = wantedCoord === 'x' ? 'width' : 'height'

        const shouldCompensate = dist > 0 && cell[wantedCoord] > root[wantedCoord] + root[dimension]
        if (!shouldCompensate) {
          return null
        }

        const size = Math.abs(root[wantedCoord] + root[dimension] / 2 - cell[wantedCoord])
        const other = root[otherCoord]
        const dominant = root[wantedCoord] + root[dimension] / 2

        return wantedCoord === 'x'
          ? {
              width: size,
              height: 1,
              top: other,
              left: dominant,
            }
          : {
              height: size,
              width: 1,
              left: other,
              top: dominant,
            }
      }

      const topIndicator = {
        ...position('x', cellFrame, targetCellBoundingBox, distanceLeft, distanceTop),
        compensateNegative: compensationNegative(
          'x',
          cellFrame,
          targetCellBoundingBox,
          distanceTop,
        ),
        compensatePositive: compensationPositive(
          'x',
          cellFrame,
          targetCellBoundingBox,
          distanceTop,
        ),
      }

      const leftIndicator = {
        ...position('y', cellFrame, targetCellBoundingBox, distanceLeft, distanceLeft),
        compensateNegative: compensationNegative(
          'y',
          cellFrame,
          targetCellBoundingBox,
          distanceLeft,
        ),
        compensatePositive: compensationPositive(
          'y',
          cellFrame,
          targetCellBoundingBox,
          distanceLeft,
        ),
      }

      return { topIndicator, leftIndicator }
    }, [cellFrame, targetCellBoundingBox, distanceLeft, distanceTop])

    if (targetCellBoundingBox == null || cellFrame == null || positioning == null) {
      return null
    }

    const backgroundColor = colorTheme.primary.value
    const dashedBorder = `1px dashed ${backgroundColor}`

    return (
      <React.Fragment>
        {/* top distance */}
        <React.Fragment>
          <div
            style={{
              position: 'absolute',
              borderLeft: dashedBorder,
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',

              left: positioning.topIndicator.left,
              top: positioning.topIndicator.top,
              width: 1,
              height: Math.abs(distanceTop),
            }}
          >
            <span
              style={{
                backgroundColor: backgroundColor,
                padding: '0px 2px',
                borderRadius: 2,
                fontSize: 9,
                color: '#fff',
              }}
            >
              {distanceTop}
            </span>
          </div>
          {/* compensate */}
          {positioning.topIndicator.compensateNegative != null ? (
            <div
              style={{
                position: 'absolute',
                borderTop: dashedBorder,

                left: positioning.topIndicator.compensateNegative.left,
                top: positioning.topIndicator.compensateNegative.top,
                width: positioning.topIndicator.compensateNegative.width,
                height: positioning.topIndicator.compensateNegative.height,
              }}
            />
          ) : null}
          {positioning.topIndicator.compensatePositive != null ? (
            <div
              style={{
                position: 'absolute',
                borderTop: dashedBorder,

                left: positioning.topIndicator.compensatePositive.left,
                top: positioning.topIndicator.compensatePositive.top,
                width: positioning.topIndicator.compensatePositive.width,
                height: positioning.topIndicator.compensatePositive.height,
              }}
            />
          ) : null}
        </React.Fragment>

        {/* left distance */}
        <React.Fragment>
          <div
            style={{
              position: 'absolute',
              borderTop: dashedBorder,
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',

              left: positioning.leftIndicator.left,
              top: positioning.leftIndicator.top,
              width: Math.abs(distanceLeft),
              height: 1,
            }}
          >
            <span
              style={{
                backgroundColor: backgroundColor,
                padding: '0px 2px',
                borderRadius: 2,
                fontSize: 9,
                color: '#fff',
              }}
            >
              {distanceLeft}
            </span>
          </div>
          {/* compensate */}
          {positioning.leftIndicator.compensateNegative != null ? (
            <div
              style={{
                position: 'absolute',
                borderLeft: dashedBorder,

                left: positioning.leftIndicator.compensateNegative.left,
                top: positioning.leftIndicator.compensateNegative.top,
                width: positioning.leftIndicator.compensateNegative.width,
                height: positioning.leftIndicator.compensateNegative.height,
              }}
            />
          ) : null}
          {positioning.leftIndicator.compensatePositive != null ? (
            <div
              style={{
                position: 'absolute',
                borderLeft: dashedBorder,

                left: positioning.leftIndicator.compensatePositive.left,
                top: positioning.leftIndicator.compensatePositive.top,
                width: positioning.leftIndicator.compensatePositive.width,
                height: positioning.leftIndicator.compensatePositive.height,
              }}
            />
          ) : null}
        </React.Fragment>
      </React.Fragment>
    )
  },
)

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

interface GridResizeControlProps {
  target: ElementPath
}

export const GridResizeControlsComponent = ({ target }: GridResizeControlProps) => {
  const colorTheme = useColorTheme()

  const element = useEditorState(
    Substores.metadata,
    (store) => MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, target),
    'GridResizeControls element',
  )

  const dispatch = useDispatch()
  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'GridResizeControls scale',
  )

  const resizeControlRef = useRefEditorState((store) =>
    store.editor.canvas.interactionSession?.activeControl.type !== 'GRID_RESIZE_HANDLE'
      ? null
      : store.editor.canvas.interactionSession.activeControl,
  )

  const dragRef = useRefEditorState((store) =>
    store.editor.canvas.interactionSession?.interactionData.type !== 'DRAG'
      ? null
      : store.editor.canvas.interactionSession?.interactionData.drag,
  )

  const [startingBounds, setStartingBounds] = React.useState<CanvasRectangle | null>(null)
  const [bounds, setBounds] = React.useState<CanvasRectangle | null>(null)
  const onMouseMove = React.useCallback(() => {
    if (resizeControlRef.current == null || dragRef.current == null) {
      return
    }

    if (startingBounds == null) {
      return
    }
    setBounds(
      resizeBoundingBoxFromSide(
        startingBounds,
        dragRef.current,
        gridEdgeToEdgePosition(resizeControlRef.current.edge),
        'non-center-based',
        null,
      ),
    )
  }, [dragRef, resizeControlRef, startingBounds])

  const isResizing = bounds != null

  const onMouseUp = React.useCallback(() => {
    setBounds(null)
    setStartingBounds(null)
  }, [])

  React.useEffect(() => {
    window.addEventListener('mousemove', onMouseMove)
    window.addEventListener('mouseup', onMouseUp)
    return () => {
      window.removeEventListener('mousemove', onMouseMove)
      window.removeEventListener('mouseup', onMouseUp)
    }
  }, [onMouseMove, onMouseUp])

  const startResizeInteraction = React.useCallback(
    (uid: string, edge: GridResizeEdge) => (event: React.MouseEvent) => {
      event.stopPropagation()
      const frame = zeroRectIfNullOrInfinity(element?.globalFrame ?? null)
      setBounds(frame)
      setStartingBounds(frame)
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
            gridResizeHandle(uid, edge),
            'zero-drag-not-permitted',
          ),
        ),
      ])
    },
    [canvasOffsetRef, dispatch, element?.globalFrame, scale],
  )

  const canShowHandles = React.useMemo(() => {
    if (isResizing) {
      return true
    }
    if (element?.globalFrame == null || isInfinityRectangle(element.globalFrame)) {
      return false
    }
    const scaledFrame = scaleRect(element.globalFrame, scale)
    return scaledFrame.width * scale > 30 && scaledFrame.height > 30
  }, [element, scale, isResizing])

  const onEdgeMouseDown = React.useCallback(
    (position: EdgePosition) => (e: React.MouseEvent<HTMLDivElement>) => {
      if (element == null) {
        return
      }

      const edge = edgePositionToGridResizeEdge(position)
      if (edge == null) {
        return
      }

      startResizeInteraction(EP.toUid(element.elementPath), edge)(e)
    },
    [element, startResizeInteraction],
  )

  const resizeEdges = useResizeEdges([target], {
    onEdgeDoubleClick: () => NO_OP,
    onEdgeMouseMove: NO_OP,
    onEdgeMouseDown: onEdgeMouseDown,
    cursors: {
      top: CSSCursor.RowResize,
      bottom: CSSCursor.RowResize,
      left: CSSCursor.ColResize,
      right: CSSCursor.ColResize,
    },
  })

  if (
    element == null ||
    element.globalFrame == null ||
    isInfinityRectangle(element.globalFrame) ||
    !canShowHandles
  ) {
    return null
  }

  return (
    <CanvasOffsetWrapper>
      <div
        data-testid={`grid-resize-container-${EP.toString(element.elementPath)}`}
        key={`grid-resize-container-${EP.toString(element.elementPath)}`}
        style={{
          pointerEvents: 'none',
          position: 'absolute',
          top: bounds?.y ?? element.globalFrame.y,
          left: bounds?.x ?? element.globalFrame.x,
          width: bounds?.width ?? element.globalFrame.width,
          height: bounds?.height ?? element.globalFrame.height,
          backgroundColor: isResizing ? colorTheme.primary25.value : 'transparent',
        }}
      >
        <div
          style={{
            position: 'relative',
            width: '100%',
            height: '100%',
            pointerEvents: 'none',
          }}
        >
          {resizeEdges.top}
          {resizeEdges.left}
          {resizeEdges.bottom}
          {resizeEdges.right}
        </div>
      </div>
    </CanvasOffsetWrapper>
  )
}

function gridEdgeToCSSCursor(edge: GridResizeEdge): CSSCursor {
  switch (edge) {
    case 'column-end':
    case 'column-start':
      return CSSCursor.ColResize
    case 'row-end':
    case 'row-start':
      return CSSCursor.RowResize
    default:
      assertNever(edge)
  }
}

function gridKeyFromPath(path: ElementPath): string {
  return `grid-${EP.toString(path)}`
}

const borderWidth = 1
function gridPlaceholderBorder(color: string, scale: number): string {
  return `${borderWidth / scale}px solid ${color}`
}

const borderExtension = 0.5
function gridPlaceholderTopOrLeftPosition(scale: number): string {
  return `${-borderExtension / scale}px`
}

function gridPlaceholderWidthOrHeight(scale: number): string {
  return `calc(100% + ${(borderExtension * 2) / scale}px)`
}

function useAllGrids(metadata: ElementInstanceMetadataMap) {
  return React.useMemo(() => {
    return MetadataUtils.getAllGrids(metadata)
  }, [metadata])
}
