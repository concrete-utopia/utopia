/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import { v4 as UUID } from 'uuid'
import type { AnimationControls } from 'framer-motion'
import { motion, useAnimationControls } from 'framer-motion'
import type { CSSProperties } from 'react'
import React from 'react'
import type { Sides } from 'utopia-api/core'
import type { ElementPath } from 'utopia-shared/src/types'
import type { PropsOrJSXAttributes } from '../../../core/model/element-metadata-utils'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls, range, stripNulls, uniqBy } from '../../../core/shared/array-utils'
import { defaultEither, eitherToMaybe } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import type {
  ElementInstanceMetadataMap,
  GridAutoOrTemplateDimensions,
  GridContainerProperties,
  GridPositionOrSpan,
} from '../../../core/shared/element-template'
import {
  isGridAutoOrTemplateDimensions,
  isGridSpan,
  type GridAutoOrTemplateBase,
} from '../../../core/shared/element-template'
import type { CanvasPoint, CanvasRectangle, LocalRectangle } from '../../../core/shared/math-utils'
import {
  canvasPoint,
  canvasRectangle,
  isFiniteRectangle,
  isInfinityRectangle,
  nullIfInfinity,
  pointsEqual,
  scaleRect,
  windowPoint,
  windowRectangle,
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
  printCSSNumberWithDefaultUnit,
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
import {
  getGridChildCellCoordBoundsFromCanvas,
  gridCellTargetId,
} from '../canvas-strategies/strategies/grid-cell-bounds'
import {
  getGlobalFrameOfGridCellFromMetadata,
  getGridRelatedIndexes,
  getGridElementPinState,
  printPin,
  getGridIdentifierContainerOrComponentPath,
  gridIdentifierToString,
  gridIdentifiersSimilar,
  findOriginalGrid,
  isAutoGridPin,
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
  GridCellTestId,
  GridControlKey,
  gridEdgeToEdgePosition,
  GridElementContainingBlockKey,
  GridMeasurementHelperKey,
  GridMeasurementHelperMap,
  GridMeasurementHelpersKey,
  useGridData,
  useGridMeasurementHelperData,
} from './grid-controls-for-strategies'
import { useMaybeHighlightElement } from './select-mode/select-mode-hooks'
import { useResizeEdges } from './select-mode/use-resize-edges'
import { getGridHelperStyleMatchingTargetGrid } from './grid-controls-helpers'
import { isFillOrStretchModeAppliedOnSpecificSide } from '../../inspector/inspector-common'
import type { PinOutlineProps } from './position-outline'
import { PinOutline, usePropsOrJSXAttributes } from './position-outline'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { styleStringInArray } from '../../../utils/common-constants'
import { gridContainerIdentifier, type GridIdentifier } from '../../editor/store/editor-state'
import type { RulerMarkerType } from './grid-controls-ruler-markers'
import { rulerMarkerIcons } from './grid-controls-ruler-markers'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import {
  getCanvasBounds,
  useGetFrameWhenOutsideBounds,
} from './elements-outside-visible-area-hooks'
import { useSetAtom } from 'jotai'
import { showRulersAtom } from '../../editor/canvas-toolbar'

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

const GridTrackSizeLabel = React.memo((props: GridResizingControlProps) => {
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

  const canResize = React.useMemo(() => {
    return (
      props.fromPropsAxisValues?.type === 'DIMENSIONS' &&
      props.fromPropsAxisValues.dimensions.length > 0
    )
  }, [props.fromPropsAxisValues])

  const mouseDownHandler = React.useCallback(
    (event: React.MouseEvent): void => {
      event.stopPropagation()
      event.preventDefault()

      if (!canResize) {
        return
      }

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
    },
    [canvasOffset, dispatch, props.axis, props.dimensionIndex, scale, setResizingIndex, canResize],
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

  const cssProp = React.useMemo(
    () => ({
      backgroundColor:
        props.resizing === 'resize-target'
          ? colorTheme.primary.value
          : colorTheme.primarySubdued.value,
      '&:hover': {
        zIndex: 1,
        backgroundColor: colorTheme.primary.value,
      },
    }),
    [props.resizing, colorTheme],
  )

  return (
    <div
      key={containerId}
      data-testid={containerId}
      style={{
        display: 'flex',
        alignItems: props.axis === 'column' ? 'flex-start' : 'center',
        justifyContent: props.axis === 'column' ? 'center' : 'flex-start',
        height: '100%',
        width: '100%',
        position: 'relative',
      }}
    >
      <div
        data-testid={labelId}
        style={{
          position: 'absolute',
          zoom: 1 / scale,
          height: GRID_RESIZE_HANDLE_SIZE,
          right: props.axis === 'row' ? 'calc(100% + 8px)' : undefined,
          bottom: props.axis === 'column' ? 'calc(100% + 8px)' : undefined,
          borderRadius: 3,
          padding: '0 4px',
          border: `.1px solid ${colorTheme.white.value}`,
          color: colorTheme.white.value,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          cursor: canResize
            ? gridEdgeToCSSCursor(props.axis === 'column' ? 'column-start' : 'row-start')
            : 'default',
          pointerEvents: 'initial',
        }}
        onMouseDown={mouseDownHandler}
        onMouseMove={onMouseMove}
        css={cssProp}
      >
        {getLabelForAxis(props.dimension, props.dimensionIndex, props.fromPropsAxisValues)}
      </div>
    </div>
  )
})
GridTrackSizeLabel.displayName = 'GridTrackSizeLabel'

const GridResizingStripedIndicator = React.memo((props: GridResizingControlProps) => {
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'GridResizingControl scale',
  )
  const colorTheme = useColorTheme()

  const labelId = `grid-${props.axis}-handle-${props.dimensionIndex}`
  const containerId = `${labelId}-container`

  return (
    <div
      key={containerId}
      data-testid={containerId}
      style={{
        display: 'flex',
        alignItems: props.axis === 'column' ? 'flex-start' : 'center',
        justifyContent: props.axis === 'column' ? 'center' : 'flex-start',
        height: '100%',
        width: '100%',
        position: 'relative',
      }}
    >
      {when(
        props.resizing !== 'not-resizing',
        <div
          style={{
            display: 'flex',
            width: '100%',
            height: '100%',
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
            props.dimension.lineName != null,
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
              {props.dimension.lineName}
            </div>,
          )}
        </div>,
      )}
    </div>
  )
})
GridResizingStripedIndicator.displayName = 'GridResizingStripedIndicator'

interface GridResizingProps {
  targetGrid: GridMeasurementHelperData
  axisValues: GridAutoOrTemplateBase | null
  fromPropsAxisValues: GridAutoOrTemplateBase | null
  stripedAreaLength: number | null
  containingFrame: CanvasRectangle
  axis: Axis
  gap: number | null
  padding: Sides | null
  justifyContent: string | undefined
  alignContent: string | undefined
}

const GridResizing = React.memo((props: GridResizingProps) => {
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
      const dimensions = props.axisValues.dimensions

      const helperGridBaseStyle: React.CSSProperties = getGridHelperStyleMatchingTargetGrid(
        props.targetGrid,
      )

      const helperGridStyle: React.CSSProperties = {
        ...helperGridBaseStyle,
        gridTemplateRows: props.axis === 'row' ? helperGridBaseStyle.gridTemplateRows : '1fr',
        gridTemplateColumns:
          props.axis === 'column' ? helperGridBaseStyle.gridTemplateColumns : '1fr',
      }

      return (
        <React.Fragment>
          <div style={helperGridStyle}>
            {dimensions.flatMap((dimension, dimensionIndex) => {
              return (
                <GridResizingStripedIndicator
                  key={`grid-resizing-striped-indicator-${dimensionIndex}`}
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
          <div style={helperGridStyle}>
            {dimensions.flatMap((dimension, dimensionIndex) => {
              return (
                <GridTrackSizeLabel
                  key={`grid-resizing-label-${dimensionIndex}`}
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
        </React.Fragment>
      )
    case 'FALLBACK':
      return null
    default:
      assertNever(props.axisValues)
  }
})
GridResizing.displayName = 'GridResizing'

interface GridRowColumnResizingControlsProps {
  target: GridIdentifier
}

export const GridRowColumnResizingControlsComponent = ({
  target,
}: GridRowColumnResizingControlsProps) => {
  const grids = useGridData([target])

  function getStripedAreaLength(
    template: GridAutoOrTemplateBase | null,
    gap: number,
  ): number | null {
    if (template?.type !== 'DIMENSIONS') {
      return null
    }
    const fromDimensions = template.dimensions.reduce((acc, curr, index) => {
      if (curr.type === 'NUMBER') {
        return acc + curr.value.value + (index > 0 ? gap : 0)
      }
      return acc
    }, 0)
    if (fromDimensions <= 0) {
      return null
    }
    return fromDimensions
  }

  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'GridRowColumnResizingControls scale',
  )

  const selectedGridItems = useSelectedGridItems()

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
        if (selectedGridItems.length > 0) {
          return null
        }
        return (
          <GridResizing
            key={`grid-resizing-column-${gridIdentifierToString(grid.identifier)}}`}
            targetGrid={grid}
            axisValues={grid.gridTemplateColumns}
            fromPropsAxisValues={grid.gridTemplateColumnsFromProps}
            containingFrame={grid.frame}
            axis={'column'}
            gap={grid.columnGap ?? grid.gap}
            padding={grid.padding}
            stripedAreaLength={
              getStripedAreaLength(grid.gridTemplateRows, grid.rowGap ?? grid.gap ?? 0) ??
              grid.frame.height
            }
            alignContent={grid.computedStyling.justifyContent}
            justifyContent={grid.computedStyling.alignContent}
          />
        )
      })}
      {gridsWithVisibleResizeControls.flatMap((grid) => {
        if (selectedGridItems.length > 0) {
          return null
        }
        return (
          <GridResizing
            key={`grid-resizing-row-${gridIdentifierToString(grid.identifier)}}`}
            targetGrid={grid}
            axisValues={grid.gridTemplateRows}
            fromPropsAxisValues={grid.gridTemplateRowsFromProps}
            containingFrame={grid.frame}
            axis={'row'}
            gap={grid.rowGap ?? grid.gap}
            padding={grid.padding}
            stripedAreaLength={
              getStripedAreaLength(grid.gridTemplateColumns, grid.columnGap ?? grid.gap ?? 0) ??
              grid.frame.width
            }
            alignContent={grid.computedStyling.alignContent}
            justifyContent={grid.computedStyling.justifyContent}
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
        MetadataUtils.isGridItemWithPositioning(store.editor.jsxMetadata, elementPath),
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

  const anyTargetNotPinned = useEditorState(
    Substores.metadata,
    (store) =>
      store.editor.selectedViews.some(
        (elementPath) =>
          getGridElementPinState(
            MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, elementPath)
              ?.specialSizeMeasurements.elementGridPropertiesFromProps ?? null,
          ) !== 'pinned',
      ),
    'GridControl anyTargetNotPinned',
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
    const children = (() => {
      switch (grid.identifier.type) {
        case 'GRID_CONTAINER':
          return MetadataUtils.getChildrenUnordered(jsxMetadata, grid.identifier.container)
        case 'GRID_ITEM':
          return MetadataUtils.getSiblingsUnordered(jsxMetadata, grid.identifier.item)
        default:
          assertNever(grid.identifier)
      }
    })()
    return mapDropNulls((cell, index) => {
      if (cell == null || cell.globalFrame == null || !isFiniteRectangle(cell.globalFrame)) {
        return null
      }
      const countedRow = Math.floor(index / grid.columns) + 1
      const countedColumn = Math.floor(index % grid.columns) + 1

      const columnFromProps = cell.specialSizeMeasurements.elementGridProperties.gridColumnStart
      const rowFromProps = cell.specialSizeMeasurements.elementGridProperties.gridRowStart

      function getAxisValue(value: GridPositionOrSpan | null, counted: number): number {
        if (
          value == null ||
          isCSSKeyword(value) ||
          isGridSpan(value) ||
          value.numericalPosition == null
        ) {
          return counted
        }
        return value.numericalPosition
      }
      return {
        elementPath: cell.elementPath,
        globalFrame: cell.globalFrame,
        borderRadius: cell.specialSizeMeasurements.borderRadius,
        column: getAxisValue(columnFromProps, countedColumn),
        row: getAxisValue(rowFromProps, countedRow),
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
    disabled: anyTargetAbsolute || anyTargetNotPinned,
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

  const targetRootCellIsValidTarget = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.controls.gridControlData?.rootCell != null,
    'GridControl targetRootCellIsValidTarget',
  )

  const dontShowActiveCellHighlight =
    (!targetsAreCellsWithPositioning && anyTargetAbsolute) ||
    (anyTargetNotPinned && !targetRootCellIsValidTarget)

  const gridContainerOrComponentPath = getGridIdentifierContainerOrComponentPath(grid.identifier)
  return (
    <React.Fragment>
      {/* grid lines */}
      <div
        key={gridKeyFromPath(gridContainerOrComponentPath)}
        id={gridKeyFromPath(gridContainerOrComponentPath)}
        data-grid-path={EP.toString(gridContainerOrComponentPath)}
        style={style}
      >
        {placeholders.map((cell) => {
          const countedRow = Math.floor(cell / grid.columns) + 1
          const countedColumn = Math.floor(cell % grid.columns) + 1
          const id = gridCellTargetId(gridContainerOrComponentPath, countedRow, countedColumn)
          const borderID = `${id}-border`

          const isActiveGrid =
            (dragging != null && EP.isParentOf(gridContainerOrComponentPath, dragging)) ||
            (currentHoveredGrid != null &&
              gridIdentifiersSimilar(grid.identifier, currentHoveredGrid))
          const isActiveCell =
            isActiveGrid &&
            countedColumn === currentHoveredCell?.column &&
            countedRow === currentHoveredCell?.row

          const activePositioningTarget = isActiveCell && !dontShowActiveCellHighlight // TODO: move the logic into runGridChangeElementLocation and do not set targetCell prop in these cases

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

  const { grids, gridItems } = useAllGrids(metadata)

  return (
    <CanvasOffsetWrapper>
      {grids.map((grid) => (
        <GridMeasurementHelper
          key={GridMeasurementHelpersKey(grid)}
          elementPath={grid}
          source='element'
        />
      ))}
      {gridItems.map((gridItem) => (
        <GridMeasurementHelper
          key={GridMeasurementHelpersKey(gridItem)}
          elementPath={gridItem}
          source='parent'
        />
      ))}
    </CanvasOffsetWrapper>
  )
})
GridMeasurementHelpers.displayName = 'GridMeasurementHelpers'

export interface GridMeasurementHelperProps {
  elementPath: ElementPath
  source: 'parent' | 'element'
}

export const GridMeasurementHelper = React.memo<GridMeasurementHelperProps>(
  ({ elementPath, source }) => {
    const gridData = useGridMeasurementHelperData(elementPath, source)
    if (gridData == null) {
      return null
    }

    const placeholders = range(0, gridData.cells)

    const style: CSSProperties = {
      ...getGridHelperStyleMatchingTargetGrid(gridData),
      opacity: 1,
    }

    const uid = UUID()
    GridMeasurementHelperMap.current.set(gridData.element, uid)
    return (
      <div id={uid} data-grid-path={EP.toString(elementPath)} style={style}>
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
  },
)
GridMeasurementHelper.displayName = 'GridMeasurementHelper'

export const GridControlsComponent = ({ targets }: GridControlsProps) => {
  const ancestorPaths = React.useMemo(() => {
    return targets.flatMap((target) =>
      EP.getAncestors(getGridIdentifierContainerOrComponentPath(target)),
    )
  }, [targets])
  const ancestorGrids: Array<GridIdentifier> = useEditorState(
    Substores.metadata,
    (store) => {
      return ancestorPaths
        .filter((ancestorPath) => {
          const ancestorMetadata = MetadataUtils.findElementByElementPath(
            store.editor.jsxMetadata,
            ancestorPath,
          )
          return MetadataUtils.isGridLayoutedContainer(ancestorMetadata)
        })
        .map((p) => gridContainerIdentifier(p))
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

  const gridsWithVisibleControls: Array<GridIdentifier> = [...targets, ...hoveredGrids]

  const selectedGridItems = useSelectedGridItems()
  const isGridItemInteractionActive = useIsGridItemInteractionActive()

  // Uniqify the grid paths, and then sort them by depth.
  // With the lowest depth grid at the end so that it renders on top and catches the events
  // before those above it in the hierarchy.
  const grids = useGridData(
    uniqBy([...gridsWithVisibleControls, ...ancestorGrids], gridIdentifiersSimilar).sort((a, b) => {
      const aDepth =
        a.type === 'GRID_CONTAINER' ? EP.fullDepth(a.container) : EP.fullDepth(a.item) - 1
      const bDepth =
        b.type === 'GRID_CONTAINER' ? EP.fullDepth(b.container) : EP.fullDepth(b.item) - 1
      return aDepth - bDepth
    }),
  )

  const isGridItemSelectedWithoutInteraction =
    selectedGridItems.length > 0 && !isGridItemInteractionActive

  if (grids.length === 0) {
    return null
  }

  return (
    <div id={'grid-controls'}>
      <CanvasOffsetWrapper>
        {grids.map((grid) => {
          const gridContainerOrComponentPath = getGridIdentifierContainerOrComponentPath(
            grid.identifier,
          )
          const shouldHaveVisibleControls = gridsWithVisibleControls.some((g) => {
            if (isGridItemSelectedWithoutInteraction) {
              return false
            }

            const visibleControlPath = getGridIdentifierContainerOrComponentPath(g)
            return EP.pathsEqual(gridContainerOrComponentPath, visibleControlPath)
          })

          return (
            <GridControl
              key={GridControlKey(gridContainerOrComponentPath)}
              grid={grid}
              controlsVisible={shouldHaveVisibleControls ? 'visible' : 'not-visible'}
            />
          )
        })}
        {/* Ruler markers */}
        {when(
          isFeatureEnabled('Grid Ruler Markers'),
          selectedGridItems.map((path) => {
            return <RulerMarkers key={`ruler-markers-${EP.toString(path)}`} path={path} />
          }),
        )}
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
      return getGlobalFrameOfGridCellFromMetadata(gridMetadata, props.targetRootCell)
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
    'useCellAnimation selectedViews',
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

    return getGlobalFrameOfGridCellFromMetadata(gridMetadata, targetRootCell)
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
  target: GridIdentifier
}

export const GridResizeControlsComponent = ({ target }: GridResizeControlProps) => {
  const gridTarget = getGridIdentifierContainerOrComponentPath(target)
  const colorTheme = useColorTheme()

  const element = useEditorState(
    Substores.metadata,
    (store) => MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, gridTarget),
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

  const resizeEdges = useResizeEdges([gridTarget], {
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

  const resizeDirection = useEditorState(
    Substores.metadata,
    (store) => {
      if (element == null) {
        return { horizontal: false, vertical: false }
      }
      return {
        horizontal: isFillOrStretchModeAppliedOnSpecificSide(
          store.editor.jsxMetadata,
          element.elementPath,
          'horizontal',
        ),
        vertical: isFillOrStretchModeAppliedOnSpecificSide(
          store.editor.jsxMetadata,
          element.elementPath,
          'vertical',
        ),
      }
    },
    'GridResizeControlsComponent resizeDirection',
  )

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
          {when(
            resizeDirection.vertical,
            <React.Fragment>
              {resizeEdges.top}
              {resizeEdges.bottom}
            </React.Fragment>,
          )}
          {when(
            resizeDirection.horizontal,
            <React.Fragment>
              {resizeEdges.left}
              {resizeEdges.right}
            </React.Fragment>,
          )}
        </div>
      </div>
    </CanvasOffsetWrapper>
  )
}

function collectGridPinOutlines(
  attributes: PropsOrJSXAttributes,
  frame: LocalRectangle,
  scale: number,
): PinOutlineProps[] {
  const pinLeft = eitherToMaybe(getLayoutProperty('left', attributes, styleStringInArray))
  const pinTop = eitherToMaybe(getLayoutProperty('top', attributes, styleStringInArray))
  const pinRight = eitherToMaybe(getLayoutProperty('right', attributes, styleStringInArray))
  const pinBottom = eitherToMaybe(getLayoutProperty('bottom', attributes, styleStringInArray))
  let pins: PinOutlineProps[] = []
  if (pinLeft != null) {
    let startY: string | number | undefined = undefined
    let endY: string | number | undefined = undefined
    if (pinTop != null) {
      startY = `calc(${printCSSNumberWithDefaultUnit(pinTop, 'px')} + ${frame.height / 2}px)`
    } else if (pinBottom != null) {
      endY = `calc(${printCSSNumberWithDefaultUnit(pinBottom, 'px')} + ${frame.height / 2}px)`
    } else {
      startY = frame.height / 2
    }
    pins.push({
      name: 'left',
      isHorizontalLine: true,
      size: printCSSNumberWithDefaultUnit(pinLeft, 'px'),
      startX: 0,
      startY: startY,
      endY: endY,
      scale: scale,
    })
  }
  if (pinTop != null) {
    let startX: string | number | undefined = undefined
    let endX: string | number | undefined = undefined
    if (pinLeft != null) {
      startX = `calc(${printCSSNumberWithDefaultUnit(pinLeft, 'px')} + ${frame.width / 2}px)`
    } else if (pinRight != null) {
      endX = `calc(${printCSSNumberWithDefaultUnit(pinRight, 'px')} + ${frame.width / 2}px)`
    } else {
      startX = frame.width / 2
    }
    pins.push({
      name: 'top',
      isHorizontalLine: false,
      size: printCSSNumberWithDefaultUnit(pinTop, 'px'),
      startX: startX,
      endX: endX,
      startY: 0,
      scale: scale,
    })
  }
  if (pinRight != null) {
    let startY: string | number | undefined = undefined
    let endY: string | number | undefined = undefined
    if (pinTop != null) {
      startY = `calc(${printCSSNumberWithDefaultUnit(pinTop, 'px')} + ${frame.height / 2}px)`
    } else if (pinBottom != null) {
      endY = `calc(${printCSSNumberWithDefaultUnit(pinBottom, 'px')} + ${frame.height / 2}px)`
    } else {
      endY = frame.height / 2
    }
    pins.push({
      name: 'right',
      isHorizontalLine: true,
      size: printCSSNumberWithDefaultUnit(pinRight, 'px'),
      startY: startY,
      endX: 0,
      endY: endY,
      scale: scale,
    })
  }
  if (pinBottom != null) {
    let startX: string | number | undefined = undefined
    let endX: string | number | undefined = undefined
    if (pinLeft != null) {
      startX = `calc(${printCSSNumberWithDefaultUnit(pinLeft, 'px')} + ${frame.width / 2}px)`
    } else if (pinRight != null) {
      endX = `calc(${printCSSNumberWithDefaultUnit(pinRight, 'px')} + ${frame.width / 2}px)`
    } else {
      endX = frame.width / 2
    }
    pins.push({
      name: 'bottom',
      isHorizontalLine: false,
      size: printCSSNumberWithDefaultUnit(pinBottom, 'px'),
      startX: startX,
      endX: endX,
      endY: 0,
      scale: scale,
    })
  }
  return pins
}

export interface GridElementContainingBlockProps {
  gridPath: ElementPath
  gridChild: ElementPath
}

function nullHandlingPrintPin(
  gridTemplate: GridContainerProperties | null | undefined,
  pin: GridPositionOrSpan | null | undefined,
  axis: 'row' | 'column',
): string | number | undefined {
  if (gridTemplate == null || pin == null) {
    return undefined
  }
  return printPin(gridTemplate, pin, axis)
}

const GridElementContainingBlock = React.memo<GridElementContainingBlockProps>((props) => {
  const gridData = useGridMeasurementHelperData(props.gridPath, 'element')
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'GridElementContainingBlock scale',
  )
  const position = useEditorState(
    Substores.metadata,
    (store) => {
      const childMetadata = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        props.gridChild,
      )
      return childMetadata?.specialSizeMeasurements.position
    },
    'GridElementContainingBlock position',
  )
  const gridChildStyle: CSSProperties = useEditorState(
    Substores.metadata,
    (store) => {
      const childMetadata = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        props.gridChild,
      )
      if (childMetadata == null) {
        return {}
      }
      const gridFromProps = childMetadata.specialSizeMeasurements.elementGridPropertiesFromProps
      const gridComputed = childMetadata.specialSizeMeasurements.elementGridProperties
      return {
        gridColumnStart: nullHandlingPrintPin(
          childMetadata.specialSizeMeasurements.containerGridProperties,
          gridFromProps.gridColumnStart ?? gridComputed.gridColumnStart,
          'column',
        ),
        gridColumnEnd: nullHandlingPrintPin(
          childMetadata.specialSizeMeasurements.containerGridProperties,
          gridFromProps.gridColumnEnd ?? gridComputed.gridColumnEnd,
          'column',
        ),
        gridRowStart: nullHandlingPrintPin(
          childMetadata.specialSizeMeasurements.containerGridProperties,
          gridFromProps.gridRowStart ?? gridComputed.gridRowStart,
          'row',
        ),
        gridRowEnd: nullHandlingPrintPin(
          childMetadata.specialSizeMeasurements.containerGridProperties,
          gridFromProps.gridRowEnd ?? gridComputed.gridRowEnd,
          'row',
        ),
        position: childMetadata.specialSizeMeasurements.position ?? undefined,
      }
    },
    'GridElementContainingBlock gridChildStyle',
  )
  const gridChildFrame = useEditorState(
    Substores.metadata,
    (store) => {
      return MetadataUtils.getLocalFrame(props.gridChild, store.editor.jsxMetadata, null)
    },
    'GridElementContainingBlock gridChildFrame',
  )
  const attributes = usePropsOrJSXAttributes(props.gridChild)

  if (gridChildFrame == null || isInfinityRectangle(gridChildFrame)) {
    return null
  }

  if (gridData == null) {
    return null
  }

  if (position !== 'absolute') {
    return null
  }

  const style: CSSProperties = {
    ...getGridHelperStyleMatchingTargetGrid(gridData),
    opacity: 1,
  }

  const pins: Array<PinOutlineProps> = collectGridPinOutlines(attributes, gridChildFrame, scale)

  return (
    <div
      id={GridElementContainingBlockKey(props.gridPath)}
      data-grid-path={EP.toString(props.gridPath)}
      style={style}
    >
      <div
        id={`${GridElementContainingBlockKey(props.gridPath)}-child`}
        style={{
          ...gridChildStyle,
          pointerEvents: 'none',
          top: 0,
          left: 0,
          bottom: 0,
          right: 0,
        }}
      >
        {pins.map((pin) => (
          <PinOutline {...pin} key={pin.name} />
        ))}
      </div>
    </div>
  )
})
GridElementContainingBlock.displayName = 'GridElementContainingBlock'

export interface GridElementContainingBlocksProps {}

export const GridElementContainingBlocks = React.memo<GridElementContainingBlocksProps>((props) => {
  const selectedGridChildElements = useEditorState(
    Substores.metadata,
    (store) => {
      return store.editor.selectedViews.filter((selectedView) => {
        return MetadataUtils.isGridItemWithLayoutProvidingGridParent(
          store.editor.jsxMetadata,
          selectedView,
        )
      })
    },
    'GridElementContainingBlocks selectedViews',
  )

  return (
    <CanvasOffsetWrapper>
      {selectedGridChildElements.map((selectedGridChildElement) => {
        return (
          <GridElementContainingBlock
            key={GridElementContainingBlockKey(selectedGridChildElement)}
            gridPath={EP.parentPath(selectedGridChildElement)}
            gridChild={selectedGridChildElement}
          />
        )
      })}
    </CanvasOffsetWrapper>
  )
})

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
    const gridPaths = MetadataUtils.getAllGrids(metadata)
    const gridItemPaths = MetadataUtils.getAllGridItems(metadata)
    const gridItemPathsWithoutGridPaths = gridItemPaths.filter(
      (path) => !gridPaths.some((gridPath) => EP.isParentOf(gridPath, path)),
    )
    return {
      grids: gridPaths,
      gridItems: gridItemPathsWithoutGridPaths,
    }
  }, [metadata])
}

function useIsGridItemInteractionActive() {
  return useEditorState(
    Substores.canvas,
    (store) => {
      if (store.editor.canvas.interactionSession == null) {
        return false
      }
      return (
        // movement
        store.editor.canvas.interactionSession.activeControl.type === 'GRID_CELL_HANDLE' ||
        // resize (cell)
        store.editor.canvas.interactionSession.activeControl.type === 'GRID_RESIZE_HANDLE' ||
        // resize (abs)
        store.editor.canvas.interactionSession.activeControl.type === 'RESIZE_HANDLE'
      )
    },
    'useIsGridItemInteractionActive isItemInteractionActive',
  )
}

function useSelectedGridItems(): ElementPath[] {
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
  const jsxMetadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
  return selectedViewsRef.current.filter((selected) =>
    MetadataUtils.isGridItem(jsxMetadataRef.current, selected),
  )
}

const rulerMarkerIconSize = 12 // px

type RulerMarkerData = {
  parentGrid: GridContainerProperties
  columnStart: RulerMarkerPositionData
  columnEnd: RulerMarkerPositionData
  rowStart: RulerMarkerPositionData
  rowEnd: RulerMarkerPositionData
}

type RulerMarkerPositionData = {
  top: number
  left: number
  position: GridPositionOrSpan | null
  bound: 'start' | 'end'
}

const navigatorWidth = 265
const topbarHeight = 40

const RulerMarkers = React.memo((props: { path: ElementPath }) => {
  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'GridResizingControl canvasOffset',
  )

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

      return {
        parentGrid: parentGrid,
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

  const isNavigatorOpen = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.leftMenu.visible,
    'RulerMarkers isNavigatorOpen',
  )
  const canvasBounds = getCanvasBounds()

  const isOutsideBounds = useGetFrameWhenOutsideBounds(
    canvasBounds != null
      ? windowRectangle({
          x: canvasBounds.x + (isNavigatorOpen ? navigatorWidth + 10 : 0),
          y: canvasBounds.y + topbarHeight,
          width: canvasBounds.width,
          height: canvasBounds.height,
        })
      : null,
  )

  const stickyColumnStart =
    isOutsideBounds(
      canvasRectangle({
        y: (markers?.columnStart.top ?? 0) - rulerMarkerIconSize * 2,
        x: (markers?.columnStart.left ?? 0) - rulerMarkerIconSize * 2,
        width: rulerMarkerIconSize,
        height: rulerMarkerIconSize,
      }),
    ) != null
  const stickyColumnEnd =
    isOutsideBounds(
      canvasRectangle({
        y: (markers?.columnEnd.top ?? 0) - rulerMarkerIconSize * 2,
        x: (markers?.columnEnd.left ?? 0) - rulerMarkerIconSize * 2,
        width: rulerMarkerIconSize,
        height: rulerMarkerIconSize,
      }),
    ) != null
  const stickyRowStart =
    isOutsideBounds(
      canvasRectangle({
        y: (markers?.rowStart.top ?? 0) - rulerMarkerIconSize * 2,
        x: (markers?.rowStart.left ?? 0) - rulerMarkerIconSize * 2,
        width: rulerMarkerIconSize,
        height: rulerMarkerIconSize,
      }),
    ) != null
  const stickyRowEnd =
    isOutsideBounds(
      canvasRectangle({
        y: (markers?.rowEnd.top ?? 0) - rulerMarkerIconSize * 2,
        x: (markers?.rowEnd.left ?? 0) - rulerMarkerIconSize * 2,
        width: rulerMarkerIconSize,
        height: rulerMarkerIconSize,
      }),
    ) != null

  const showRulers = useSetAtom(showRulersAtom)
  React.useEffect(() => {
    if (!isFeatureEnabled('Grid Ruler Markers sticky (window)')) {
      return
    }
    // horrible hack
    showRulers(stickyColumnEnd || stickyColumnStart)
  }, [stickyColumnEnd, stickyColumnStart, showRulers])

  if (markers == null) {
    return null
  }

  return (
    <React.Fragment>
      {when(
        isFeatureEnabled('Grid Ruler Markers sticky (window)') &&
          (stickyColumnStart || stickyColumnEnd),
        <div
          style={{
            position: 'absolute',
            top: -canvasOffset.y,
            left: -canvasOffset.x,
            width: '100vw',
            height: 20,
            background: 'white',
            borderBottom: '1px solid #ccc',
            boxShadow: '0px 2px 3px #00000022',
            display: 'flex',
            justifyContent: 'space-evenly',
          }}
          css={{
            opacity: 0.5,
          }}
        />,
      )}
      {when(
        isFeatureEnabled('Grid Ruler Markers sticky (window)') && (stickyRowStart || stickyRowEnd),
        <div
          style={{
            position: 'absolute',
            top: -canvasOffset.y,
            left: -canvasOffset.x + (isNavigatorOpen ? 263 : 0),
            height: '100vw',
            width: 20,
            background: 'white',
            borderRight: '1px solid #ccc',
            boxShadow: '2px 0px 3px #00000022',
          }}
          css={{
            opacity: 0.5,
          }}
        />,
      )}
      <RulerMarkerIndicator
        parentGrid={markers.parentGrid}
        marker={markers.columnStart}
        axis={'column'}
        sticky={stickyColumnStart}
      />
      <RulerMarkerIndicator
        parentGrid={markers.parentGrid}
        marker={markers.columnEnd}
        axis={'column'}
        sticky={stickyColumnEnd}
      />
      <RulerMarkerIndicator
        parentGrid={markers.parentGrid}
        marker={markers.rowStart}
        axis={'row'}
        sticky={stickyRowStart}
      />
      <RulerMarkerIndicator
        parentGrid={markers.parentGrid}
        marker={markers.rowEnd}
        axis={'row'}
        sticky={stickyRowEnd}
      />
    </React.Fragment>
  )
})
RulerMarkers.displayName = 'RulerMarkers'

const RulerMarkerIndicator = React.memo(
  (props: {
    parentGrid: GridContainerProperties
    marker: RulerMarkerPositionData
    axis: 'row' | 'column'
    sticky: boolean
  }) => {
    const colorTheme = useColorTheme()

    const sticky = isFeatureEnabled('Grid Ruler Markers sticky (window)') && props.sticky

    const canvasOffset = useEditorState(
      Substores.canvasOffset,
      (store) => store.editor.canvas.roundedCanvasOffset,
      'GridResizingControl canvasOffset',
    )

    const markerType = getRulerMarkerType({
      position: props.marker.position,
      bound: props.marker.bound,
    })
    const markerIcon = rulerMarkerIcons[markerType][props.axis]

    const canvasScale = useEditorState(
      Substores.canvasOffset,
      (store) => store.editor.canvas.scale,
      'RulerMarkerIndicator canvasScale',
    )

    const scaledTop = props.marker.top * canvasScale
    const top =
      scaledTop -
      skewMarkerPosition(props.axis === 'column', props.axis, props.marker.bound, markerType)

    const scaledLeft = props.marker.left * canvasScale
    const left =
      scaledLeft -
      skewMarkerPosition(props.axis === 'row', props.axis, props.marker.bound, markerType)

    const labelText = React.useMemo(() => {
      if (props.marker.position == null) {
        return null
      }
      return printPin(props.parentGrid, props.marker.position, props.axis)
    }, [props.marker, props.parentGrid, props.axis])

    const labelClass = 'ruler-marker-label'

    const isNavigatorOpen = useEditorState(
      Substores.restOfEditor,
      (store) => store.editor.leftMenu.visible,
      'RulerMarkerIndicator isNavigatorOpen',
    )

    return (
      <div
        style={{
          position: 'fixed',
          top: sticky && props.axis === 'column' ? -canvasOffset.y + 5 : top,
          left:
            sticky && props.axis === 'row'
              ? -canvasOffset.x + (isNavigatorOpen ? navigatorWidth : 0) + 5
              : left,
          color: colorTheme.primary.value,
          maxHeight: rulerMarkerIconSize,
          maxWidth: rulerMarkerIconSize,
          display: 'flex',
          zoom: 1 / canvasScale,
        }}
        css={{
          [`> .${labelClass}`]: {
            visibility: 'hidden',
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
    return 10
  }
  const pinnedEndColumn = axis === 'column' && markerType === 'pinned'
  if (pinnedEndColumn) {
    return 5
  }
  // any other ending marker, on the column
  const endColumn = bound === 'end' && axis === 'column'
  if (endColumn) {
    return 2
  }

  // span-end triangle, on the row
  const spanEndRow = axis === 'row' && markerType === 'span-end'
  if (spanEndRow) {
    return 9
  }
  // any other ending marker, on the row
  const endRow = bound === 'end' && axis === 'row'
  if (endRow) {
    return 6
  }

  // span-start triangle, on the column
  const spanStartColumn = axis === 'column' && markerType === 'span-start'
  if (spanStartColumn) {
    return 0
  }
  // any starting marker, on the column
  const startColumn = bound === 'start' && axis === 'column'
  if (startColumn) {
    return 5
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
