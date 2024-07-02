import React from 'react'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { isFiniteRectangle, windowPoint } from '../../../core/shared/math-utils'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'
import { useDispatch } from '../../editor/store/dispatch-context'
import { createInteractionViaMouse, gridAxisHandle } from '../canvas-strategies/interaction-state'
import CanvasActions from '../canvas-actions'
import { Modifier } from '../../../utils/modifiers'
import { windowToCanvasCoordinates } from '../dom-lookup'
import {
  isGridAutoOrTemplateDimensions,
  type GridAutoOrTemplateBase,
} from '../../../core/shared/element-template'
import { assertNever } from '../../../core/shared/utils'
import type { GridCSSNumber } from '../../../components/inspector/common/css-utils'
import type { Optic } from '../../../core/shared/optics/optics'
import {
  fromArrayIndex,
  fromField,
  fromTypeGuard,
  notNull,
} from '../../../core/shared/optics/optic-creators'
import { toFirst } from '../../../core/shared/optics/optic-utilities'
import { defaultEither } from '../../../core/shared/either'

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

function getFromPropsOptic(index: number): Optic<GridAutoOrTemplateBase | null, GridCSSNumber> {
  return notNull<GridAutoOrTemplateBase>()
    .compose(fromTypeGuard(isGridAutoOrTemplateDimensions))
    .compose(fromField('dimensions'))
    .compose(fromArrayIndex(index))
}

function gridCSSNumberToLabel(gridCSSNumber: GridCSSNumber): string {
  return `${gridCSSNumber.value}${gridCSSNumber.unit ?? ''}`
}

function getLabelForAxis(
  fromDOM: GridCSSNumber,
  index: number,
  fromProps: GridAutoOrTemplateBase | null,
): string {
  const fromPropsAtIndex = toFirst(getFromPropsOptic(index), fromProps)
  return gridCSSNumberToLabel(defaultEither(fromDOM, fromPropsAtIndex))
}

export const GridControls = controlForStrategyMemoized(() => {
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
        const gridTemplateColumnsFromProps =
          targetGridContainer.specialSizeMeasurements.containerGridPropertiesFromProps
            .gridTemplateColumns
        const gridTemplateRowsFromProps =
          targetGridContainer.specialSizeMeasurements.containerGridPropertiesFromProps
            .gridTemplateRows

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
          gridTemplateColumnsFromProps: gridTemplateColumnsFromProps,
          gridTemplateRowsFromProps: gridTemplateRowsFromProps,
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

  const dispatch = useDispatch()

  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
  const scaleRef = useRefEditorState((store) => store.editor.canvas.scale)

  return (
    <React.Fragment>
      <CanvasOffsetWrapper>
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
                      {getLabelForAxis(
                        dimension,
                        dimensionIndex,
                        grid.gridTemplateColumnsFromProps,
                      )}
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
                      {getLabelForAxis(dimension, dimensionIndex, grid.gridTemplateRowsFromProps)}
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
    </React.Fragment>
  )
})
