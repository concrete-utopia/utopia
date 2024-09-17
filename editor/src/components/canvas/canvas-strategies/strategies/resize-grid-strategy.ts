import {
  fromArrayIndex,
  fromField,
  fromTypeGuard,
  notNull,
} from '../../../../core/shared/optics/optic-creators'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import * as PP from '../../../../core/shared/property-path'
import { setProperty } from '../../commands/set-property-command'
import {
  controlsForGridPlaceholders,
  GridRowColumnResizingControls,
} from '../../controls/grid-controls'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { InteractionCanvasState } from '../canvas-strategy-types'
import {
  getTargetPathsFromInteractionTarget,
  emptyStrategyApplicationResult,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import type { GridDimension } from '../../../../components/inspector/common/css-utils'
import {
  gridCSSRepeat,
  isGridCSSNumber,
  printArrayGridDimensions,
} from '../../../../components/inspector/common/css-utils'
import { modify, toFirst } from '../../../../core/shared/optics/optic-utilities'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import type { Either } from '../../../../core/shared/either'
import { foldEither, isRight } from '../../../../core/shared/either'
import { roundToNearestWhole } from '../../../../core/shared/math-utils'
import type { GridAutoOrTemplateBase } from '../../../../core/shared/element-template'

export const resizeGridStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
) => {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }

  const selectedElement = selectedElements[0]

  const isGridCell = MetadataUtils.isGridCell(canvasState.startingMetadata, selectedElement)
  const isGrid = MetadataUtils.isGridLayoutedContainer(
    MetadataUtils.findElementByElementPath(canvasState.startingMetadata, selectedElement),
  )
  const isGridOrGridCell = isGridCell || isGrid

  if (!isGridOrGridCell) {
    return null
  }

  const gridPath = isGrid ? selectedElement : EP.parentPath(selectedElement)

  return {
    id: 'resize-grid-strategy',
    name: 'Resize Grid',
    descriptiveLabel: 'Resize Grid',
    icon: {
      category: 'tools',
      type: 'pointer',
    },
    controlsToRender: [
      {
        control: GridRowColumnResizingControls,
        props: { target: gridPath },
        key: `grid-row-col-resize-controls-${EP.toString(gridPath)}`,
        show: 'always-visible',
        priority: 'top',
      },
      controlsForGridPlaceholders(gridPath),
    ],
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'GRID_AXIS_HANDLE', 1),
    apply: () => {
      if (
        interactionSession == null ||
        interactionSession.interactionData.type !== 'DRAG' ||
        interactionSession.interactionData.drag == null ||
        interactionSession.activeControl.type !== 'GRID_AXIS_HANDLE'
      ) {
        return emptyStrategyApplicationResult
      }

      const modifiers = interactionSession.interactionData.modifiers

      const control = interactionSession.activeControl
      const drag = interactionSession.interactionData.drag
      const dragAmount = control.axis === 'column' ? drag.x : drag.y

      const gridSpecialSizeMeasurements =
        canvasState.startingMetadata[EP.toString(gridPath)].specialSizeMeasurements

      const originalValues =
        control.axis === 'column'
          ? gridSpecialSizeMeasurements.containerGridPropertiesFromProps.gridTemplateColumns
          : gridSpecialSizeMeasurements.containerGridPropertiesFromProps.gridTemplateRows
      const calculatedValues =
        control.axis === 'column'
          ? gridSpecialSizeMeasurements.containerGridProperties.gridTemplateColumns
          : gridSpecialSizeMeasurements.containerGridProperties.gridTemplateRows

      if (
        calculatedValues == null ||
        calculatedValues.type !== 'DIMENSIONS' ||
        originalValues == null ||
        originalValues.type !== 'DIMENSIONS'
      ) {
        return emptyStrategyApplicationResult
      }

      const expandedOriginalValues = originalValues.dimensions.reduce((acc, cur, index) => {
        if (cur.type === 'REPEAT') {
          let expanded: ExpandedGridDimension[] = []
          for (let i = 0; i < cur.times; i++) {
            expanded.push(
              ...cur.value.map((v) => ({
                ...v,
                repeated: true,
                originalIndex: index,
              })),
            )
          }
          return acc.concat(expanded)
        } else {
          return acc.concat({
            ...cur,
            originalIndex: index,
            repeated: false,
          })
        }
      }, [] as ExpandedGridDimension[])

      const mergedValues: GridAutoOrTemplateBase = {
        type: calculatedValues.type,
        dimensions: calculatedValues.dimensions.map((dim, index) => {
          if (index < expandedOriginalValues.length) {
            return expandedOriginalValues[index]
          }
          return dim
        }),
      }

      const unitOptic = fromArrayIndex<GridDimension>(control.columnOrRow)
        .compose(fromTypeGuard(isGridCSSNumber))
        .compose(fromField('value'))
        .compose(fromField('unit'))
        .compose(notNull())
      const valueOptic = fromArrayIndex<GridDimension>(control.columnOrRow)
        .compose(fromTypeGuard(isGridCSSNumber))
        .compose(fromField('value'))
        .compose(fromField('value'))

      const calculatedValue = toFirst(valueOptic, calculatedValues.dimensions)
      const mergedValue = toFirst(valueOptic, mergedValues.dimensions)
      const mergedUnit = toFirst(unitOptic, mergedValues.dimensions)
      const isFractional = isRight(mergedUnit) && mergedUnit.value === 'fr'
      const precision = modifiers.cmd ? 'coarse' : 'precise'

      const resizedDimensions = modify(
        valueOptic,
        (current) =>
          newResizedValue(
            current,
            getNewDragValue(dragAmount, isFractional, calculatedValue, mergedValue),
            precision,
            isFractional,
          ),
        mergedValues.dimensions,
      )

      const newDimensions = buildResizedDimensionsArray({
        expandedOriginalValues: expandedOriginalValues,
        originalValues: originalValues.dimensions,
        resizedValues: resizedDimensions,
        resizedIndex: control.columnOrRow,
      })
      const propertyValueAsString = printArrayGridDimensions(newDimensions)

      const commands = [
        setProperty(
          'always',
          gridPath,
          PP.create(
            'style',
            control.axis === 'column' ? 'gridTemplateColumns' : 'gridTemplateRows',
          ),
          propertyValueAsString,
        ),
        setElementsToRerenderCommand([gridPath]),
      ]

      return strategyApplicationResult(commands)
    },
  }
}

type ExpandedGridDimension = GridDimension & {
  repeated: boolean
  originalIndex: number
}

// Reconstruct the final, resized dimensions trying to preserve as much as possible the original CSS.
// 1. Determine the original index of the targeted dimension, as the one that is found in the original values array
// 2. Extract the left and right portions of the dimensions from the original values, as they did not change
// 3. Calculate the "mid chunk" portion of the dimensions, which was affected by the resize.
//    If the target is part of a repeated dimension, derive the final repeat so that the repeat statement itself
//    is not denormalized but updated to repeat with the new dimensions
// 4. Finally build the new dimensions as the concatenation of the left chunk, mid chunk, and right chunk.
function buildResizedDimensionsArray(params: {
  expandedOriginalValues: ExpandedGridDimension[]
  originalValues: GridDimension[]
  resizedValues: GridDimension[]
  resizedIndex: number
}): GridDimension[] {
  // 1.
  const targetIndex = params.expandedOriginalValues[params.resizedIndex].originalIndex
  const targetDimension = params.originalValues[targetIndex]

  // 2.
  const leftChunk = params.originalValues.slice(0, targetIndex)
  const rightChunk = params.originalValues.slice(targetIndex + 1)

  // 3.
  // midChunkStart is the index in the expanded values matching the first occurrence of the target index in the
  // expanded values.
  const midChunkStart = params.expandedOriginalValues.findIndex(
    (c) => c.originalIndex === targetIndex,
  )
  // midChunkEnd is the start + the number of repeated elements for this `repeat` function
  const midChunkEnd =
    midChunkStart +
    params.expandedOriginalValues.filter((c) => c.originalIndex === targetIndex).length
  const midChunkSize = midChunkEnd - midChunkStart

  let midChunk: GridDimension[] = []
  if (targetDimension.type === 'REPEAT') {
    const groupSize = midChunkSize / targetDimension.times
    const midChunkValues = params.resizedValues.slice(midChunkStart, midChunkStart + groupSize)
    const targetChunkIndex = (params.resizedIndex - midChunkStart) % groupSize // the index *relative to inside* the repeat group

    const resizedValue = params.resizedValues[params.resizedIndex]
    const values = midChunkValues.map((v, index) => {
      const valueIsRepeated = index % (groupSize * targetDimension.times) === targetChunkIndex
      return valueIsRepeated ? resizedValue : v
    })
    midChunk.push(gridCSSRepeat(targetDimension.times, values))
  } else {
    midChunk.push(...params.resizedValues.slice(midChunkStart, midChunkEnd))
  }

  // 4.
  return [...leftChunk, ...midChunk, ...rightChunk]
}

function getNewDragValue(
  dragAmount: number,
  isFractional: boolean,
  possibleCalculatedValue: Either<string, number>,
  mergedValue: Either<string, number>,
) {
  if (!isFractional) {
    return dragAmount
  }

  if (!isRight(possibleCalculatedValue)) {
    return 0
  }

  const mergedFractionalValue = foldEither(
    () => 0,
    (r) => r,
    mergedValue,
  )
  const calculatedValue = possibleCalculatedValue.value
  const perPointOne =
    mergedFractionalValue == 0 ? 10 : (calculatedValue / mergedFractionalValue) * 0.1
  const newValue = roundToNearestWhole((dragAmount / perPointOne) * 10) / 10
  return newValue
}

function newResizedValue(
  current: number,
  increment: number,
  precision: 'coarse' | 'precise',
  isFractional: boolean,
): number {
  const newValue = current + increment
  if (precision === 'precise') {
    return newValue
  } else if (isFractional) {
    // .5x steps
    return Math.round(newValue * 2) / 2
  } else {
    // 10x steps
    return Math.round(newValue / 10) * 10
  }
}
