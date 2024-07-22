import { fromArrayIndex, fromField, notNull } from '../../../../core/shared/optics/optic-creators'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import * as PP from '../../../../core/shared/property-path'
import { setProperty } from '../../commands/set-property-command'
import { GridControls } from '../../controls/grid-controls'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { InteractionCanvasState } from '../canvas-strategy-types'
import {
  getTargetPathsFromInteractionTarget,
  emptyStrategyApplicationResult,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import type { GridCSSNumber } from '../../../../components/inspector/common/css-utils'
import { printArrayCSSNumber } from '../../../../components/inspector/common/css-utils'
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
        control: GridControls,
        props: {},
        key: `grid-controls-${EP.toString(selectedElement)}`,
        show: 'always-visible',
      },
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

      const gridPath = isGrid ? selectedElement : EP.parentPath(selectedElement)

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

      const mergedValues: GridAutoOrTemplateBase = {
        type: calculatedValues.type,
        dimensions: calculatedValues.dimensions.map((dim, index) => {
          if (index < originalValues.dimensions.length) {
            return originalValues.dimensions[index]
          }
          return dim
        }),
      }

      const unitOptic = fromArrayIndex<GridCSSNumber>(control.columnOrRow)
        .compose(fromField('unit'))
        .compose(notNull())
      const valueOptic = fromArrayIndex<GridCSSNumber>(control.columnOrRow).compose(
        fromField('value'),
      )

      const calculatedValue = toFirst(valueOptic, calculatedValues.dimensions)
      const mergedValue = toFirst(valueOptic, mergedValues.dimensions)
      const mergedUnit = toFirst(unitOptic, mergedValues.dimensions)
      const isFractional = isRight(mergedUnit) && mergedUnit.value === 'fr'
      const precision = modifiers.cmd ? 'coarse' : 'precise'

      const newSetting = modify(
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
      const propertyValueAsString = printArrayCSSNumber(newSetting)

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
