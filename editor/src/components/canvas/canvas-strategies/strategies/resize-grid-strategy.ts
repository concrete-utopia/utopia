import { fromArrayIndex, fromField } from '../../../../core/shared/optics/optic-creators'
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

      const valueOptic = fromArrayIndex<GridCSSNumber>(control.columnOrRow).compose(
        fromField('value'),
      )

      const isFractional = mergedValues.dimensions.at(control.columnOrRow)?.unit === 'fr'

      let newSetting: Array<GridCSSNumber>
      if (isFractional) {
        const possibleCalculatedValue = toFirst(valueOptic, calculatedValues.dimensions)
        if (isRight(possibleCalculatedValue)) {
          const mergedFractionalValue = foldEither(
            () => 0,
            (r) => r,
            toFirst(valueOptic, mergedValues.dimensions),
          )
          const calculatedValue = possibleCalculatedValue.value
          const perPointOne =
            mergedFractionalValue == 0 ? 10 : (calculatedValue / mergedFractionalValue) * 0.1
          const newValue = roundToNearestWhole((dragAmount / perPointOne) * 10) / 10
          newSetting = modify(valueOptic, (current) => current + newValue, mergedValues.dimensions)
        } else {
          throw new Error(`Somehow we cannot identify the right dimensions.`)
        }
      } else {
        newSetting = modify(valueOptic, (current) => current + dragAmount, mergedValues.dimensions)
      }
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
