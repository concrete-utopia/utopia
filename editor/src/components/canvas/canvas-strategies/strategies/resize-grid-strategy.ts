import {
  filtered,
  fromArrayIndex,
  fromField,
  notNull,
} from '../../../../core/shared/optics/optic-creators'
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
import type { CSSNumber, GridCSSNumber } from '../../../../components/inspector/common/css-utils'
import { printArrayCSSNumber } from '../../../../components/inspector/common/css-utils'
import { any, anyBy, modify, toFirst } from '../../../../core/shared/optics/optic-utilities'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { isRight } from '../../../../core/shared/either'
import { roundToNearestWhole } from '../../../../core/shared/math-utils'

export const resizeGridStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
) => {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }

  const selectedElement = selectedElements[0]
  const parentPath = EP.parentPath(selectedElement)
  const ok = MetadataUtils.isGridLayoutedContainer(
    MetadataUtils.findElementByElementPath(canvasState.startingMetadata, parentPath),
  )
  if (!ok) {
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
      const parentSpecialSizeMeasurements =
        canvasState.startingMetadata[EP.toString(parentPath)].specialSizeMeasurements
      const originalValues =
        control.axis === 'column'
          ? parentSpecialSizeMeasurements.containerGridPropertiesFromProps.gridTemplateColumns
          : parentSpecialSizeMeasurements.containerGridPropertiesFromProps.gridTemplateRows
      const calculatedValues =
        control.axis === 'column'
          ? parentSpecialSizeMeasurements.containerGridProperties.gridTemplateColumns
          : parentSpecialSizeMeasurements.containerGridProperties.gridTemplateRows

      if (
        calculatedValues == null ||
        calculatedValues.type !== 'DIMENSIONS' ||
        originalValues == null ||
        originalValues.type !== 'DIMENSIONS'
      ) {
        return emptyStrategyApplicationResult
      }
      const unitOptic = fromArrayIndex<GridCSSNumber>(control.columnOrRow)
        .compose(fromField('unit'))
        .compose(notNull())
      const valueOptic = fromArrayIndex<GridCSSNumber>(control.columnOrRow).compose(
        fromField('value'),
      )
      const isFractional = anyBy(unitOptic, (unit) => unit === 'fr', originalValues.dimensions)
      let newSetting: Array<GridCSSNumber>
      const originalDimensions = originalValues.dimensions
      if (isFractional) {
        const possibleOriginalFractionalValue = toFirst(valueOptic, originalValues.dimensions)
        const possibleCalculatedValue = toFirst(valueOptic, calculatedValues.dimensions)
        if (isRight(possibleOriginalFractionalValue) && isRight(possibleCalculatedValue)) {
          const originalFractionalValue = possibleOriginalFractionalValue.value
          const calculatedValue = possibleCalculatedValue.value
          const perPointOne =
            originalFractionalValue == 0 ? 10 : (calculatedValue / originalFractionalValue) * 0.1
          const newValue = roundToNearestWhole((dragAmount / perPointOne) * 10) / 10
          newSetting = modify(valueOptic, (current) => current + newValue, originalDimensions)
        } else {
          throw new Error(`Somehow we cannot identify the right dimensions.`)
        }
      } else {
        newSetting = modify(valueOptic, (current) => current + dragAmount, originalDimensions)
      }
      const propertyValueAsString = printArrayCSSNumber(newSetting)

      const commands = [
        setProperty(
          'always',
          parentPath,
          PP.create(
            'style',
            control.axis === 'column' ? 'gridTemplateColumns' : 'gridTemplateRows',
          ),
          propertyValueAsString,
        ),
        setElementsToRerenderCommand([parentPath]),
      ]

      return strategyApplicationResult(commands)
    },
  }
}
