import {
  fromArrayIndex,
  fromField,
  fromTypeGuard,
  notNull,
} from '../../../../core/shared/optics/optic-creators'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import * as PP from '../../../../core/shared/property-path'
import type { PropertyToUpdate } from '../../commands/set-property-command'
import {
  propertyToDelete,
  propertyToSet,
  updateBulkProperties,
} from '../../commands/set-property-command'
import {
  controlsForGridPlaceholders,
  GridRowColumnResizingControls,
} from '../../controls/grid-controls-for-strategies'
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
  cssNumber,
  gridCSSNumber,
  isDynamicGridRepeat,
  isGridCSSNumber,
  printArrayGridDimensions,
} from '../../../../components/inspector/common/css-utils'
import { toFirst } from '../../../../core/shared/optics/optic-utilities'
import { isLeft } from '../../../../core/shared/either'
import { roundToNearestWhole } from '../../../../core/shared/math-utils'
import type { GridAutoOrTemplateBase } from '../../../../core/shared/element-template'
import {
  expandGridDimensions,
  findOriginalGrid,
  isJustAutoGridDimension,
  replaceGridTemplateDimensionAtIndex,
} from './grid-helpers'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { CSSCursor } from '../../canvas-types'
import type { CanvasCommand } from '../../commands/commands'
import type { Axis } from '../../gap-utils'
import { getComponentDescriptorForTarget } from '../../../../core/property-controls/property-controls-utils'

export const resizeGridStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
) => {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }

  const selectedElement = selectedElements[0]
  const selectedElementMetadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    selectedElement,
  )
  if (selectedElementMetadata == null) {
    return null
  }

  const supportsStyleProp = MetadataUtils.targetRegisteredStyleControlsOrHonoursStyleProps(
    canvasState.projectContents,
    selectedElementMetadata,
    canvasState.propertyControlsInfo,
    'layout-system',
    ['gridTemplateColumns', 'gridTemplateRows'],
    'some',
  )

  const isGridCell = MetadataUtils.isGridItem(canvasState.startingMetadata, selectedElement)
  const isGrid = MetadataUtils.isGridLayoutedContainer(
    MetadataUtils.findElementByElementPath(canvasState.startingMetadata, selectedElement),
  )
  const isGridOrGridCell = isGridCell || isGrid

  if (!isGridOrGridCell) {
    return null
  }

  const gridPath = isGrid
    ? selectedElement
    : findOriginalGrid(canvasState.startingMetadata, EP.parentPath(selectedElement)) // TODO don't use EP.parentPath
  if (gridPath == null) {
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
        control: GridRowColumnResizingControls,
        props: { target: gridPath },
        key: `grid-row-col-resize-controls-${EP.toString(gridPath)}`,
        show: 'always-visible',
      },
      controlsForGridPlaceholders(gridPath),
    ],
    fitness: supportsStyleProp
      ? onlyFitWhenDraggingThisControl(interactionSession, 'GRID_AXIS_HANDLE', 1)
      : 0,
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

      const otherAxis: Axis = control.axis === 'column' ? 'row' : 'column'
      const otherAxisValues =
        otherAxis === 'column'
          ? gridSpecialSizeMeasurements.containerGridPropertiesFromProps.gridTemplateColumns
          : gridSpecialSizeMeasurements.containerGridPropertiesFromProps.gridTemplateRows

      if (
        calculatedValues == null ||
        calculatedValues.type !== 'DIMENSIONS' ||
        originalValues == null ||
        originalValues.type !== 'DIMENSIONS'
      ) {
        return emptyStrategyApplicationResult
      }

      if (!canResizeGridTemplate(originalValues)) {
        return strategyApplicationResult([setCursorCommand(CSSCursor.NotPermitted)], [])
      }

      const expandedOriginalValues = expandGridDimensions(originalValues.dimensions)
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
      if (isLeft(calculatedValue)) {
        return emptyStrategyApplicationResult
      }
      const mergedValue = toFirst(valueOptic, mergedValues.dimensions)
      if (isLeft(mergedValue)) {
        return emptyStrategyApplicationResult
      }
      const mergedUnit = toFirst(unitOptic, mergedValues.dimensions)
      if (isLeft(mergedUnit)) {
        return emptyStrategyApplicationResult
      }

      const isFractional = mergedUnit.value === 'fr'
      const precision = modifiers.cmd ? 'coarse' : 'precise'
      const lineName = mergedValues.dimensions[control.columnOrRow]?.lineName ?? null

      const newValue = Math.max(
        0,
        newResizedValue(
          mergedValue.value,
          getNewDragValue(dragAmount, isFractional, calculatedValue.value, mergedValue.value),
          precision,
          isFractional,
        ),
      )

      const newDimensions = replaceGridTemplateDimensionAtIndex(
        originalValues.dimensions,
        expandedOriginalValues,
        control.columnOrRow,
        gridCSSNumber(cssNumber(newValue, mergedUnit.value), lineName),
      )

      const propertyValueAsString = printArrayGridDimensions(newDimensions)

      const propertyAxis = PP.create(
        'style',
        control.axis === 'column' ? 'gridTemplateColumns' : 'gridTemplateRows',
      )
      let propertiesToUpdate: PropertyToUpdate[] = [
        propertyToSet(propertyAxis, propertyValueAsString),
        propertyToDelete(PP.create('style', 'gridTemplate')), // delete the shorthand in favor of the longhands
      ]

      if (
        otherAxisValues?.type === 'DIMENSIONS' &&
        otherAxisValues.dimensions.length > 0 &&
        !isJustAutoGridDimension(otherAxisValues.dimensions)
      ) {
        // if the other axis has dimensions, serialize them in their longhand
        const propertyOtherAxis = PP.create(
          'style',
          otherAxis === 'column' ? 'gridTemplateColumns' : 'gridTemplateRows',
        )
        const otherAxisValueAsString = printArrayGridDimensions(otherAxisValues.dimensions)
        propertiesToUpdate.push(propertyToSet(propertyOtherAxis, otherAxisValueAsString))
      }

      let commands: CanvasCommand[] = [
        updateBulkProperties('always', gridPath, propertiesToUpdate),
        setCursorCommand(control.axis === 'column' ? CSSCursor.ColResize : CSSCursor.RowResize),
      ]

      return strategyApplicationResult(commands, [gridPath])
    },
  }
}

function getNewDragValue(
  dragAmount: number,
  isFractional: boolean,
  calculatedValue: number,
  mergedValue: number,
): number {
  if (!isFractional) {
    return dragAmount
  }

  if (calculatedValue === 0) {
    return 0
  }

  // for fr units, adjust the value to proportionally to .1
  let proportionalResize = calculatedValue * 0.1
  if (mergedValue !== 0) {
    proportionalResize /= mergedValue
  }

  return roundToNearestWhole(dragAmount / proportionalResize) * 0.1
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

export function canResizeGridTemplate(template: GridAutoOrTemplateBase): boolean {
  return template.type === 'DIMENSIONS' && !template.dimensions.some(isDynamicGridRepeat)
}
