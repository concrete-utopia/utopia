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
import type { Either } from '../../../../core/shared/either'
import { foldEither, isLeft, isRight } from '../../../../core/shared/either'
import { roundTo, roundToNearestWhole } from '../../../../core/shared/math-utils'
import type { GridAutoOrTemplateBase } from '../../../../core/shared/element-template'
import {
  expandGridDimensions,
  findOriginalGrid,
  replaceGridTemplateDimensionAtIndex,
} from './grid-helpers'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { CSSCursor } from '../../canvas-types'

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
  const metadata = interactionSession?.latestMetadata ?? canvasState.startingMetadata
  const originalGridPath = findOriginalGrid(metadata, gridPath)
  if (originalGridPath == null) {
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
        canvasState.startingMetadata[EP.toString(originalGridPath)].specialSizeMeasurements

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

      const commands = [
        setProperty(
          'always',
          originalGridPath,
          PP.create(
            'style',
            control.axis === 'column' ? 'gridTemplateColumns' : 'gridTemplateRows',
          ),
          propertyValueAsString,
        ),
        setCursorCommand(control.axis === 'column' ? CSSCursor.ColResize : CSSCursor.RowResize),
      ]

      return strategyApplicationResult(commands, [originalGridPath])
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
