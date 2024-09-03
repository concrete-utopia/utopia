import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { GridControls, GridControlsKey } from '../../controls/grid-controls'
import type {
  ElementInstanceMetadataMap,
  GridAutoOrTemplateBase,
} from '../../../../core/shared/element-template'
import * as PP from '../../../../core/shared/property-path'
import { printGridAutoOrTemplateBase } from '../../../inspector/common/css-utils'
import type { PropertyToUpdate } from '../../commands/set-property-command'
import {
  propertyToDelete,
  propertyToSet,
  updateBulkProperties,
} from '../../commands/set-property-command'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type {
  ControlWithProps,
  CustomStrategyState,
  CustomStrategyStatePatch,
  InteractionCanvasState,
  InteractionLifecycle,
} from '../canvas-strategy-types'
import {
  getTargetPathsFromInteractionTarget,
  emptyStrategyApplicationResult,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { DragInteractionData, InteractionSession } from '../interaction-state'
import { runGridRearrangeMove } from './grid-helpers'
import type { CanvasRectangle } from '../../../../core/shared/math-utils'
import { isInfinityRectangle, offsetPoint } from '../../../../core/shared/math-utils'
import { findReparentStrategies } from './reparent-helpers/reparent-strategy-helpers'
import { applyAbsoluteReparent, controlsForAbsoluteReparent } from './absolute-reparent-strategy'
import type { CanvasCommand } from '../../commands/commands'
import { applyStaticReparent, controlsForStaticReparent } from './reparent-as-static-strategy'
import type { FindReparentStrategyResult } from './reparent-helpers/reparent-strategy-parent-lookup'
import { applyGridReparent, controlsForGridReparent } from './grid-reparent-strategy'
import { assertNever } from '../../../../core/shared/utils'

export const gridRearrangeMoveStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customState: CustomStrategyState,
) => {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (
    selectedElements.length === 0 ||
    interactionSession == null ||
    interactionSession.interactionData.type !== 'DRAG' ||
    interactionSession.interactionData.drag == null ||
    interactionSession.activeControl.type !== 'GRID_CELL_HANDLE' ||
    interactionSession.interactionData.modifiers.alt
  ) {
    return null
  }

  const selectedElement = selectedElements[0]
  if (!MetadataUtils.isGridCell(canvasState.startingMetadata, selectedElement)) {
    return null
  }

  const parentGridPath = EP.parentPath(selectedElement)
  const gridFrame = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    parentGridPath,
  )?.globalFrame
  if (gridFrame == null || isInfinityRectangle(gridFrame)) {
    return null
  }

  const initialTemplates = getGridTemplates(canvasState.startingMetadata, parentGridPath)
  if (initialTemplates == null) {
    return null
  }

  const strategyToApply = getStrategyToApply(
    canvasState,
    interactionSession.interactionData,
    parentGridPath,
  )
  if (strategyToApply == null) {
    return null
  }

  return {
    id: 'rearrange-grid-move-strategy',
    name: strategyToApply.name,
    descriptiveLabel: strategyToApply.name,
    icon: {
      category: 'tools',
      type: 'pointer',
    },
    controlsToRender: strategyToApply.controlsToRender,
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'GRID_CELL_HANDLE', 2),
    apply: (strategyLifecycle) => {
      if (
        interactionSession == null ||
        interactionSession.interactionData.type !== 'DRAG' ||
        interactionSession.interactionData.drag == null ||
        interactionSession.activeControl.type !== 'GRID_CELL_HANDLE'
      ) {
        return emptyStrategyApplicationResult
      }

      const midInteractionCommands = [
        // during the interaction, freeze the template with the calculated values…
        updateBulkProperties('mid-interaction', parentGridPath, [
          propertyToSet(
            PP.create('style', 'gridTemplateColumns'),
            printGridAutoOrTemplateBase(initialTemplates.calculated.columns),
          ),
          propertyToSet(
            PP.create('style', 'gridTemplateRows'),
            printGridAutoOrTemplateBase(initialTemplates.calculated.rows),
          ),
        ]),
      ]

      const onCompleteCommands = [
        // …eventually, restore the grid template on complete.
        updateBulkProperties(
          'on-complete',
          parentGridPath,
          restoreGridTemplateFromProps(initialTemplates.fromProps),
        ),
      ]

      const { commands, patch } =
        strategyToApply.type === 'GRID_REARRANGE'
          ? getCommandsAndPatchForGridRearrange(
              canvasState,
              interactionSession,
              customState,
              selectedElement,
              strategyLifecycle,
            )
          : getCommandsAndPatchForReparent(
              strategyToApply.strategy,
              canvasState,
              interactionSession.interactionData,
              interactionSession,
              customState,
              selectedElement,
              strategyLifecycle,
              gridFrame,
            )

      if (commands.length === 0) {
        return emptyStrategyApplicationResult
      }

      return strategyApplicationResult(
        [...midInteractionCommands, ...onCompleteCommands, ...commands],
        patch,
      )
    },
  }
}

function getCommandsAndPatchForGridRearrange(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  customState: CustomStrategyState,
  selectedElement: ElementPath,
  strategyLifecycle: InteractionLifecycle,
): { commands: CanvasCommand[]; patch: CustomStrategyStatePatch } {
  const {
    commands,
    targetCell: targetGridCell,
    draggingFromCell,
    originalRootCell,
    targetRootCell,
  } = runGridRearrangeMove(
    selectedElement,
    selectedElement,
    canvasState,
    interactionSession,
    customState,
    strategyLifecycle,
    false,
  )

  return {
    commands: commands,
    patch: {
      grid: {
        targetCell: targetGridCell,
        draggingFromCell: draggingFromCell,
        originalRootCell: originalRootCell,
        currentRootCell: targetRootCell,
      },
    },
  }
}

function getCommandsAndPatchForReparent(
  strategy: FindReparentStrategyResult,
  canvasState: InteractionCanvasState,
  interactionData: DragInteractionData,
  interactionSession: InteractionSession,
  customState: CustomStrategyState,
  targetElement: ElementPath,
  strategyLifecycle: InteractionLifecycle,
  gridFrame: CanvasRectangle,
): { commands: CanvasCommand[]; patch: CustomStrategyStatePatch } {
  if (interactionData.drag == null) {
    return { commands: [], patch: {} }
  }

  function applyReparent() {
    switch (strategy.strategy) {
      case 'REPARENT_AS_ABSOLUTE':
        return applyAbsoluteReparent(
          canvasState,
          interactionSession,
          customState,
          strategy.target,
          [targetElement],
        )(strategyLifecycle)
      case 'REPARENT_AS_STATIC':
        return applyStaticReparent(canvasState, interactionSession, customState, strategy.target)
      case 'REPARENT_INTO_GRID':
        return applyGridReparent(
          canvasState,
          interactionData,
          customState,
          strategy.target,
          [targetElement],
          gridFrame,
        )()
      default:
        assertNever(strategy.strategy)
    }
  }
  const result = applyReparent()

  let commands: CanvasCommand[] = []

  const frame = MetadataUtils.getFrameOrZeroRect(targetElement, canvasState.startingMetadata)

  if (strategy.strategy === 'REPARENT_AS_ABSOLUTE') {
    // for absolute reparents, set positioning and size props
    commands.push(
      updateBulkProperties('always', targetElement, [
        propertyToSet(PP.create('style', 'position'), 'absolute'),
        propertyToSet(PP.create('style', 'top'), frame.y + interactionData.drag.y),
        propertyToSet(PP.create('style', 'left'), frame.x + interactionData.drag.x),
        propertyToSet(PP.create('style', 'width'), frame.width),
        propertyToSet(PP.create('style', 'height'), frame.height),
      ]),
    )
  } else if (strategy.strategy === 'REPARENT_AS_STATIC') {
    // for static reparents, set size props
    commands.push(
      updateBulkProperties('always', targetElement, [
        propertyToSet(PP.create('style', 'width'), frame.width),
        propertyToSet(PP.create('style', 'height'), frame.height),
      ]),
    )
  }

  // for absolute, static, or non-same-grid reparents, remove cell placement props
  if (
    strategy.strategy !== 'REPARENT_INTO_GRID' ||
    !EP.pathsEqual(strategy.target.newParent.intendedParentPath, EP.parentPath(targetElement))
  ) {
    commands.push(
      updateBulkProperties('on-complete', targetElement, [
        propertyToDelete(PP.create('style', 'gridRow')),
        propertyToDelete(PP.create('style', 'gridColumn')),
      ]),
    )
  }

  commands.push(...result.commands)

  return {
    commands: commands,
    patch: result.customStatePatch,
  }
}

function restoreGridTemplateFromProps(params: {
  columns: GridAutoOrTemplateBase
  rows: GridAutoOrTemplateBase
}): PropertyToUpdate[] {
  let properties: PropertyToUpdate[] = []
  const newCols = printGridAutoOrTemplateBase(params.columns)
  const newRows = printGridAutoOrTemplateBase(params.rows)
  if (newCols === '') {
    properties.push(propertyToDelete(PP.create('style', 'gridTemplateColumns')))
  } else {
    properties.push(propertyToSet(PP.create('style', 'gridTemplateColumns'), newCols))
  }
  if (newRows === '') {
    properties.push(propertyToDelete(PP.create('style', 'gridTemplateRows')))
  } else {
    properties.push(propertyToSet(PP.create('style', 'gridTemplateRows'), newRows))
  }
  return properties
}

function getGridTemplates(jsxMetadata: ElementInstanceMetadataMap, gridPath: ElementPath) {
  const grid = MetadataUtils.findElementByElementPath(jsxMetadata, gridPath)
  if (grid == null) {
    return null
  }

  const templateFromProps = grid.specialSizeMeasurements.containerGridPropertiesFromProps
  const templateRowsFromProps = templateFromProps.gridTemplateRows
  if (templateRowsFromProps == null) {
    return null
  }
  const templateColsFromProps = templateFromProps.gridTemplateColumns
  if (templateColsFromProps == null) {
    return null
  }

  const templateCalculated = grid.specialSizeMeasurements.containerGridProperties
  const templateRowsCalculated = templateCalculated.gridTemplateRows
  if (templateRowsCalculated == null) {
    return null
  }
  const templateColsCalculated = templateCalculated.gridTemplateColumns
  if (templateColsCalculated == null) {
    return null
  }

  return {
    calculated: {
      columns: templateColsCalculated,
      rows: templateRowsCalculated,
    },
    fromProps: {
      columns: templateColsFromProps,
      rows: templateRowsFromProps,
    },
  }
}

type StrategyToApply =
  | {
      type: 'GRID_REARRANGE'
      controlsToRender: ControlWithProps<any>[]
      name: string
    }
  | {
      type: 'REPARENT'
      controlsToRender: ControlWithProps<any>[]
      name: string
      strategy: FindReparentStrategyResult
    }

function getStrategyToApply(
  canvasState: InteractionCanvasState,
  interactionData: DragInteractionData,
  parentGridPath: ElementPath,
): StrategyToApply | null {
  if (interactionData.drag == null) {
    return null
  }

  const shouldReparent = interactionData.modifiers.cmd
  if (shouldReparent) {
    const pointOnCanvas = offsetPoint(interactionData.originalDragStart, interactionData.drag)
    const reparentStrategies = findReparentStrategies(
      canvasState,
      true,
      pointOnCanvas,
      'allow-smaller-parent',
    )

    const strategy = reparentStrategies[0]
    if (strategy != null) {
      switch (strategy.strategy) {
        case 'REPARENT_AS_ABSOLUTE':
          return {
            type: 'REPARENT',
            name: 'Reparent (Abs)',
            controlsToRender: controlsForAbsoluteReparent(strategy.target),
            strategy: strategy,
          }
        case 'REPARENT_AS_STATIC':
          return {
            type: 'REPARENT',
            name: 'Reparent (Flex)',
            controlsToRender: controlsForStaticReparent(strategy.target),
            strategy: strategy,
          }
        case 'REPARENT_INTO_GRID':
          return {
            type: 'REPARENT',
            name: 'Reparent (Grid)',
            controlsToRender: controlsForGridReparent(strategy.target),
            strategy: strategy,
          }
        default:
          assertNever(strategy.strategy)
      }
    }
  }

  return {
    type: 'GRID_REARRANGE',
    name: 'Rearrange Grid (Move)',
    controlsToRender: [
      {
        control: GridControls,
        props: { targets: [parentGridPath] },
        key: GridControlsKey(parentGridPath),
        show: 'always-visible',
        priority: 'bottom',
      },
    ],
  }
}
