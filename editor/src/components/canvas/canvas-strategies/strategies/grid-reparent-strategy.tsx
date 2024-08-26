import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import * as PP from '../../../../core/shared/property-path'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { CSSCursor } from '../../canvas-types'
import { setCursorCommand } from '../../commands/set-cursor-command'
import {
  propertyToDelete,
  propertyToSet,
  updateBulkProperties,
} from '../../commands/set-property-command'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import { ParentBounds } from '../../controls/parent-bounds'
import { ParentOutlines } from '../../controls/parent-outlines'
import { ZeroSizedElementControls } from '../../controls/zero-sized-element-controls'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import type {
  CanvasStrategy,
  CustomStrategyState,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession, UpdatedPathMap } from '../interaction-state'
import { honoursPropsPosition, shouldKeepMovingDraggedGroupChildren } from './absolute-utils'
import { replaceFragmentLikePathsWithTheirChildrenRecursive } from './fragment-like-helpers'
import { ifAllowedToReparent, isAllowedToReparent } from './reparent-helpers/reparent-helpers'
import type { ReparentTarget } from './reparent-helpers/reparent-strategy-helpers'
import { getReparentOutcome, pathToReparent } from './reparent-utils'
import { flattenSelection } from './shared-move-strategies-helpers'
import { isInfinityRectangle } from '../../../../core/shared/math-utils'
import { showGridControls } from '../../commands/show-grid-controls-command'
import { GridControls } from '../../controls/grid-controls'

export function gridReparentStrategy(
  reparentTarget: ReparentTarget,
  fitness: number,
  customStrategyState: CustomStrategyState,
): CanvasStrategyFactory {
  return (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
  ): CanvasStrategy | null => {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (
      selectedElements.length === 0 ||
      interactionSession == null ||
      interactionSession.interactionData.type !== 'DRAG'
    ) {
      return null
    }

    const dragInteractionData = interactionSession.interactionData
    const filteredSelectedElements = flattenSelection(selectedElements)
    const isApplicable = replaceFragmentLikePathsWithTheirChildrenRecursive(
      canvasState.startingMetadata,
      canvasState.startingAllElementProps,
      canvasState.startingElementPathTree,
      filteredSelectedElements,
    ).every((element) => {
      return honoursPropsPosition(canvasState, element)
    })
    if (!isApplicable) {
      return null
    }
    return {
      id: `GRID_REPARENT`,
      name: `Reparent (Grid)`,
      descriptiveLabel: 'Reparent (Grid)',
      icon: {
        category: 'modalities',
        type: 'reparent-large',
      },
      controlsToRender: [
        controlWithProps({
          control: ParentOutlines,
          props: { targetParent: reparentTarget.newParent.intendedParentPath },
          key: 'parent-outlines-control',
          show: 'visible-only-while-active',
        }),
        controlWithProps({
          control: ParentBounds,
          props: { targetParent: reparentTarget.newParent.intendedParentPath },
          key: 'parent-bounds-control',
          show: 'visible-only-while-active',
        }),
        controlWithProps({
          control: ZeroSizedElementControls,
          props: { showAllPossibleElements: true },
          key: 'zero-size-control',
          show: 'visible-only-while-active',
        }),
        {
          control: GridControls,
          props: { targets: [reparentTarget.newParent.intendedParentPath] },
          key: `draw-into-grid-strategy-controls`,
          show: 'always-visible',
          priority: 'bottom',
        },
      ],
      fitness: shouldKeepMovingDraggedGroupChildren(
        canvasState.startingMetadata,
        selectedElements,
        reparentTarget.newParent,
      )
        ? 1
        : fitness,
      apply: () => {
        const { projectContents, nodeModules } = canvasState
        const newParent = reparentTarget.newParent
        return ifAllowedToReparent(
          canvasState,
          canvasState.startingMetadata,
          filteredSelectedElements,
          newParent.intendedParentPath,
          () => {
            if (dragInteractionData.drag == null) {
              return emptyStrategyApplicationResult
            }

            const allowedToReparent = filteredSelectedElements.every((selectedElement) => {
              return isAllowedToReparent(
                canvasState.projectContents,
                canvasState.startingMetadata,
                selectedElement,
                newParent.intendedParentPath,
              )
            })

            if (!(reparentTarget.shouldReparent && allowedToReparent)) {
              return emptyStrategyApplicationResult
            }
            const outcomes = mapDropNulls((selectedElement) => {
              const reparentResult = getReparentOutcome(
                canvasState.startingMetadata,
                canvasState.startingElementPathTree,
                canvasState.startingAllElementProps,
                canvasState.builtInDependencies,
                projectContents,
                nodeModules,
                pathToReparent(selectedElement),
                newParent,
                'always',
                null,
              )

              if (reparentResult == null) {
                return null
              }

              const { commands: reparentCommands, newPath } = reparentResult

              const gridFrame = MetadataUtils.findElementByElementPath(
                canvasState.startingMetadata,
                reparentTarget.newParent.intendedParentPath,
              )?.globalFrame
              if (gridFrame == null || isInfinityRectangle(gridFrame)) {
                return null
              }

              const gridContainerCommands = updateBulkProperties(
                'mid-interaction',
                reparentTarget.newParent.intendedParentPath,
                [
                  propertyToSet(PP.create('style', 'width'), gridFrame.width),
                  propertyToSet(PP.create('style', 'height'), gridFrame.height),
                ],
              )
              const gridCellCommands = updateBulkProperties('always', newPath, [
                propertyToDelete(PP.create('style', 'position')),
                propertyToDelete(PP.create('style', 'top')),
                propertyToDelete(PP.create('style', 'left')),
                propertyToDelete(PP.create('style', 'bottom')),
                propertyToDelete(PP.create('style', 'right')),
              ])

              return {
                commands: [...reparentCommands, gridContainerCommands, gridCellCommands],
                newPath: newPath,
                oldPath: selectedElement,
              }
            }, selectedElements)

            let newPaths: Array<ElementPath> = []
            let updatedTargetPaths: UpdatedPathMap = {}

            outcomes.forEach((c) => {
              newPaths.push(c.newPath)
              updatedTargetPaths[EP.toString(c.oldPath)] = c.newPath
            })

            const elementsToRerender = EP.uniqueElementPaths([
              ...customStrategyState.elementsToRerender,
              ...newPaths,
              ...newPaths.map(EP.parentPath),
              ...filteredSelectedElements.map(EP.parentPath),
            ])
            return strategyApplicationResult(
              [
                ...outcomes.flatMap((c) => c.commands),
                updateSelectedViews('always', newPaths),
                setCursorCommand(CSSCursor.Reparent),
                showGridControls('mid-interaction', reparentTarget.newParent.intendedParentPath),
              ],
              {
                elementsToRerender,
              },
            )
          },
        )
      },
    }
  }
}
