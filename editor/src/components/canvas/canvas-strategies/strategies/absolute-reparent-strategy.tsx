import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { CSSCursor } from '../../canvas-types'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
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
  defaultCustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession, UpdatedPathMap } from '../interaction-state'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { honoursPropsPosition, shouldKeepMovingDraggedGroupChildren } from './absolute-utils'
import { replaceFragmentLikePathsWithTheirChildrenRecursive } from './fragment-like-helpers'
import { ifAllowedToReparent, isAllowedToReparent } from './reparent-helpers/reparent-helpers'
import { getAbsoluteReparentPropertyChanges } from './reparent-helpers/reparent-property-changes'
import type { ReparentTarget } from './reparent-helpers/reparent-strategy-helpers'
import { getReparentOutcome, pathToReparent } from './reparent-utils'
import { flattenSelection } from './shared-move-strategies-helpers'

export function baseAbsoluteReparentStrategy(
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

    const dragInteractionData = interactionSession.interactionData // Why TypeScript?!
    const filteredSelectedElements = flattenSelection(selectedElements)
    const isApplicable = replaceFragmentLikePathsWithTheirChildrenRecursive(
      canvasState.startingMetadata,
      canvasState.startingAllElementProps,
      canvasState.startingElementPathTree,
      filteredSelectedElements,
    ).every((element) => {
      const elementMetadata = MetadataUtils.findElementByElementPath(
        canvasState.startingMetadata,
        element,
      )

      return (
        elementMetadata?.specialSizeMeasurements.position === 'absolute' &&
        honoursPropsPosition(canvasState, element)
      )
    })
    if (!isApplicable) {
      return null
    }
    return {
      id: `ABSOLUTE_REPARENT`,
      name: `Reparent (Abs)`,
      descriptiveLabel: 'Reparenting Absolute Elements',
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
      ],
      fitness: shouldKeepMovingDraggedGroupChildren(
        canvasState.startingMetadata,
        selectedElements,
        reparentTarget.newParent,
      )
        ? 1
        : fitness,
      apply: (strategyLifecycle) => {
        const { projectContents, openFile, nodeModules } = canvasState
        return ifAllowedToReparent(
          canvasState,
          canvasState.startingMetadata,
          filteredSelectedElements,
          () => {
            if (dragInteractionData.drag == null) {
              return emptyStrategyApplicationResult
            }

            const newParent = reparentTarget.newParent
            const allowedToReparent = filteredSelectedElements.every((selectedElement) => {
              return isAllowedToReparent(
                canvasState.projectContents,
                canvasState.startingMetadata,
                selectedElement,
              )
            })

            if (reparentTarget.shouldReparent && allowedToReparent) {
              const commands = mapDropNulls((selectedElement) => {
                const reparentResult = getReparentOutcome(
                  canvasState.startingMetadata,
                  canvasState.startingElementPathTree,
                  canvasState.startingAllElementProps,
                  canvasState.builtInDependencies,
                  projectContents,
                  nodeModules,
                  openFile,
                  pathToReparent(selectedElement),
                  newParent,
                  'always',
                  null,
                )

                if (reparentResult == null) {
                  return null
                } else {
                  const offsetCommands = replaceFragmentLikePathsWithTheirChildrenRecursive(
                    canvasState.startingMetadata,
                    canvasState.startingAllElementProps,
                    canvasState.startingElementPathTree,
                    [selectedElement],
                  ).flatMap((target) => {
                    return getAbsoluteReparentPropertyChanges(
                      target,
                      newParent.intendedParentPath,
                      canvasState.startingMetadata,
                      canvasState.startingMetadata,
                      canvasState.projectContents,
                    )
                  })

                  const { commands: reparentCommands, newPath } = reparentResult
                  return {
                    oldPath: selectedElement,
                    newPath: newPath,
                    commands: [...offsetCommands, ...reparentCommands],
                  }
                }
              }, filteredSelectedElements)

              let newPaths: Array<ElementPath> = []
              let updatedTargetPaths: UpdatedPathMap = {}

              commands.forEach((c) => {
                newPaths.push(c.newPath)
                updatedTargetPaths[EP.toString(c.oldPath)] = c.newPath
              })

              const moveCommands =
                absoluteMoveStrategy(
                  canvasState,
                  {
                    ...interactionSession,
                    updatedTargetPaths: updatedTargetPaths,
                  },
                  defaultCustomStrategyState(),
                )?.strategy.apply(strategyLifecycle).commands ?? []

              const elementsToRerender = EP.uniqueElementPaths([
                ...customStrategyState.elementsToRerender,
                ...newPaths,
                ...newPaths.map(EP.parentPath),
                ...filteredSelectedElements.map(EP.parentPath),
              ])
              return strategyApplicationResult(
                [
                  ...moveCommands,
                  ...commands.flatMap((c) => c.commands),
                  updateSelectedViews('always', newPaths),
                  setElementsToRerenderCommand(elementsToRerender),
                  setCursorCommand(CSSCursor.Move),
                ],
                {
                  elementsToRerender,
                },
              )
            } else {
              const moveCommands =
                absoluteMoveStrategy(
                  canvasState,
                  interactionSession,
                  defaultCustomStrategyState(),
                )?.strategy.apply(strategyLifecycle).commands ?? []
              return strategyApplicationResult(moveCommands)
            }
          },
        )
      },
    }
  }
}
