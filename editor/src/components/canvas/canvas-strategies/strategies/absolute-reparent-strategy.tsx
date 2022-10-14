import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { CSSCursor } from '../../canvas-types'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import { ParentBounds } from '../../controls/parent-bounds'
import { ParentOutlines } from '../../controls/parent-outlines'
import { CanvasStrategyFactory } from '../canvas-strategies'
import {
  CanvasStrategy,
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession, MissingBoundsHandling, UpdatedPathMap } from '../interaction-state'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { honoursPropsPosition } from './absolute-utils'
import { ifAllowedToReparent, isAllowedToReparent } from './reparent-helpers'
import { getAbsoluteReparentPropertyChanges, ReparentTarget } from './reparent-strategy-helpers'
import { getReparentOutcome, pathToReparent } from './reparent-utils'
import { getDragTargets } from './shared-move-strategies-helpers'

export function baseAbsoluteReparentStrategy(
  reparentTarget: ReparentTarget,
  missingBoundsHandling: MissingBoundsHandling,
  fitness: number,
): CanvasStrategyFactory {
  const forced = missingBoundsHandling === 'allow-missing-bounds'
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
    const filteredSelectedElements = getDragTargets(selectedElements)
    const isApplicable = filteredSelectedElements.every((element) => {
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
      id: `${forced ? 'FORCED_' : ''}ABSOLUTE_REPARENT`,
      name: `Reparent (Abs${forced ? ', Force' : ''})`,
      controlsToRender: [
        controlWithProps({
          control: ParentOutlines,
          props: { targetParent: reparentTarget.newParent },
          key: 'parent-outlines-control',
          show: 'visible-only-while-active',
        }),
        controlWithProps({
          control: ParentBounds,
          props: { targetParent: reparentTarget.newParent },
          key: 'parent-bounds-control',
          show: 'visible-only-while-active',
        }),
      ],
      fitness: fitness,
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
                canvasState.openFile,
                canvasState.startingMetadata,
                selectedElement,
              )
            })

            if (reparentTarget.shouldReparent && allowedToReparent) {
              const commands = mapDropNulls((selectedElement) => {
                const reparentResult = getReparentOutcome(
                  canvasState.builtInDependencies,
                  projectContents,
                  nodeModules,
                  openFile,
                  pathToReparent(selectedElement),
                  newParent,
                  'always',
                )

                if (reparentResult == null) {
                  return null
                } else {
                  const offsetCommands = getAbsoluteReparentPropertyChanges(
                    selectedElement,
                    newParent,
                    canvasState.startingMetadata,
                    canvasState.startingMetadata,
                    canvasState.projectContents,
                    canvasState.openFile,
                  )

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
                absoluteMoveStrategy(canvasState, {
                  ...interactionSession,
                  updatedTargetPaths: updatedTargetPaths,
                })?.apply(strategyLifecycle).commands ?? []

              return strategyApplicationResult([
                ...moveCommands,
                ...commands.flatMap((c) => c.commands),
                updateSelectedViews('always', newPaths),
                setElementsToRerenderCommand([...newPaths, ...filteredSelectedElements]),
                setCursorCommand('mid-interaction', CSSCursor.Move),
              ])
            } else {
              const moveCommands =
                absoluteMoveStrategy(canvasState, interactionSession)?.apply(strategyLifecycle)
                  .commands ?? []

              return strategyApplicationResult(moveCommands)
            }
          },
        )
      },
    }
  }
}
