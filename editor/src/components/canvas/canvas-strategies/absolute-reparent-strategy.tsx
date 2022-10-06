import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { CSSCursor } from '../canvas-types'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { updateSelectedViews } from '../commands/update-selected-views-command'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import {
  CanvasStrategy,
  controlWithProps,
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
} from './canvas-strategy-types'
import { getDragTargets } from './shared-move-strategies-helpers'
import { ifAllowedToReparent, isAllowedToReparent } from './reparent-helpers'
import {
  existingReparentSubjects,
  getAbsoluteReparentPropertyChanges,
  getFitnessForReparentStrategy,
  getReparentTargetUnified,
} from './reparent-strategy-helpers'
import { offsetPoint } from '../../../core/shared/math-utils'
import { getReparentOutcome, pathToReparent } from './reparent-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { honoursPropsPosition } from './absolute-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { InteractionSession, MissingBoundsHandling, UpdatedPathMap } from './interaction-state'

function baseAbsoluteReparentStrategy(
  id: 'ABSOLUTE_REPARENT' | 'FORCED_ABSOLUTE_REPARENT',
  name: string,
  missingBoundsHandling: MissingBoundsHandling,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (
    selectedElements.length > 0 &&
    interactionSession != null &&
    interactionSession.interactionData.type === 'DRAG'
  ) {
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

    if (isApplicable) {
      return {
        id: id,
        name: name,
        controlsToRender: [
          controlWithProps({
            control: ParentOutlines,
            props: {},
            key: 'parent-outlines-control',
            show: 'visible-only-while-active',
          }),
          controlWithProps({
            control: ParentBounds,
            props: {},
            key: 'parent-bounds-control',
            show: 'visible-only-while-active',
          }),
        ],
        fitness: getFitnessForReparentStrategy(
          'ABSOLUTE_REPARENT_TO_ABSOLUTE',
          canvasState,
          interactionSession,
          missingBoundsHandling,
        ),
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

              const pointOnCanvas = offsetPoint(
                dragInteractionData.originalDragStart,
                dragInteractionData.drag,
              )

              const reparentTarget = getReparentTargetUnified(
                existingReparentSubjects(filteredSelectedElements),
                pointOnCanvas,
                dragInteractionData.modifiers.cmd,
                canvasState,
                canvasState.startingMetadata,
                canvasState.startingAllElementProps,
                missingBoundsHandling,
              )
              const newParent = reparentTarget.newParent
              const allowedToReparent = filteredSelectedElements.every((selectedElement) => {
                return isAllowedToReparent(
                  canvasState.projectContents,
                  canvasState.openFile,
                  canvasState.startingMetadata,
                  selectedElement,
                )
              })

              if (reparentTarget.shouldReparent && newParent != null && allowedToReparent) {
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

  return null
}

export const absoluteReparentStrategy = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null =>
  baseAbsoluteReparentStrategy(
    'ABSOLUTE_REPARENT',
    'Reparent (Abs)',
    'use-strict-bounds',
    canvasState,
    interactionSession,
  )

export const forcedAbsoluteReparentStrategy = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null =>
  baseAbsoluteReparentStrategy(
    'FORCED_ABSOLUTE_REPARENT',
    'Reparent (Abs, Force)',
    'allow-missing-bounds',
    canvasState,
    interactionSession,
  )
