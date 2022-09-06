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
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
} from './canvas-strategy-types'
import { getDragTargets } from './shared-absolute-move-strategy-helpers'
import { ifAllowedToReparent, isAllowedToReparent } from './reparent-helpers'
import {
  findReparentStrategy,
  getAbsoluteReparentPropertyChanges,
  getReparentTargetUnified,
} from './reparent-strategy-helpers'
import { offsetPoint } from '../../../core/shared/math-utils'
import { getReparentOutcome, pathToReparent } from './reparent-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { honoursPropsPosition } from './absolute-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { UpdatedPathMap } from './interaction-state'

export const absoluteReparentStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_REPARENT',
  name: 'Reparent Absolute Elements',
  isApplicable: (canvasState, interactionState, metadata) => {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (
      selectedElements.length > 0 &&
      interactionState != null &&
      interactionState.interactionData.type === 'DRAG'
    ) {
      const filteredSelectedElements = getDragTargets(selectedElements)
      return filteredSelectedElements.every((element) => {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)

        return (
          elementMetadata?.specialSizeMeasurements.position === 'absolute' &&
          honoursPropsPosition(canvasState, element)
        )
      })
    }
    return false
  },
  controlsToRender: [
    {
      control: ParentOutlines,
      key: 'parent-outlines-control',
      show: 'visible-only-while-active',
    },
    {
      control: ParentBounds,
      key: 'parent-bounds-control',
      show: 'visible-only-while-active',
    },
  ],
  fitness: (canvasState, interactionState, strategyState) => {
    // All 4 reparent strategies use the same fitness function findReparentStrategy
    const reparentStrategy = findReparentStrategy(
      canvasState,
      interactionState,
      strategyState,
      true,
    ).strategy
    if (reparentStrategy === 'ABSOLUTE_REPARENT_TO_ABSOLUTE') {
      return 3
    }
    return 0
  },
  apply: (canvasState, interactionState, strategyState) => {
    if (
      interactionState.interactionData.type != 'DRAG' ||
      interactionState.interactionData.drag == null
    ) {
      return emptyStrategyApplicationResult
    }

    const { interactionTarget, projectContents, openFile, nodeModules } = canvasState
    const selectedElements = getTargetPathsFromInteractionTarget(interactionTarget)
    const filteredSelectedElements = getDragTargets(selectedElements)

    const pointOnCanvas = offsetPoint(
      interactionState.interactionData.originalDragStart,
      interactionState.interactionData.drag,
    )

    const reparentTarget = getReparentTargetUnified(
      filteredSelectedElements,
      pointOnCanvas,
      interactionState.interactionData.modifiers.cmd,
      canvasState,
      strategyState.startingMetadata,
      strategyState.startingAllElementProps,
    )
    const newParent = reparentTarget.newParent
    const providesBoundsForAbsoluteChildren =
      MetadataUtils.findElementByElementPath(strategyState.startingMetadata, newParent)
        ?.specialSizeMeasurements.providesBoundsForAbsoluteChildren ?? false
    const parentIsStoryboard = newParent == null ? false : EP.isStoryboardPath(newParent)
    const allowedToReparent = filteredSelectedElements.every((selectedElement) => {
      return isAllowedToReparent(
        canvasState.projectContents,
        canvasState.openFile,
        strategyState.startingMetadata,
        selectedElement,
      )
    })

    if (
      reparentTarget.shouldReparent &&
      newParent != null &&
      (providesBoundsForAbsoluteChildren || parentIsStoryboard) &&
      allowedToReparent
    ) {
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
            strategyState.startingMetadata,
            strategyState.startingMetadata,
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

      const moveCommands = absoluteMoveStrategy.apply(
        canvasState,
        {
          ...interactionState,
          updatedTargetPaths: updatedTargetPaths,
        },
        strategyState,
      )

      return {
        commands: [
          ...moveCommands.commands,
          ...commands.flatMap((c) => c.commands),
          updateSelectedViews('always', newPaths),
          setElementsToRerenderCommand([...newPaths, ...filteredSelectedElements]),
          setCursorCommand('mid-interaction', CSSCursor.Move),
        ],
        customState: null,
      }
    } else {
      const moveCommands = absoluteMoveStrategy.apply(canvasState, interactionState, strategyState)

      return {
        commands: moveCommands.commands,
        customState: null,
      }
    }
  },
}
