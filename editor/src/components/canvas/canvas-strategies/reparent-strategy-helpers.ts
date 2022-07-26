import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import {
  canvasRectangle,
  CanvasRectangle,
  offsetPoint,
  zeroCanvasRect,
} from '../../../core/shared/math-utils'
import { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { CSSCursor } from '../canvas-types'
import { getReparentTarget } from '../canvas-utils'
import { CanvasCommand } from '../commands/commands'
import { deleteProperties } from '../commands/delete-properties-command'
import { reorderElement } from '../commands/reorder-element-command'
import { reparentElement } from '../commands/reparent-element-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { updateSelectedViews } from '../commands/update-selected-views-command'
import { wildcardPatch } from '../commands/wildcard-patch-command'
import {
  emptyStrategyApplicationResult,
  InteractionCanvasState,
  StrategyApplicationResult,
} from './canvas-strategy-types'
import { getReorderIndex } from './flex-reorder-strategy'
import { InteractionSession, StrategyState } from './interaction-state'
import { getDragTargets } from './shared-absolute-move-strategy-helpers'

type ReparentStrategy =
  | 'FLEX_REPARENT_TO_ABSOLUTE'
  | 'FLEX_REPARENT_TO_FLEX'
  | 'ABSOLUTE_REPARENT_TO_ABSOLUTE'
  | 'ABSOLUTE_REPARENT_TO_FLEX'

export function findReparentStrategy(
  canvasState: InteractionCanvasState,
  interactionState: InteractionSession,
  strategyState: StrategyState,
): { strategy: ReparentStrategy; newParent: ElementPath } | { strategy: 'do-not-reparent' } {
  if (
    canvasState.selectedElements.length === 0 ||
    interactionState.activeControl.type !== 'BOUNDING_AREA' ||
    interactionState.interactionData.type !== 'DRAG' ||
    !interactionState.interactionData.modifiers.cmd ||
    interactionState.interactionData.drag == null // TODO delete this drag nullcheck? do we start the reparent on mouse down or mouse move beyond threshold?
  ) {
    return { strategy: 'do-not-reparent' }
  }

  const { selectedElements, scale, canvasOffset, projectContents, openFile } = canvasState
  const startingMetadata = strategyState.startingMetadata
  const filteredSelectedElements = getDragTargets(selectedElements)

  const allDraggedElementsFlex = filteredSelectedElements.every((element) =>
    MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      element,
      startingMetadata,
    ),
  )
  const allDraggedElementsAbsolute = filteredSelectedElements.every((element) =>
    MetadataUtils.isPositionAbsolute(
      MetadataUtils.findElementByElementPath(startingMetadata, element),
    ),
  )

  const reparentResult = getReparentTargetForFlexElement(
    filteredSelectedElements,
    interactionState,
    canvasState,
    strategyState,
  )

  const newParentPath = reparentResult.newParent
  const newParentMetadata = MetadataUtils.findElementByElementPath(startingMetadata, newParentPath)
  const parentProvidesBoundsForAbsoluteChildren =
    newParentMetadata?.specialSizeMeasurements.providesBoundsForAbsoluteChildren ?? false

  const parentIsFlexLayout = MetadataUtils.isFlexLayoutedContainer(newParentMetadata)
  const parentIsStoryboard = newParentPath == null ? false : EP.isStoryboardPath(newParentPath)

  if (reparentResult.shouldReparent && newParentPath != null) {
    if (allDraggedElementsAbsolute) {
      if (parentIsFlexLayout) {
        return { strategy: 'ABSOLUTE_REPARENT_TO_FLEX', newParent: newParentPath }
      }
      if (parentProvidesBoundsForAbsoluteChildren || parentIsStoryboard) {
        return { strategy: 'ABSOLUTE_REPARENT_TO_ABSOLUTE', newParent: newParentPath }
      }
    }
    if (allDraggedElementsFlex) {
      if (parentIsFlexLayout) {
        return { strategy: 'FLEX_REPARENT_TO_FLEX', newParent: newParentPath }
      }
      if (parentProvidesBoundsForAbsoluteChildren || parentIsStoryboard) {
        return { strategy: 'FLEX_REPARENT_TO_ABSOLUTE', newParent: newParentPath }
      }
    }
  }
  return { strategy: 'do-not-reparent' }
}

export function getReparentTargetForFlexElement(
  filteredSelectedElements: Array<ElementPath>,
  interactionSession: InteractionSession,
  canvasState: InteractionCanvasState,
  strategyState: StrategyState,
): {
  shouldReparent: boolean
  newParent: ElementPath | null
  shouldReorder: boolean
} {
  if (
    interactionSession.interactionData.type !== 'DRAG' ||
    interactionSession.interactionData.drag == null
  ) {
    return {
      shouldReparent: false,
      newParent: null,
      shouldReorder: false,
    }
  }

  const pointOnCanvas = offsetPoint(
    interactionSession.interactionData.originalDragStart,
    interactionSession.interactionData.drag,
  )

  const reparentResult = getReparentTarget(
    filteredSelectedElements,
    filteredSelectedElements,
    strategyState.startingMetadata,
    [],
    pointOnCanvas,
    canvasState.projectContents,
    canvasState.openFile,
    strategyState.startingAllElementProps,
  )
  if (reparentResult.newParent == null) {
    return {
      ...reparentResult,
      shouldReorder: false,
    }
  } else {
    // The target is in a flex container, so we want the parent of the target to reparent
    // into and reordering should be triggered because the pointer is over an existing flex element.
    if (
      MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        reparentResult.newParent,
        strategyState.startingMetadata,
      )
    ) {
      return {
        shouldReparent: true,
        newParent: EP.parentPath(reparentResult.newParent),
        shouldReorder: true,
      }
    } else {
      // Otherwise we want to use the target directly.
      // But in this case no re-ordering should be triggered, the element should just be
      // added to the end.
      return {
        shouldReparent: true,
        newParent: reparentResult.newParent,
        shouldReorder: false,
      }
    }
  }
}

const propertiesToRemove: Array<PropertyPath> = [
  PP.create(['style', 'position']),
  PP.create(['style', 'left']),
  PP.create(['style', 'top']),
  PP.create(['style', 'right']),
  PP.create(['style', 'bottom']),
]

export function applyFlexReparent(
  stripAbsoluteProperties: 'strip-absolute-props' | 'do-not-strip-props',
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  strategyState: StrategyState,
  lifecycle: 'mid-interaction' | 'end-interaction',
): StrategyApplicationResult {
  if (
    interactionSession.interactionData.type == 'DRAG' &&
    interactionSession.interactionData.drag != null
  ) {
    const filteredSelectedElements = getDragTargets(canvasState.selectedElements)
    const reparentResult = getReparentTargetForFlexElement(
      filteredSelectedElements,
      interactionSession,
      canvasState,
      strategyState,
    )

    if (
      reparentResult.shouldReparent &&
      reparentResult.newParent != null &&
      filteredSelectedElements.length === 1
    ) {
      const target = filteredSelectedElements[0]
      const newParent = reparentResult.newParent
      // Reparent the element.
      const newPath = EP.appendToPath(reparentResult.newParent, EP.toUid(target))
      const reparentCommand = reparentElement('permanent', target, reparentResult.newParent)

      // Strip the `position`, positional and dimension properties.
      const commandToRemoveProperties = stripAbsoluteProperties
        ? [deleteProperties('permanent', newPath, propertiesToRemove)]
        : []

      const commandsBeforeReorder = [reparentCommand, updateSelectedViews('permanent', [newPath])]

      const commandsAfterReorder = [
        ...commandToRemoveProperties,
        setElementsToRerenderCommand([newPath]),
        updateHighlightedViews('transient', []),
        setCursorCommand('transient', CSSCursor.Move),
      ]

      const newParentFlexDirection = MetadataUtils.getFlexDirection(
        MetadataUtils.findElementByElementPath(strategyState.startingMetadata, newParent),
      )

      let interactionFinishCommadns: Array<CanvasCommand>
      let midInteractionCommands: Array<CanvasCommand>

      const siblingsOfTarget = MetadataUtils.getChildrenPaths(
        strategyState.startingMetadata,
        newParent,
      )

      const parentRect = MetadataUtils.getFrameInCanvasCoords(
        newParent,
        strategyState.startingMetadata,
      )

      if (reparentResult.shouldReorder) {
        // Reorder the newly reparented element into the flex ordering.
        const pointOnCanvas = offsetPoint(
          interactionSession.interactionData.dragStart,
          interactionSession.interactionData.drag,
        )

        const newIndex = getReorderIndex(
          strategyState.startingMetadata,
          siblingsOfTarget,
          pointOnCanvas,
        )

        const siblingPosition: CanvasRectangle =
          [
            // parentRect, // we add the parent as the first element
            ...siblingsOfTarget.map((sibling) => {
              return MetadataUtils.getFrameInCanvasCoords(sibling, strategyState.startingMetadata)
            }),
          ][newIndex] ?? zeroCanvasRect

        const targetLineBeforeSibling: CanvasRectangle =
          newParentFlexDirection === 'row'
            ? canvasRectangle({
                x: siblingPosition?.x,
                y: siblingPosition?.y,
                height: siblingPosition?.height,
                width: 2,
              })
            : canvasRectangle({
                x: siblingPosition?.x,
                y: siblingPosition?.y,
                width: siblingPosition?.width,
                height: 2,
              })

        midInteractionCommands = [
          wildcardPatch('transient', {
            canvas: { controls: { flexReparentTargetLines: { $set: [targetLineBeforeSibling] } } },
          }),
        ]

        interactionFinishCommadns = [
          ...commandsBeforeReorder,
          reorderElement('permanent', newPath, newIndex),
          ...commandsAfterReorder,
        ]
      } else {
        const siblingPosition = MetadataUtils.getFrameInCanvasCoords(
          siblingsOfTarget[siblingsOfTarget.length - 1],
          strategyState.startingMetadata,
        )

        if (siblingPosition != null) {
          const targetLineAfterSibling: CanvasRectangle =
            newParentFlexDirection === 'row'
              ? canvasRectangle({
                  x: siblingPosition.x + siblingPosition.width,
                  y: siblingPosition.y,
                  height: siblingPosition.height,
                  width: 2,
                })
              : canvasRectangle({
                  x: siblingPosition.x,
                  y: siblingPosition.y + siblingPosition.height,
                  width: siblingPosition?.width,
                  height: 2,
                })

          midInteractionCommands = [
            wildcardPatch('transient', {
              canvas: { controls: { flexReparentTargetLines: { $set: [targetLineAfterSibling] } } },
            }),
          ]
        } else if (parentRect != null) {
          const targetLineBeginningOfParent: CanvasRectangle =
            newParentFlexDirection === 'row'
              ? canvasRectangle({
                  x: parentRect.x,
                  y: parentRect.y,
                  height: parentRect.height,
                  width: 2,
                })
              : canvasRectangle({
                  x: parentRect.x,
                  y: parentRect.y,
                  width: parentRect.width,
                  height: 2,
                })

          midInteractionCommands = [
            wildcardPatch('transient', {
              canvas: {
                controls: { flexReparentTargetLines: { $set: [targetLineBeginningOfParent] } },
              },
            }),
          ]
        } else {
          // this should be an error because parentRect should never be null
          midInteractionCommands = []
        }

        interactionFinishCommadns = [...commandsBeforeReorder, ...commandsAfterReorder]
      }

      if (lifecycle === 'mid-interaction') {
        // do nothing
        return { commands: midInteractionCommands, customState: null }
      }

      return {
        commands: interactionFinishCommadns,
        customState: strategyState.customStrategyState,
      }
    }
  }
  return emptyStrategyApplicationResult
}
