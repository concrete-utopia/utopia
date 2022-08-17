import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import {
  CanvasPoint,
  CanvasVector,
  offsetPoint,
  rectContainsPoint,
} from '../../../core/shared/math-utils'
import { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { CSSCursor } from '../canvas-types'
import { getReparentTarget } from '../canvas-utils'
import { CanvasCommand } from '../commands/commands'
import { deleteProperties } from '../commands/delete-properties-command'
import { reorderElement } from '../commands/reorder-element-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { updateSelectedViews } from '../commands/update-selected-views-command'
import {
  emptyStrategyApplicationResult,
  InteractionCanvasState,
  StrategyApplicationResult,
} from './canvas-strategy-types'
import { InteractionSession, StrategyState } from './interaction-state'
import { ifAllowedToReparent } from './reparent-helpers'
import { getReparentCommands } from './reparent-utils'
import { getDragTargets } from './shared-absolute-move-strategy-helpers'
import Utils from '../../../utils/utils'
import { reverse } from '../../../core/shared/array-utils'

interface ReorderElement {
  distance: number
  centerPoint: CanvasPoint
  closestSibling: ElementPath
  siblingIndex: number
}

export function getReorderIndex(
  metadata: ElementInstanceMetadataMap,
  siblings: Array<ElementPath>,
  point: CanvasVector,
  existingElement: ElementPath | null,
): number {
  let rowOrColumn: 'row' | 'column' | null = null

  const first = siblings[0]
  if (first != null) {
    const parentPath = EP.parentPath(first)
    if (parentPath != null) {
      const parentMetadata = MetadataUtils.findElementByElementPath(metadata, parentPath)
      if (parentMetadata?.specialSizeMeasurements.layoutSystemForChildren != 'flex') {
        throw new Error(`Element ${EP.toString(parentPath)} is not a flex container`)
      }

      const flexDirection = parentMetadata?.specialSizeMeasurements.flexDirection
      if (flexDirection != null) {
        if (flexDirection.includes('row')) {
          rowOrColumn = 'row'
        } else if (flexDirection.includes('col')) {
          rowOrColumn = 'column'
        }
      }
    }
  }

  let reorderResult: ReorderElement | null = null
  let siblingIndex: number = 0

  for (const sibling of siblings) {
    const frame = MetadataUtils.getFrameInCanvasCoords(sibling, metadata)
    if (
      frame != null &&
      MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(sibling, metadata)
    ) {
      const centerPoint = Utils.getRectCenter(frame)
      const distance = Utils.distance(point, centerPoint)
      // First one that has been found or if it's closer than a previously found entry.
      if (reorderResult == null || distance < reorderResult.distance) {
        reorderResult = {
          distance: distance,
          centerPoint: centerPoint,
          closestSibling: sibling,
          siblingIndex: siblingIndex,
        }
      }
    }
    siblingIndex++
  }

  if (reorderResult == null) {
    // We were unable to find an appropriate entry.
    return -1
  } else if (EP.pathsEqual(reorderResult.closestSibling, existingElement)) {
    // Reparenting to the same position that the existing element started in.
    return reorderResult.siblingIndex
  } else {
    // Check which "side" of the target this falls on.
    let newIndex = reorderResult.siblingIndex
    if (rowOrColumn === 'row') {
      if (point.x > reorderResult.centerPoint.x) {
        newIndex++
      }
    } else if (rowOrColumn === 'column') {
      if (point.y > reorderResult.centerPoint.y) {
        newIndex++
      }
    }
    return newIndex
  }
}

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
        shouldReorder: true,
      }
    }
  }
}

const absolutePropsToRemove: Array<PropertyPath> = [
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
): StrategyApplicationResult {
  const filteredSelectedElements = getDragTargets(canvasState.selectedElements)

  return ifAllowedToReparent(canvasState, strategyState, filteredSelectedElements, () => {
    if (
      interactionSession.interactionData.type == 'DRAG' &&
      interactionSession.interactionData.drag != null
    ) {
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
        const reparentCommands = getReparentCommands(
          canvasState.builtInDependencies,
          canvasState.projectContents,
          canvasState.nodeModules,
          canvasState.openFile,
          target,
          reparentResult.newParent,
        )

        // Strip the `position`, positional and dimension properties.
        const commandToRemoveProperties =
          stripAbsoluteProperties === 'strip-absolute-props'
            ? [deleteProperties('always', newPath, absolutePropsToRemove)]
            : []

        const commandsBeforeReorder = [
          ...reparentCommands,
          updateSelectedViews('always', [newPath]),
        ]

        const commandsAfterReorder = [
          ...commandToRemoveProperties,
          setElementsToRerenderCommand([newPath]),
          updateHighlightedViews('mid-interaction', []),
          setCursorCommand('mid-interaction', CSSCursor.Move),
        ]

        let commands: Array<CanvasCommand>
        if (reparentResult.shouldReorder) {
          // Reorder the newly reparented element into the flex ordering.
          const pointOnCanvas = offsetPoint(
            interactionSession.interactionData.dragStart,
            interactionSession.interactionData.drag,
          )

          const siblingsOfTarget = MetadataUtils.getChildrenPaths(
            strategyState.startingMetadata,
            newParent,
          )

          const newIndex = getReorderIndex(
            strategyState.startingMetadata,
            siblingsOfTarget,
            pointOnCanvas,
            null,
          )
          commands = [
            ...commandsBeforeReorder,
            reorderElement('always', newPath, newIndex),
            ...commandsAfterReorder,
          ]
        } else {
          commands = [...commandsBeforeReorder, ...commandsAfterReorder]
        }

        return {
          commands: commands,
          customState: strategyState.customStrategyState,
        }
      }
    }
    return emptyStrategyApplicationResult
  })
}
