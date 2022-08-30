import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadataMap, JSXElement } from '../../../core/shared/element-template'
import {
  canvasPoint,
  CanvasPoint,
  CanvasVector,
  offsetPoint,
  pointDifference,
  rectContainsPoint,
  zeroCanvasRect,
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
import { getReparentOutcome, pathToReparent } from './reparent-utils'
import { getDragTargets } from './shared-absolute-move-strategy-helpers'
import Utils, { absolute } from '../../../utils/utils'
import { ProjectContentTreeRoot } from '../../../components/assets'
import { getElementFromProjectContents } from '../../../components/editor/store/editor-state'
import { stylePropPathMappingFn } from '../../../components/inspector/common/property-path-hooks'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { LayoutPinnedProp, framePointForPinnedProp } from '../../../core/layout/layout-helpers-new'
import { isRight, right } from '../../../core/shared/either'
import { isHorizontalPoint } from 'utopia-api/core'
import { mapDropNulls } from '../../../core/shared/array-utils'
import {
  AdjustCssLengthProperty,
  adjustCssLengthProperty,
} from '../commands/adjust-css-length-command'

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

export type ReparentStrategy =
  | 'FLEX_REPARENT_TO_ABSOLUTE'
  | 'FLEX_REPARENT_TO_FLEX'
  | 'ABSOLUTE_REPARENT_TO_ABSOLUTE'
  | 'ABSOLUTE_REPARENT_TO_FLEX'

export type FindReparentStrategyResult =
  | { strategy: ReparentStrategy; newParent: ElementPath }
  | { strategy: 'do-not-reparent' }

export function reparentStrategyForParent(
  originalMetadata: ElementInstanceMetadataMap,
  targetMetadata: ElementInstanceMetadataMap,
  elements: Array<ElementPath>,
  newParent: ElementPath,
): FindReparentStrategyResult {
  const allDraggedElementsFlex = elements.every((element) =>
    MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      element,
      originalMetadata,
    ),
  )
  const allDraggedElementsAbsolute = elements.every((element) =>
    MetadataUtils.isPositionAbsolute(
      MetadataUtils.findElementByElementPath(originalMetadata, element),
    ),
  )

  const newParentMetadata = MetadataUtils.findElementByElementPath(targetMetadata, newParent)
  const parentProvidesBoundsForAbsoluteChildren =
    newParentMetadata?.specialSizeMeasurements.providesBoundsForAbsoluteChildren ?? false

  const parentIsFlexLayout = MetadataUtils.isFlexLayoutedContainer(newParentMetadata)
  const parentIsStoryboard = EP.isStoryboardPath(newParent)

  if (allDraggedElementsAbsolute) {
    if (parentIsFlexLayout) {
      return { strategy: 'ABSOLUTE_REPARENT_TO_FLEX', newParent: newParent }
    }
    if (parentProvidesBoundsForAbsoluteChildren || parentIsStoryboard) {
      return { strategy: 'ABSOLUTE_REPARENT_TO_ABSOLUTE', newParent: newParent }
    }
  }
  if (allDraggedElementsFlex) {
    if (parentIsFlexLayout) {
      return { strategy: 'FLEX_REPARENT_TO_FLEX', newParent: newParent }
    }
    if (parentProvidesBoundsForAbsoluteChildren || parentIsStoryboard) {
      return { strategy: 'FLEX_REPARENT_TO_ABSOLUTE', newParent: newParent }
    }
  }
  return { strategy: 'do-not-reparent' }
}

export function findReparentStrategy(
  canvasState: InteractionCanvasState,
  interactionState: InteractionSession,
  strategyState: StrategyState,
): FindReparentStrategyResult {
  if (
    canvasState.selectedElements.length === 0 ||
    interactionState.activeControl.type !== 'BOUNDING_AREA' ||
    interactionState.interactionData.type !== 'DRAG' ||
    !interactionState.interactionData.modifiers.cmd ||
    interactionState.interactionData.drag == null // TODO delete this drag nullcheck? do we start the reparent on mouse down or mouse move beyond threshold?
  ) {
    return { strategy: 'do-not-reparent' }
  }

  const filteredSelectedElements = getDragTargets(canvasState.selectedElements)

  const reparentResult = getReparentTargetForFlexElement(
    filteredSelectedElements,
    interactionState,
    canvasState,
    strategyState,
  )

  if (reparentResult.shouldReparent && reparentResult.newParent != null) {
    return reparentStrategyForParent(
      strategyState.startingMetadata,
      strategyState.startingMetadata,
      filteredSelectedElements,
      reparentResult.newParent,
    )
  } else {
    return { strategy: 'do-not-reparent' }
  }
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

export type StripAbsoluteProperties = 'strip-absolute-props' | 'do-not-strip-props'

export function applyFlexReparent(
  stripAbsoluteProperties: StripAbsoluteProperties,
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
        const outcomeResult = getReparentOutcome(
          canvasState.builtInDependencies,
          canvasState.projectContents,
          canvasState.nodeModules,
          canvasState.openFile,
          pathToReparent(target),
          reparentResult.newParent,
        )

        if (outcomeResult != null) {
          const { commands: reparentCommands, newPath } = outcomeResult

          // Strip the `position`, positional and dimension properties.
          const propertyChangeCommands = getFlexReparentPropertyChanges(
            stripAbsoluteProperties,
            newPath,
          )

          const commandsBeforeReorder = [
            ...reparentCommands,
            updateSelectedViews('always', [newPath]),
          ]

          const commandsAfterReorder = [
            ...propertyChangeCommands,
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
              reorderElement('always', newPath, absolute(newIndex)),
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
    }
    return emptyStrategyApplicationResult
  })
}

export function getAbsoluteReparentPropertyChanges(
  target: ElementPath,
  newParent: ElementPath,
  targetStartingMetadata: ElementInstanceMetadataMap,
  newParentStartingMetadata: ElementInstanceMetadataMap,
  projectContents: ProjectContentTreeRoot,
  openFile: string | null | undefined,
): Array<AdjustCssLengthProperty> {
  const element: JSXElement | null = getElementFromProjectContents(
    target,
    projectContents,
    openFile,
  )

  if (element == null) {
    return []
  }

  const currentParentContentBox =
    MetadataUtils.findElementByElementPath(targetStartingMetadata, EP.parentPath(target))
      ?.specialSizeMeasurements.globalContentBox ?? zeroCanvasRect

  const newParentContentBox =
    MetadataUtils.findElementByElementPath(newParentStartingMetadata, newParent)
      ?.specialSizeMeasurements.globalContentBox ?? zeroCanvasRect

  const offsetTL = pointDifference(newParentContentBox, currentParentContentBox)
  const offsetBR = pointDifference(
    canvasPoint({
      x: currentParentContentBox.x + currentParentContentBox.width,
      y: currentParentContentBox.y + currentParentContentBox.height,
    }),
    canvasPoint({
      x: newParentContentBox.x + newParentContentBox.width,
      y: newParentContentBox.y + newParentContentBox.height,
    }),
  )

  const createAdjustCssLengthProperty = (
    pin: LayoutPinnedProp,
    newValue: number,
    parentDimension: number | undefined,
  ): AdjustCssLengthProperty | null => {
    const value = getLayoutProperty(pin, right(element.props), ['style'])
    if (isRight(value) && value.value != null) {
      // TODO what to do about missing properties?
      return adjustCssLengthProperty(
        'always',
        target,
        stylePropPathMappingFn(pin, ['style']),
        newValue,
        parentDimension,
        true,
      )
    } else {
      return null
    }
  }

  const newParentFrame = MetadataUtils.getFrameInCanvasCoords(newParent, newParentStartingMetadata)

  return [
    ...mapDropNulls(
      (pin) => {
        const horizontal = isHorizontalPoint(framePointForPinnedProp(pin))
        return createAdjustCssLengthProperty(
          pin,
          horizontal ? offsetTL.x : offsetTL.y,
          horizontal ? newParentFrame?.width : newParentFrame?.height,
        )
      },
      ['top', 'left'] as const,
    ),
    ...mapDropNulls(
      (pin) => {
        const horizontal = isHorizontalPoint(framePointForPinnedProp(pin))
        return createAdjustCssLengthProperty(
          pin,
          horizontal ? offsetBR.x : offsetBR.y,
          horizontal ? newParentFrame?.width : newParentFrame?.height,
        )
      },
      ['bottom', 'right'] as const,
    ),
  ]
}

export function getFlexReparentPropertyChanges(
  stripAbsoluteProperties: StripAbsoluteProperties,
  newPath: ElementPath,
) {
  return stripAbsoluteProperties === 'strip-absolute-props'
    ? [deleteProperties('always', newPath, absolutePropsToRemove)]
    : []
}

export function getReparentPropertyChanges(
  reparentStrategy: ReparentStrategy,
  target: ElementPath,
  newParent: ElementPath,
  targetStartingMetadata: ElementInstanceMetadataMap,
  newParentStartingMetadata: ElementInstanceMetadataMap,
  projectContents: ProjectContentTreeRoot,
  openFile: string | null | undefined,
): Array<CanvasCommand> {
  switch (reparentStrategy) {
    case 'FLEX_REPARENT_TO_ABSOLUTE':
    case 'ABSOLUTE_REPARENT_TO_ABSOLUTE':
      return getAbsoluteReparentPropertyChanges(
        target,
        newParent,
        targetStartingMetadata,
        newParentStartingMetadata,
        projectContents,
        openFile,
      )
    case 'ABSOLUTE_REPARENT_TO_FLEX':
    case 'FLEX_REPARENT_TO_FLEX':
      const newPath = EP.appendToPath(newParent, EP.toUid(target))
      return getFlexReparentPropertyChanges('strip-absolute-props', newPath)
  }
}
