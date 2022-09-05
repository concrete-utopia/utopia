import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getStoryboardElementPath } from '../../../core/model/scene-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadataMap, JSXElement } from '../../../core/shared/element-template'
import {
  canvasPoint,
  CanvasPoint,
  CanvasRectangle,
  canvasRectangle,
  CanvasVector,
  offsetPoint,
  pointDifference,
  rectContainsPoint,
  rectContainsPointInclusive,
  rectFromTwoPoints,
  Size,
  size,
  sizeFitsInTarget,
  zeroCanvasRect,
} from '../../../core/shared/math-utils'
import { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { ProjectContentTreeRoot } from '../../assets'
import { AllElementProps } from '../../editor/store/editor-state'
import { CSSCursor } from '../canvas-types'
import { CanvasCommand } from '../commands/commands'
import { deleteProperties } from '../commands/delete-properties-command'
import { reorderElement } from '../commands/reorder-element-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { setProperty } from '../commands/set-property-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { updateSelectedViews } from '../commands/update-selected-views-command'
import { wildcardPatch } from '../commands/wildcard-patch-command'
import { getAllTargetsAtPointAABB } from '../dom-lookup'
import {
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  StrategyApplicationResult,
} from './canvas-strategy-types'
import { InteractionSession, StrategyState } from './interaction-state'
import { ifAllowedToReparent } from './reparent-helpers'
import { getReparentOutcome, pathToReparent } from './reparent-utils'
import { getDragTargets } from './shared-absolute-move-strategy-helpers'
import Utils, { absolute } from '../../../utils/utils'
import { getElementFromProjectContents } from '../../../components/editor/store/editor-state'
import { stylePropPathMappingFn } from '../../../components/inspector/common/property-path-hooks'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { LayoutPinnedProp, framePointForPinnedProp } from '../../../core/layout/layout-helpers-new'
import { isRight, right } from '../../../core/shared/either'
import { isHorizontalPoint } from 'utopia-api/core'
import {
  AdjustCssLengthProperty,
  adjustCssLengthProperty,
} from '../commands/adjust-css-length-command'
import { updatePropIfExists } from '../commands/update-prop-if-exists-command'
import { FlexReparentIndicatorSize } from '../controls/select-mode/flex-reparent-target-indicator'

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
  log = false, // DELETE ME BEFORE MERGE
): FindReparentStrategyResult {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (
    selectedElements.length === 0 ||
    interactionState.activeControl.type !== 'BOUNDING_AREA' ||
    interactionState.interactionData.type !== 'DRAG' ||
    interactionState.interactionData.drag == null // TODO delete this drag nullcheck? do we start the reparent on mouse down or mouse move beyond threshold?
  ) {
    return { strategy: 'do-not-reparent' }
  }

  const filteredSelectedElements = getDragTargets(selectedElements)

  const pointOnCanvas = offsetPoint(
    interactionState.interactionData.originalDragStart,
    interactionState.interactionData.drag,
  )

  const cmdPressed = interactionState.interactionData.modifiers.cmd

  const reparentResult = getReparentTargetUnified(
    filteredSelectedElements,
    pointOnCanvas,
    cmdPressed,
    canvasState,
    strategyState.startingMetadata,
    strategyState.startingAllElementProps,
  )

  const newParentPath = reparentResult.newParent

  const parentStayedTheSame = filteredSelectedElements.some(
    (e) => EP.parentPath(e) === newParentPath,
  )
  const newParentMetadata = MetadataUtils.findElementByElementPath(
    strategyState.startingMetadata,
    newParentPath,
  )
  const parentIsFlexLayout = MetadataUtils.isFlexLayoutedContainer(newParentMetadata)

  if (
    reparentResult.shouldReparent &&
    newParentPath != null &&
    // holding cmd forces a reparent even if the target parent was under the mouse at the interaction start
    (cmdPressed || newParentPath !== interactionState.startingTargetParentToFilterOut?.newParent) &&
    (parentIsFlexLayout || !parentStayedTheSame) // TODO review this, as it is a result of a merge with master
  ) {
    return reparentStrategyForParent(
      strategyState.startingMetadata,
      strategyState.startingMetadata,
      filteredSelectedElements,
      newParentPath,
    )
  } else {
    return { strategy: 'do-not-reparent' }
  }
}

export interface ReparentTarget {
  shouldReparent: boolean
  newParent: ElementPath | null
  shouldReorder: boolean
  newIndex: number
}

export function reparentTarget(
  shouldReparent: boolean,
  newParent: ElementPath | null,
  shouldReorder: boolean,
  newIndex: number,
): ReparentTarget {
  return {
    shouldReparent: shouldReparent,
    newParent: newParent,
    shouldReorder: shouldReorder,
    newIndex: newIndex,
  }
}

export function getReparentTargetUnified(
  filteredSelectedElements: Array<ElementPath>,
  pointOnCanvas: CanvasPoint,
  cmdPressed: boolean,
  canvasState: InteractionCanvasState,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
): ReparentTarget {
  const projectContents = canvasState.projectContents
  const openFile = canvasState.openFile ?? null

  const multiselectBounds: Size =
    MetadataUtils.getBoundingRectangleInCanvasCoords(filteredSelectedElements, metadata) ??
    size(0, 0)

  const allElementsUnderPoint = getAllTargetsAtPointAABB(
    metadata,
    [],
    [],
    'no-filter',
    pointOnCanvas,
    allElementProps,
    false,
  )

  if (allElementsUnderPoint.length === 0) {
    const storyboardComponent = getStoryboardElementPath(projectContents, openFile)
    return {
      shouldReparent: storyboardComponent != null,
      newParent: storyboardComponent,
      shouldReorder: false,
      newIndex: -1,
    }
  }

  const filteredSelectedElementsMetadata = mapDropNulls(
    (path) => MetadataUtils.findElementByElementPath(metadata, path),
    filteredSelectedElements,
  )

  const filteredElementsUnderPoint = allElementsUnderPoint.filter((target) => {
    const targetMetadata = MetadataUtils.findElementByElementPath(metadata, target)
    const isFlex = MetadataUtils.isFlexLayoutedContainer(targetMetadata)
    const providesBoundsForAbsoluteChildren =
      targetMetadata?.specialSizeMeasurements.providesBoundsForAbsoluteChildren ?? false

    // TODO extend here when we implement static layout support
    const validParentForFlexOrAbsolute = isFlex || providesBoundsForAbsoluteChildren

    // TODO BEFORE MERGE consider multiselect!!!!!
    // the current parent should be included in the array of valid targets
    return (
      filteredSelectedElementsMetadata.some((maybeChild) =>
        EP.isChildOf(maybeChild.elementPath, target),
      ) ||
      // any of the dragged elements (or their flex parents) and their descendants are not game for reparenting
      (filteredSelectedElementsMetadata.findIndex((maybeAncestorOrEqual) =>
        !cmdPressed && maybeAncestorOrEqual.specialSizeMeasurements.parentLayoutSystem === 'flex'
          ? // for Flex children, we also want to filter out all their siblings to force a Flex Reorder strategy
            EP.isDescendantOf(target, EP.parentPath(maybeAncestorOrEqual.elementPath))
          : // for non-flex elements, we filter out their descendants and themselves
            EP.isDescendantOfOrEqualTo(target, maybeAncestorOrEqual.elementPath),
      ) === -1 &&
        // simply skip elements that do not support children
        MetadataUtils.targetSupportsChildren(projectContents, openFile, metadata, target) &&
        // if cmd is not pressed, we only allow reparent to parents that are larger than the multiselect bounds
        (cmdPressed ||
          sizeFitsInTarget(
            multiselectBounds,
            MetadataUtils.getFrameInCanvasCoords(target, metadata) ?? size(0, 0),
          )) &&
        validParentForFlexOrAbsolute)
    )
  })

  // if the mouse is over the canvas, return the canvas root as the target path

  const flexElementsUnderPoint = [...filteredElementsUnderPoint]
    .reverse()
    .filter((element) =>
      MetadataUtils.isFlexLayoutedContainer(
        MetadataUtils.findElementByElementPath(metadata, element),
      ),
    )

  // first try to find a flex element insertion area
  for (const flexElementPath of flexElementsUnderPoint) {
    const targets: Array<CanvasRectangle> = drawTargetRectanglesForChildrenOfElement(
      metadata,
      flexElementPath,
      'padded-edge',
    )

    const targetUnderMouseIndex = targets.findIndex((target) => {
      return rectContainsPoint(target, pointOnCanvas)
    })

    if (targetUnderMouseIndex > -1) {
      // we found a target!
      return {
        shouldReparent: true,
        shouldReorder: true,
        newParent: flexElementPath,
        newIndex: targetUnderMouseIndex,
      }
    }
  }

  // fall back to trying to find an absolute element, or the "background" area of a flex element
  const targetParentPath = filteredElementsUnderPoint[0]
  if (targetParentPath == null) {
    // none of the targets were under the mouse, fallback return
    return {
      shouldReparent: false,
      shouldReorder: false,
      newParent: null,
      newIndex: -1,
    }
  }
  const element = MetadataUtils.findElementByElementPath(metadata, targetParentPath)
  const isFlex = MetadataUtils.isFlexLayoutedContainer(element)

  if (!isFlex) {
    // TODO we now assume this is "absolute", but this is too vauge
    return {
      shouldReparent: true,
      newParent: targetParentPath,
      shouldReorder: false,
      newIndex: -1,
    }
  } else {
    const targets: Array<CanvasRectangle> = drawTargetRectanglesForChildrenOfElement(
      metadata,
      targetParentPath,
      'full-size',
    )

    const targetUnderMouseIndex = targets.findIndex((target) => {
      return rectContainsPointInclusive(target, pointOnCanvas)
    })

    // found flex element, todo index
    return {
      shouldReparent: true,
      newParent: targetParentPath,
      shouldReorder: targetUnderMouseIndex > -1,
      newIndex: targetUnderMouseIndex,
    }
  }
}

const propertiesToRemove: Array<PropertyPath> = [
  PP.create(['style', 'left']),
  PP.create(['style', 'top']),
  PP.create(['style', 'right']),
  PP.create(['style', 'bottom']),
]

function drawTargetRectanglesForChildrenOfElement(
  metadata: ElementInstanceMetadataMap,
  flexElementPath: ElementPath,
  targetRectangleSize: 'padded-edge' | 'full-size',
) {
  const flexElement = MetadataUtils.findElementByElementPath(metadata, flexElementPath)
  const flexDirection = MetadataUtils.getFlexDirection(flexElement)
  const parentBounds = MetadataUtils.getFrameInCanvasCoords(flexElementPath, metadata)

  if (parentBounds == null) {
    // TODO should we throw an error?
    return []
  }

  const leftOrTop = flexDirection === 'row' ? 'x' : 'y'
  const leftOrTopComplement = flexDirection === 'row' ? 'y' : 'x'
  const widthOrHeight = flexDirection === 'row' ? 'width' : 'height'
  const widthOrHeightComplement = flexDirection === 'row' ? 'height' : 'width'

  const pseudoElementBefore = {
    start: parentBounds[leftOrTop],
    size: 0,
    end: parentBounds[leftOrTop],
  }
  const pseudoElementAfter = {
    start: parentBounds[leftOrTop] + parentBounds[widthOrHeight],
    size: 0,
    end: parentBounds[leftOrTop] + parentBounds[widthOrHeight],
  }

  const childrenBounds: Array<{ start: number; size: number; end: number }> =
    MetadataUtils.getChildrenPaths(metadata, flexElementPath).map((childPath) => {
      const bounds = MetadataUtils.getFrameInCanvasCoords(childPath, metadata)!
      return {
        start: bounds[leftOrTop],
        size: bounds[widthOrHeight],
        end: bounds[leftOrTop] + bounds[widthOrHeight],
      }
    })

  const childrenBoundsAlongAxis: Array<{ start: number; size: number; end: number }> = [
    pseudoElementBefore,
    ...childrenBounds,
    pseudoElementAfter,
  ]

  let flexInsertionTargets: Array<CanvasRectangle> = []

  if (targetRectangleSize === 'padded-edge') {
    for (let index = 0; index < childrenBoundsAlongAxis.length - 1; index++) {
      const start = childrenBoundsAlongAxis[index].end
      const end = childrenBoundsAlongAxis[index + 1].start

      const normalizedStart = Math.min(start, end)
      const normalizedEnd = Math.max(start, end)

      const ExtraPadding = 10

      const paddedStart = normalizedStart - ExtraPadding
      const paddedEnd = normalizedEnd + ExtraPadding

      flexInsertionTargets.push(
        rectFromTwoPoints(
          {
            [leftOrTop]: paddedStart,
            [leftOrTopComplement]: parentBounds[leftOrTopComplement],
          } as any as CanvasPoint, // TODO improve my type
          {
            [leftOrTop]: paddedEnd,
            [leftOrTopComplement]:
              parentBounds[leftOrTopComplement] + parentBounds[widthOrHeightComplement],
          } as any as CanvasPoint, // TODO improve my type
        ),
      )
    }
  } else {
    // full size target rectangles, covering the entire flex element
    for (let index = 0; index < childrenBoundsAlongAxis.length - 1; index++) {
      const start = childrenBoundsAlongAxis[index].start + childrenBoundsAlongAxis[index].size / 2
      const end =
        childrenBoundsAlongAxis[index + 1].start + childrenBoundsAlongAxis[index + 1].size / 2

      const normalizedStart = Math.min(start, end)
      const normalizedEnd = Math.max(start, end)

      flexInsertionTargets.push(
        rectFromTwoPoints(
          {
            [leftOrTop]: normalizedStart,
            [leftOrTopComplement]: parentBounds[leftOrTopComplement],
          } as any as CanvasPoint, // TODO improve my type
          {
            [leftOrTop]: normalizedEnd,
            [leftOrTopComplement]:
              parentBounds[leftOrTopComplement] + parentBounds[widthOrHeightComplement],
          } as any as CanvasPoint, // TODO improve my type
        ),
      )
    }
  }

  return flexInsertionTargets
}

export type StripAbsoluteProperties = 'strip-absolute-props' | 'do-not-strip-props'

export function applyFlexReparent(
  stripAbsoluteProperties: StripAbsoluteProperties,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  strategyState: StrategyState,
): StrategyApplicationResult {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  const filteredSelectedElements = getDragTargets(selectedElements)

  return ifAllowedToReparent(canvasState, strategyState, filteredSelectedElements, () => {
    if (
      interactionSession.interactionData.type == 'DRAG' &&
      interactionSession.interactionData.drag != null
    ) {
      const pointOnCanvas = offsetPoint(
        interactionSession.interactionData.originalDragStart,
        interactionSession.interactionData.drag,
      )
      const reparentResult = getReparentTargetUnified(
        filteredSelectedElements,
        pointOnCanvas,
        interactionSession.interactionData.modifiers.cmd,
        canvasState,
        strategyState.startingMetadata,
        strategyState.startingAllElementProps,
      )

      if (
        reparentResult.shouldReparent &&
        reparentResult.newParent != null &&
        filteredSelectedElements.length === 1 // TODO remove the restriction of single select
      ) {
        const target = filteredSelectedElements[0]
        const targetSize =
          MetadataUtils.getFrameInCanvasCoords(target, strategyState.startingMetadata) ??
          zeroCanvasRect

        const newIndex = reparentResult.newIndex
        const newParent = reparentResult.newParent
        const newParentMetadata = MetadataUtils.findElementByElementPath(
          strategyState.startingMetadata,
          newParent,
        )
        const parentRect = MetadataUtils.getFrameInCanvasCoords(
          newParent,
          strategyState.startingMetadata,
        )
        const newParentFlexDirection = MetadataUtils.getFlexDirection(newParentMetadata)
        const parentFlexGap = newParentMetadata?.specialSizeMeasurements.flexGap ?? 0

        const newParentADescendantOfCurrentParent = EP.isDescendantOfOrEqualTo(
          newParent,
          EP.parentPath(target),
        )

        const siblingsOfTarget = MetadataUtils.getChildrenPaths(
          strategyState.startingMetadata,
          newParent,
        )

        // Reparent the element.
        const outcomeResult = getReparentOutcome(
          canvasState.builtInDependencies,
          canvasState.projectContents,
          canvasState.nodeModules,
          canvasState.openFile,
          pathToReparent(target),
          newParent,
          'on-complete',
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
            updateSelectedViews('on-complete', [newPath]),
          ]

          const commandsAfterReorder = [
            ...propertyChangeCommands,
            setElementsToRerenderCommand([target, newPath]),
            updateHighlightedViews('mid-interaction', []),
            setCursorCommand('mid-interaction', CSSCursor.Move),
          ]

          let interactionFinishCommands: Array<CanvasCommand>
          let midInteractionCommands: Array<CanvasCommand>

          if (reparentResult.shouldReorder && newIndex < siblingsOfTarget.length) {
            // Reorder the newly reparented element into the flex ordering.
            const siblingPosition: CanvasRectangle =
              [
                // parentRect, // we add the parent as the first element
                ...siblingsOfTarget.map((sibling) => {
                  return MetadataUtils.getFrameInCanvasCoords(
                    sibling,
                    strategyState.startingMetadata,
                  )
                }),
              ][newIndex] ?? zeroCanvasRect

            const targetLineBeforeSibling: CanvasRectangle =
              newParentFlexDirection === 'row' // TODO handle row-reverse and col-reverse
                ? canvasRectangle({
                    x: siblingPosition.x - parentFlexGap / 2 - FlexReparentIndicatorSize / 2,
                    y: siblingPosition.y,
                    height: siblingPosition.height,
                    width: FlexReparentIndicatorSize,
                  })
                : canvasRectangle({
                    x: siblingPosition.x,
                    y: siblingPosition.y - parentFlexGap / 2 - FlexReparentIndicatorSize / 2,
                    width: siblingPosition.width,
                    height: FlexReparentIndicatorSize,
                  })

            midInteractionCommands = [
              wildcardPatch('mid-interaction', {
                canvas: { controls: { parentHighlightPaths: { $set: [newParent] } } },
              }),
              wildcardPatch('mid-interaction', {
                canvas: {
                  controls: { flexReparentTargetLines: { $set: [targetLineBeforeSibling] } },
                },
              }),
              newParentADescendantOfCurrentParent
                ? wildcardPatch('mid-interaction', {
                    hiddenInstances: { $push: [target] },
                  })
                : wildcardPatch('mid-interaction', {
                    displayNoneInstances: { $push: [target] },
                  }),
            ]

            interactionFinishCommands = [
              ...commandsBeforeReorder,
              reorderElement('on-complete', newPath, absolute(newIndex)),
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
                      x:
                        siblingPosition.x +
                        siblingPosition.width +
                        parentFlexGap / 2 +
                        FlexReparentIndicatorSize / 2,
                      y: siblingPosition.y,
                      height: siblingPosition.height,
                      width: FlexReparentIndicatorSize,
                    })
                  : canvasRectangle({
                      x: siblingPosition.x,
                      y:
                        siblingPosition.y +
                        siblingPosition.height +
                        parentFlexGap / 2 +
                        FlexReparentIndicatorSize / 2,
                      width: siblingPosition.width,
                      height: FlexReparentIndicatorSize,
                    })

              midInteractionCommands = [
                wildcardPatch('mid-interaction', {
                  canvas: { controls: { parentHighlightPaths: { $set: [newParent] } } },
                }),
                wildcardPatch('mid-interaction', {
                  canvas: {
                    controls: { flexReparentTargetLines: { $set: [targetLineAfterSibling] } },
                  },
                }),
                newParentADescendantOfCurrentParent
                  ? wildcardPatch('mid-interaction', {
                      hiddenInstances: { $push: [target] },
                    })
                  : wildcardPatch('mid-interaction', {
                      displayNoneInstances: { $push: [target] },
                    }),
              ]
            } else if (parentRect != null) {
              const targetLineBeginningOfParent: CanvasRectangle =
                newParentFlexDirection === 'row'
                  ? canvasRectangle({
                      x: parentRect.x,
                      y: parentRect.y,
                      height: parentRect.height,
                      width: FlexReparentIndicatorSize,
                    })
                  : canvasRectangle({
                      x: parentRect.x,
                      y: parentRect.y,
                      width: parentRect.width,
                      height: FlexReparentIndicatorSize,
                    })

              midInteractionCommands = [
                wildcardPatch('mid-interaction', {
                  canvas: { controls: { parentHighlightPaths: { $set: [newParent] } } },
                }),
                wildcardPatch('mid-interaction', {
                  canvas: {
                    controls: { flexReparentTargetLines: { $set: [targetLineBeginningOfParent] } },
                  },
                }),
                newParentADescendantOfCurrentParent
                  ? wildcardPatch('mid-interaction', {
                      hiddenInstances: { $push: [target] },
                    })
                  : wildcardPatch('mid-interaction', {
                      displayNoneInstances: { $push: [target] },
                    }),
              ]
            } else {
              // this should be an error because parentRect should never be null
              midInteractionCommands = []
            }

            interactionFinishCommands = [...commandsBeforeReorder, ...commandsAfterReorder]
          }

          return {
            commands: [...midInteractionCommands, ...interactionFinishCommands],
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
  return stripAbsoluteProperties
    ? [
        deleteProperties('on-complete', newPath, propertiesToRemove),
        updatePropIfExists('on-complete', newPath, PP.create(['style', 'position']), 'relative'), // SPIKE TODO only insert position: relative if there was a position nonstatic prop before
      ]
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
