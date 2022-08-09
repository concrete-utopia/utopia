import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getStoryboardElementPath } from '../../../core/model/scene-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import {
  CanvasPoint,
  canvasRectangle,
  CanvasRectangle,
  offsetPoint,
  rectContainsPoint,
  rectContainsPointInclusive,
  rectFromTwoPoints,
  rectSize,
  size,
  Size,
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
import { reparentElement } from '../commands/reparent-element-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { setProperty } from '../commands/set-property-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { updateSelectedViews } from '../commands/update-selected-views-command'
import { wildcardPatch } from '../commands/wildcard-patch-command'
import { getAllTargetsAtPointAABB } from '../dom-lookup'
import {
  emptyStrategyApplicationResult,
  InteractionCanvasState,
  StrategyApplicationResult,
} from './canvas-strategy-types'
import {
  DragInteractionData,
  InputData,
  InteractionSession,
  StrategyState,
} from './interaction-state'
import { ifAllowedToReparent } from './reparent-helpers'
import { getReparentCommands } from './reparent-utils'
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
  log = false, // DELETE ME BEFORE MERGE
): { strategy: ReparentStrategy; newParent: ElementPath } | { strategy: 'do-not-reparent' } {
  if (
    canvasState.selectedElements.length === 0 ||
    interactionState.activeControl.type !== 'BOUNDING_AREA' ||
    interactionState.interactionData.type !== 'DRAG' ||
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

  const pointOnCanvas = offsetPoint(
    interactionState.interactionData.originalDragStart,
    interactionState.interactionData.drag,
  )

  const reparentResult = newGetReparentTarget(
    filteredSelectedElements,
    pointOnCanvas,
    interactionState.interactionData.modifiers.cmd,
    canvasState,
    strategyState.startingMetadata,
    strategyState.startingAllElementProps,
  )

  const newParentPath = reparentResult.newParent
  const newParentMetadata = MetadataUtils.findElementByElementPath(startingMetadata, newParentPath)
  const parentProvidesBoundsForAbsoluteChildren =
    newParentMetadata?.specialSizeMeasurements.providesBoundsForAbsoluteChildren ?? false

  const parentIsFlexLayout = MetadataUtils.isFlexLayoutedContainer(newParentMetadata)
  const parentIsStoryboard = newParentPath == null ? false : EP.isStoryboardPath(newParentPath)

  if (
    reparentResult.shouldReparent &&
    newParentPath != null &&
    newParentPath !== interactionState.startingTargetParentToFilterOut?.newParent
  ) {
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

export function newGetReparentTarget(
  filteredSelectedElements: Array<ElementPath>,
  pointOnCanvas: CanvasPoint,
  cmdPressed: boolean,
  canvasState: InteractionCanvasState,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
): ReparentTarget {
  const flexReparentResult = newGetReparentTargetInner(
    cmdPressed,
    metadata,
    allElementProps,
    canvasState.projectContents,
    canvasState.openFile ?? null,
    pointOnCanvas,
    filteredSelectedElements,
  )

  return flexReparentResult
}

function newGetReparentTargetInner(
  cmdPressed: boolean,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  projectContents: ProjectContentTreeRoot,
  openFile: string | null,
  point: CanvasPoint,
  filteredSelectedElements: Array<ElementPath>,
): {
  shouldReparent: boolean
  newParent: ElementPath | null
  shouldReorder: boolean
  newIndex: number
} {
  const multiselectBounds: Size =
    MetadataUtils.getBoundingRectangleInCanvasCoords(filteredSelectedElements, metadata) ??
    size(0, 0)

  const allElementsUnderPoint = getAllTargetsAtPointAABB(
    metadata,
    [],
    [],
    'no-filter',
    point,
    allElementProps,
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

  const filteredElementsUnderPoint = allElementsUnderPoint.filter(
    (target) =>
      // any of the dragged elements (or their flex parents) and their descendants are not game for reparenting
      filteredSelectedElementsMetadata.findIndex((maybeAncestorOrEqual) =>
        maybeAncestorOrEqual.specialSizeMeasurements.parentLayoutSystem === 'flex'
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
        )),
  )

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
      return rectContainsPoint(target, point)
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
  for (const elementPath of filteredElementsUnderPoint) {
    const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
    const isFlex = MetadataUtils.isFlexLayoutedContainer(element)

    if (!isFlex) {
      // TODO we now assume this is "absolute", but this is too vauge
      const providesBoundsForAbsoluteChildren =
        MetadataUtils.findElementByElementPath(metadata, elementPath)?.specialSizeMeasurements
          .providesBoundsForAbsoluteChildren ?? false

      const elementAcceptsAbsoluteChildren = providesBoundsForAbsoluteChildren
      if (elementAcceptsAbsoluteChildren) {
        return {
          shouldReparent: true,
          newParent: elementPath,
          shouldReorder: false,
          newIndex: -1,
        }
      } else {
        // this element did not support children, so let's continue the lookup
        continue
      }
    } else {
      const targets: Array<CanvasRectangle> = drawTargetRectanglesForChildrenOfElement(
        metadata,
        elementPath,
        'full-size',
      )

      const targetUnderMouseIndex = targets.findIndex((target) => {
        return rectContainsPointInclusive(target, point)
      })

      // found flex element, todo index
      return {
        shouldReparent: true,
        newParent: elementPath,
        shouldReorder: targetUnderMouseIndex > -1,
        newIndex: targetUnderMouseIndex,
      }
    }
  }

  // none of the targets were under the mouse, fallback return
  return {
    shouldReparent: false,
    shouldReorder: false,
    newParent: null,
    newIndex: -1,
  }
}

const propertiesToRemove: Array<PropertyPath> = [
  PP.create(['style', 'position']),
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
          } as any as CanvasPoint,
          {
            [leftOrTop]: paddedEnd,
            [leftOrTopComplement]:
              parentBounds[leftOrTopComplement] + parentBounds[widthOrHeightComplement],
          } as any as CanvasPoint,
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
          } as any as CanvasPoint,
          {
            [leftOrTop]: normalizedEnd,
            [leftOrTopComplement]:
              parentBounds[leftOrTopComplement] + parentBounds[widthOrHeightComplement],
          } as any as CanvasPoint,
        ),
      )
    }
  }

  return flexInsertionTargets
}

export function applyFlexReparent(
  stripAbsoluteProperties: 'strip-absolute-props' | 'do-not-strip-props',
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  strategyState: StrategyState,
  lifecycle: 'mid-interaction' | 'end-interaction',
): StrategyApplicationResult {
  const filteredSelectedElements = getDragTargets(canvasState.selectedElements)
  return ifAllowedToReparent(canvasState, strategyState, filteredSelectedElements, () => {
    if (
      interactionSession.interactionData.type == 'DRAG' &&
      interactionSession.interactionData.drag != null
    ) {
      const pointOnCanvas = offsetPoint(
        interactionSession.interactionData.originalDragStart,
        interactionSession.interactionData.drag,
      )
      const reparentResult = newGetReparentTarget(
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
          newParent,
        )

        // Strip the `position`, positional and dimension properties.
        const commandToRemoveProperties = stripAbsoluteProperties
          ? [
              deleteProperties('permanent', newPath, propertiesToRemove),
              setProperty('permanent', newPath, PP.create(['style', 'position']), 'relative'), // SPIKE TODO only insert position: relative if there was a position nonstatic prop before
            ]
          : []

        const commandsBeforeReorder = [
          ...reparentCommands,
          updateSelectedViews('permanent', [newPath]),
        ]

        const commandsAfterReorder = [
          ...commandToRemoveProperties,
          setElementsToRerenderCommand([target, newPath]),
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

        const newIndex = reparentResult.newIndex

        if (reparentResult.shouldReorder && newIndex < siblingsOfTarget.length) {
          // Reorder the newly reparented element into the flex ordering.
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
              canvas: { controls: { parentHighlightPaths: { $set: [newParent] } } },
            }),
            wildcardPatch('transient', {
              canvas: {
                controls: { flexReparentTargetLines: { $set: [targetLineBeforeSibling] } },
              },
            }),
            wildcardPatch('transient', {
              displayNoneInstances: { $push: [target] },
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
                canvas: { controls: { parentHighlightPaths: { $set: [newParent] } } },
              }),
              wildcardPatch('transient', {
                canvas: {
                  controls: { flexReparentTargetLines: { $set: [targetLineAfterSibling] } },
                },
              }),
              wildcardPatch('transient', {
                hiddenInstances: { $push: [target] },
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
                canvas: { controls: { parentHighlightPaths: { $set: [newParent] } } },
              }),
              wildcardPatch('transient', {
                canvas: {
                  controls: { flexReparentTargetLines: { $set: [targetLineBeginningOfParent] } },
                },
              }),
              wildcardPatch('transient', {
                hiddenInstances: { $push: [target] },
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
  })
}
