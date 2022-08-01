import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getStoryboardElementPath } from '../../../core/model/scene-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import {
  canvasPoint,
  CanvasPoint,
  canvasRectangle,
  CanvasRectangle,
  normalizeRect,
  offsetPoint,
  rectContainsPoint,
  rectFromTwoPoints,
  zeroCanvasRect,
} from '../../../core/shared/math-utils'
import { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { ProjectContentTreeRoot } from '../../assets'
import { AllElementProps } from '../../editor/store/editor-state'
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
import { getAllTargetsAtPointAABB } from '../dom-lookup'
import {
  emptyStrategyApplicationResult,
  InteractionCanvasState,
  StrategyApplicationResult,
} from './canvas-strategy-types'
import { getReorderIndex } from './flex-reorder-strategy'
import { InteractionSession, StrategyState } from './interaction-state'
import { ifAllowedToReparent } from './reparent-helpers'
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
  newIndex: number
} {
  if (
    interactionSession.interactionData.type !== 'DRAG' ||
    interactionSession.interactionData.drag == null
  ) {
    return {
      shouldReparent: false,
      newParent: null,
      shouldReorder: false,
      newIndex: -1,
    }
  }

  const pointOnCanvas = offsetPoint(
    interactionSession.interactionData.originalDragStart,
    interactionSession.interactionData.drag,
  )

  const flexReparentResult = newGetReparentTarget(
    strategyState.startingMetadata,
    strategyState.startingAllElementProps,
    canvasState.projectContents,
    canvasState.openFile ?? null,
    pointOnCanvas,
  )

  return flexReparentResult
}

function newGetReparentTarget(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  projectContents: ProjectContentTreeRoot,
  openFile: string | null,
  point: CanvasPoint,
): {
  shouldReparent: boolean
  newParent: ElementPath | null
  shouldReorder: boolean
  newIndex: number
} {
  const elementsUnderPoint = getAllTargetsAtPointAABB(
    metadata,
    [],
    [],
    'no-filter',
    point,
    allElementProps,
  )

  // if the mouse is over the canvas, return the canvas root as the target path
  if (elementsUnderPoint.length === 0) {
    const storyboardComponent = getStoryboardElementPath(projectContents, openFile)
    return {
      shouldReparent: storyboardComponent != null,
      newParent: storyboardComponent,
      shouldReorder: false,
      newIndex: -1,
    }
  }

  const flexElementsUnderPoint = [...elementsUnderPoint]
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
  for (const elementPath of elementsUnderPoint) {
    const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
    const isFlex = MetadataUtils.isFlexLayoutedContainer(element)
    if (!isFlex) {
      // TODO we now assume this is "absolute", but this is too vauge
      // TODO check if element accepts children
      return {
        shouldReparent: true,
        newParent: elementPath,
        shouldReorder: false,
        newIndex: -1,
      }
    } else {
      // found flex element, todo index
      return {
        shouldReparent: true,
        newParent: elementPath,
        shouldReorder: false,
        newIndex: -1,
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

  const pseudoElementBefore = { start: parentBounds[leftOrTop], end: parentBounds[leftOrTop] }
  const pseudoElementAfter = {
    start: parentBounds[leftOrTop] + parentBounds[widthOrHeight],
    end: parentBounds[leftOrTop] + parentBounds[widthOrHeight],
  }

  const childrenBounds: Array<{ start: number; end: number }> = MetadataUtils.getChildrenPaths(
    metadata,
    flexElementPath,
  ).map((childPath) => {
    const bounds = MetadataUtils.getFrameInCanvasCoords(childPath, metadata)!
    return {
      start: bounds[leftOrTop],
      end: bounds[leftOrTop] + bounds[widthOrHeight],
    }
  })

  const childrenBoundsAlongAxis: Array<{ start: number; end: number }> = [
    pseudoElementBefore,
    ...childrenBounds,
    pseudoElementAfter,
  ]

  let flexInsertionTargets: Array<CanvasRectangle> = []
  for (let index = 0; index < childrenBoundsAlongAxis.length - 1; index++) {
    const start = childrenBoundsAlongAxis[index].end + (index === 0 ? 0 : -5)
    const end = childrenBoundsAlongAxis[index + 1].start

    const normalizedStart = Math.min(start, end)
    const normalizedEnd = Math.max(start, end)

    const ExtraPadding = 5

    const paddedStart = normalizedStart - ExtraPadding
    const paddedEnd = normalizedEnd + ExtraPadding

    // TODO for tomorrow this code doesn't find the drop zone _after_ the last element
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

        const newIndex = reparentResult.newIndex

        if (reparentResult.shouldReorder && newIndex < siblingsOfTarget.length) {
          // Reorder the newly reparented element into the flex ordering.
          const pointOnCanvas = offsetPoint(
            interactionSession.interactionData.dragStart,
            interactionSession.interactionData.drag,
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
              canvas: { controls: { parentHighlightPaths: { $set: [newParent] } } },
            }),
            wildcardPatch('transient', {
              canvas: {
                controls: { flexReparentTargetLines: { $set: [targetLineBeforeSibling] } },
              },
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
