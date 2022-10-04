import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getStoryboardElementPath } from '../../../core/model/scene-utils'
import { mapDropNulls, reverse } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSXElement,
} from '../../../core/shared/element-template'
import {
  canvasPoint,
  CanvasPoint,
  CanvasRectangle,
  canvasRectangle,
  offsetPoint,
  pointDifference,
  rectContainsPoint,
  rectContainsPointInclusive,
  rectFromTwoPoints,
  roundPointToNearestHalf,
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
import {
  InteractionSession,
  MissingBoundsHandling,
  ReparentTargetsToFilter,
  StrategyState,
} from './interaction-state'
import { ifAllowedToReparent } from './reparent-helpers'
import { getReparentOutcome, pathToReparent } from './reparent-utils'
import { getDragTargets } from './shared-move-strategies-helpers'
import { absolute } from '../../../utils/utils'
import { getElementFromProjectContents } from '../../../components/editor/store/editor-state'
import { stylePropPathMappingFn } from '../../../components/inspector/common/property-path-hooks'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { LayoutPinnedProp, framePointForPinnedProp } from '../../../core/layout/layout-helpers-new'
import { isRight, right } from '../../../core/shared/either'
import { FlexDirection, isHorizontalPoint } from 'utopia-api/core'
import {
  AdjustCssLengthProperty,
  adjustCssLengthProperty,
} from '../commands/adjust-css-length-command'
import { updatePropIfExists } from '../commands/update-prop-if-exists-command'
import {
  flexDirectionToFlexForwardsOrBackwards,
  flexDirectionToSimpleFlexDirection,
  FlexForwardsOrBackwards,
  SimpleFlexDirection,
} from '../../../core/layout/layout-utils'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { assertNever } from '../../../core/shared/utils'

export type ReparentStrategy =
  | 'FLEX_REPARENT_TO_ABSOLUTE'
  | 'FLEX_REPARENT_TO_FLEX'
  | 'ABSOLUTE_REPARENT_TO_ABSOLUTE'
  | 'ABSOLUTE_REPARENT_TO_FLEX'

export type FindReparentStrategyResult =
  | { strategy: ReparentStrategy; newParent: ElementPath; forcingRequired: boolean }
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
  const parentIsFlexLayout = MetadataUtils.isFlexLayoutedContainer(newParentMetadata)

  const parentProvidesBoundsForAbsoluteChildren =
    newParentMetadata?.specialSizeMeasurements.providesBoundsForAbsoluteChildren ?? false

  const parentIsStoryboard = EP.isStoryboardPath(newParent)
  const isAbsoluteFriendlyParent = parentProvidesBoundsForAbsoluteChildren || parentIsStoryboard
  const forcingRequiredForAbsoluteReparent = !isAbsoluteFriendlyParent

  if (allDraggedElementsAbsolute) {
    if (parentIsFlexLayout) {
      return { strategy: 'ABSOLUTE_REPARENT_TO_FLEX', newParent: newParent, forcingRequired: false }
    } else {
      return {
        strategy: 'ABSOLUTE_REPARENT_TO_ABSOLUTE',
        newParent: newParent,
        forcingRequired: forcingRequiredForAbsoluteReparent,
      }
    }
  }
  if (allDraggedElementsFlex) {
    if (parentIsFlexLayout) {
      return { strategy: 'FLEX_REPARENT_TO_FLEX', newParent: newParent, forcingRequired: false }
    } else {
      return {
        strategy: 'FLEX_REPARENT_TO_ABSOLUTE',
        newParent: newParent,
        forcingRequired: forcingRequiredForAbsoluteReparent,
      }
    }
  }
  return { strategy: 'do-not-reparent' }
}

function isReparentToAbsoluteStrategy(reparentStrategy: ReparentStrategy): boolean {
  switch (reparentStrategy) {
    case 'ABSOLUTE_REPARENT_TO_ABSOLUTE':
    case 'FLEX_REPARENT_TO_ABSOLUTE':
      return true
    case 'ABSOLUTE_REPARENT_TO_FLEX':
    case 'FLEX_REPARENT_TO_FLEX':
      return false
    default:
      assertNever(reparentStrategy)
  }
}

export function getFitnessForReparentStrategy(
  reparentStrategy: ReparentStrategy,
  canvasState: InteractionCanvasState,
  interactionState: InteractionSession,
  strategyState: StrategyState,
  missingBoundsHandling: MissingBoundsHandling,
): number {
  const isForced = missingBoundsHandling === 'allow-missing-bounds'
  const allowAsFallback = isReparentToAbsoluteStrategy(reparentStrategy) && !isForced

  const foundReparentStrategy = findReparentStrategy(
    canvasState,
    interactionState,
    strategyState,
    missingBoundsHandling,
  )
  if (
    foundReparentStrategy.strategy === reparentStrategy &&
    foundReparentStrategy.forcingRequired === isForced
  ) {
    return isForced ? 0.5 : 3
  } else if (foundReparentStrategy.strategy !== 'do-not-reparent' && allowAsFallback) {
    return 2
  } else {
    return 0
  }
}

function targetIsValid(
  newParentPath: ElementPath,
  targetsToFilterOut: ReparentTargetsToFilter | null,
  missingBoundsHandling: MissingBoundsHandling,
): boolean {
  if (targetsToFilterOut == null) {
    return true
  } else {
    const targetToFilter = targetsToFilterOut[missingBoundsHandling].newParent
    return !EP.pathsEqual(targetToFilter, newParentPath)
  }
}

function findReparentStrategy(
  canvasState: InteractionCanvasState,
  interactionState: InteractionSession,
  strategyState: StrategyState,
  missingBoundsHandling: MissingBoundsHandling,
): FindReparentStrategyResult {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (
    selectedElements.length === 0 ||
    interactionState.activeControl.type !== 'BOUNDING_AREA' ||
    interactionState.interactionData.type !== 'DRAG' ||
    interactionState.interactionData.drag == null || // TODO delete this drag nullcheck? do we start the reparent on mouse down or mouse move beyond threshold?
    interactionState.interactionData.modifiers.alt
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
    existingReparentSubjects(filteredSelectedElements),
    pointOnCanvas,
    cmdPressed,
    canvasState,
    strategyState.startingMetadata,
    strategyState.startingAllElementProps,
    missingBoundsHandling,
  )

  const newParentPath = reparentResult.newParent

  const parentStayedTheSame = filteredSelectedElements.some(
    (e) => EP.parentPath(e) === newParentPath,
  )

  if (
    reparentResult.shouldReparent &&
    newParentPath != null &&
    targetIsValid(
      newParentPath,
      interactionState.startingTargetParentsToFilterOut,
      missingBoundsHandling,
    ) &&
    !parentStayedTheSame
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

type ReparentSubjects = NewReparentSubjects | ExistingReparentSubjects

export interface NewReparentSubjects {
  type: 'NEW_ELEMENTS'
}

export function newReparentSubjects(): NewReparentSubjects {
  return {
    type: 'NEW_ELEMENTS',
  }
}

export interface ExistingReparentSubjects {
  type: 'EXISTING_ELEMENTS'
  elements: Array<ElementPath>
}

export function existingReparentSubjects(elements: Array<ElementPath>): ExistingReparentSubjects {
  return {
    type: 'EXISTING_ELEMENTS',
    elements: elements,
  }
}

export function getReparentTargetUnified(
  reparentSubjects: ReparentSubjects,
  pointOnCanvas: CanvasPoint,
  cmdPressed: boolean,
  canvasState: InteractionCanvasState,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  missingBoundsHandling: MissingBoundsHandling,
): ReparentTarget {
  const projectContents = canvasState.projectContents
  const openFile = canvasState.openFile ?? null
  const canvasScale = canvasState.scale

  const multiselectBounds: Size =
    (reparentSubjects.type === 'EXISTING_ELEMENTS'
      ? MetadataUtils.getBoundingRectangleInCanvasCoords(reparentSubjects.elements, metadata)
      : null) ?? size(0, 0)

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

  const filteredElementsUnderPoint = allElementsUnderPoint.filter((target) => {
    let validParentForFlexOrAbsolute = cmdPressed
    if (missingBoundsHandling === 'allow-missing-bounds') {
      validParentForFlexOrAbsolute = true
    } else {
      const targetMetadata = MetadataUtils.findElementByElementPath(metadata, target)
      const isFlex = MetadataUtils.isFlexLayoutedContainer(targetMetadata)
      const providesBoundsForAbsoluteChildren =
        targetMetadata?.specialSizeMeasurements.providesBoundsForAbsoluteChildren ?? false

      // TODO extend here when we implement static layout support
      validParentForFlexOrAbsolute = isFlex || providesBoundsForAbsoluteChildren
    }

    const canReparent =
      // simply skip elements that do not support children
      MetadataUtils.targetSupportsChildren(projectContents, openFile, metadata, target) &&
      // if cmd is not pressed, we only allow reparent to parents that are larger than the multiselect bounds
      (cmdPressed ||
        sizeFitsInTarget(
          multiselectBounds,
          MetadataUtils.getFrameInCanvasCoords(target, metadata) ?? size(0, 0),
        )) &&
      validParentForFlexOrAbsolute

    if (reparentSubjects.type === 'EXISTING_ELEMENTS') {
      const selectedElementsMetadata = mapDropNulls(
        (path) => MetadataUtils.findElementByElementPath(metadata, path),
        reparentSubjects.elements,
      )

      // TODO BEFORE MERGE consider multiselect!!!!!
      // the current parent should be included in the array of valid targets
      return (
        selectedElementsMetadata.some((maybeChild) =>
          EP.isChildOf(maybeChild.elementPath, target),
        ) ||
        // any of the dragged elements (or their flex parents) and their descendants are not game for reparenting
        (!selectedElementsMetadata.some((maybeAncestorOrEqual) =>
          !cmdPressed && maybeAncestorOrEqual.specialSizeMeasurements.parentLayoutSystem === 'flex'
            ? // for Flex children, we also want to filter out all their siblings to force a Flex Reorder strategy
              EP.isDescendantOf(target, EP.parentPath(maybeAncestorOrEqual.elementPath))
            : // for non-flex elements, we filter out their descendants and themselves
              EP.isDescendantOfOrEqualTo(target, maybeAncestorOrEqual.elementPath),
        ) &&
          canReparent)
      )
    } else {
      return canReparent
    }
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
      canvasScale,
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
      canvasScale,
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
  canvasScale: number,
): Array<CanvasRectangle> {
  const ExtraPadding = 10 / canvasScale

  const flexElement = MetadataUtils.findElementByElementPath(metadata, flexElementPath)
  const flexDirection = MetadataUtils.getFlexDirection(flexElement)
  const simpleFlexDirection = flexDirectionToSimpleFlexDirection(flexDirection)
  const forwardsOrBackwards = flexDirectionToFlexForwardsOrBackwards(flexDirection)
  const parentBounds = MetadataUtils.getFrameInCanvasCoords(flexElementPath, metadata)

  if (parentBounds == null || simpleFlexDirection == null || forwardsOrBackwards == null) {
    // TODO should we throw an error?
    return []
  }

  const leftOrTop = simpleFlexDirection === 'row' ? 'x' : 'y'
  const leftOrTopComplement = simpleFlexDirection === 'row' ? 'y' : 'x'
  const widthOrHeight = simpleFlexDirection === 'row' ? 'width' : 'height'
  const widthOrHeightComplement = simpleFlexDirection === 'row' ? 'height' : 'width'

  interface ElemBounds {
    start: number
    size: number
    end: number
  }

  const pseudoElementBefore: ElemBounds = {
    start: parentBounds[leftOrTop],
    size: 0,
    end: parentBounds[leftOrTop],
  }
  const pseudoElementAfter: ElemBounds = {
    start: parentBounds[leftOrTop] + parentBounds[widthOrHeight],
    size: 0,
    end: parentBounds[leftOrTop] + parentBounds[widthOrHeight],
  }

  const childrenBounds: Array<ElemBounds> = MetadataUtils.getChildrenPaths(
    metadata,
    flexElementPath,
  ).map((childPath) => {
    const bounds = MetadataUtils.getFrameInCanvasCoords(childPath, metadata)!
    return {
      start: bounds[leftOrTop],
      size: bounds[widthOrHeight],
      end: bounds[leftOrTop] + bounds[widthOrHeight],
    }
  })

  const childrenBoundsAlongAxis: Array<ElemBounds> = [
    pseudoElementBefore,
    ...(forwardsOrBackwards === 'forward' ? childrenBounds : reverse(childrenBounds)),
    pseudoElementAfter,
  ]

  let flexInsertionTargets: Array<CanvasRectangle> = []

  if (targetRectangleSize === 'padded-edge') {
    for (let index = 0; index < childrenBoundsAlongAxis.length - 1; index++) {
      const start = childrenBoundsAlongAxis[index].end
      const end = childrenBoundsAlongAxis[index + 1].start

      const normalizedStart = Math.min(start, end)
      const normalizedEnd = Math.max(start, end)

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

export function getSiblingMidPointPosition(
  precedingSiblingPosition: CanvasRectangle,
  succeedingSiblingPosition: CanvasRectangle,
  direction: SimpleFlexDirection,
): number {
  let getSiblingPosition: (rect: CanvasRectangle) => number
  let getSiblingSize: (rect: CanvasRectangle) => number
  switch (direction) {
    case 'row':
      getSiblingPosition = (rect: CanvasRectangle) => {
        return rect.x
      }
      getSiblingSize = (rect: CanvasRectangle) => {
        return rect.width
      }
      break
    case 'column':
      getSiblingPosition = (rect: CanvasRectangle) => {
        return rect.y
      }
      getSiblingSize = (rect: CanvasRectangle) => {
        return rect.height
      }
      break
    default:
      const _exhaustiveCheck: never = direction
      throw new Error(`Unhandled direction of ${JSON.stringify(direction)}`)
  }

  const value =
    (getSiblingPosition(precedingSiblingPosition) +
      getSiblingSize(precedingSiblingPosition) +
      getSiblingPosition(succeedingSiblingPosition)) /
    2
  return value
}

export function siblingAndPseudoPositions(
  parentFlexDirection: SimpleFlexDirection,
  forwardsOrBackwards: FlexForwardsOrBackwards,
  parentRect: CanvasRectangle,
  siblingsOfTarget: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
) {
  const pseudoElementBefore: CanvasRectangle =
    parentFlexDirection === 'row' // TODO handle row-reverse or col-reverse
      ? canvasRectangle({
          x: parentRect.x,
          y: parentRect.y,
          width: 0,
          height: parentRect.height,
        })
      : canvasRectangle({
          x: parentRect.x,
          y: parentRect.y,
          height: 0,
          width: parentRect.width,
        })

  const pseudoElementAfter: CanvasRectangle =
    parentFlexDirection === 'row' // TODO handle row-reverse or col-reverse
      ? canvasRectangle({
          x: parentRect.x + parentRect.width,
          y: parentRect.y,
          width: 0,
          height: parentRect.height,
        })
      : canvasRectangle({
          x: parentRect.x,
          y: parentRect.y + parentRect.height,
          height: 0,
          width: parentRect.width,
        })

  const siblingsPossiblyReversed =
    forwardsOrBackwards === 'forward' ? siblingsOfTarget : reverse(siblingsOfTarget)
  const siblingPositions: Array<CanvasRectangle> = [
    pseudoElementBefore,
    ...siblingsPossiblyReversed.map((sibling) => {
      return MetadataUtils.getFrameInCanvasCoords(sibling, metadata) ?? zeroCanvasRect
    }),
    pseudoElementAfter,
  ]
  return siblingPositions
}

export function applyFlexReparent(
  stripAbsoluteProperties: StripAbsoluteProperties,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  strategyState: StrategyState,
): StrategyApplicationResult {
  const FlexReparentIndicatorSize = 2 / canvasState.scale

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
        existingReparentSubjects(filteredSelectedElements),
        pointOnCanvas,
        interactionSession.interactionData.modifiers.cmd,
        canvasState,
        strategyState.startingMetadata,
        strategyState.startingAllElementProps,
        'use-strict-bounds',
      )

      if (
        reparentResult.shouldReparent &&
        reparentResult.newParent != null &&
        filteredSelectedElements.length === 1
      ) {
        const target = filteredSelectedElements[0]

        const newIndex = reparentResult.newIndex
        const newParent = reparentResult.newParent
        const newParentMetadata = MetadataUtils.findElementByElementPath(
          strategyState.startingMetadata,
          newParent,
        )
        const parentRect =
          MetadataUtils.getFrameInCanvasCoords(newParent, strategyState.startingMetadata) ??
          zeroCanvasRect
        const flexDirection = MetadataUtils.getFlexDirection(newParentMetadata)
        const newParentFlexDirection = forceNotNull(
          'Should have a valid flex direction.',
          flexDirectionToSimpleFlexDirection(flexDirection),
        )
        const forwardsOrBackwards = forceNotNull(
          'Should have a valid flex orientation.',
          flexDirectionToFlexForwardsOrBackwards(flexDirection),
        )

        const siblingsOfTarget = MetadataUtils.getChildrenPaths(
          strategyState.startingMetadata,
          newParent,
        )

        const newParentADescendantOfCurrentParent = EP.isDescendantOfOrEqualTo(
          newParent,
          EP.parentPath(target),
        )
        // Reparent the element.
        const outcomeResult = getReparentOutcome(
          canvasState.builtInDependencies,
          canvasState.projectContents,
          canvasState.nodeModules,
          canvasState.openFile,
          pathToReparent(target),
          newParent,
          'always',
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
            setElementsToRerenderCommand([target, newPath]),
            updateHighlightedViews('mid-interaction', []),
            setCursorCommand('mid-interaction', CSSCursor.Move),
          ]

          function midInteractionCommandsForTarget(
            targetLine: CanvasRectangle,
          ): Array<CanvasCommand> {
            return [
              wildcardPatch('mid-interaction', {
                canvas: { controls: { parentHighlightPaths: { $set: [newParent] } } },
              }),
              wildcardPatch('mid-interaction', {
                canvas: {
                  controls: { flexReparentTargetLines: { $set: [targetLine] } },
                },
              }),
              newParentADescendantOfCurrentParent
                ? wildcardPatch('mid-interaction', {
                    hiddenInstances: { $push: [target] },
                  })
                : wildcardPatch('mid-interaction', {
                    displayNoneInstances: { $push: [target] },
                  }),
              wildcardPatch('mid-interaction', { displayNoneInstances: { $push: [newPath] } }),
            ]
          }

          let interactionFinishCommands: Array<CanvasCommand>
          let midInteractionCommands: Array<CanvasCommand>

          if (reparentResult.shouldReorder && siblingsOfTarget.length > 0) {
            const siblingPositions: Array<CanvasRectangle> = siblingAndPseudoPositions(
              newParentFlexDirection,
              forwardsOrBackwards,
              parentRect,
              siblingsOfTarget,
              strategyState.startingMetadata,
            )

            const precedingSiblingPosition: CanvasRectangle = siblingPositions[newIndex]
            const succeedingSiblingPosition: CanvasRectangle = siblingPositions[newIndex + 1]

            const targetLineBeforeSibling: CanvasRectangle =
              newParentFlexDirection === 'row'
                ? canvasRectangle({
                    x:
                      getSiblingMidPointPosition(
                        precedingSiblingPosition,
                        succeedingSiblingPosition,
                        'row',
                      ) -
                      FlexReparentIndicatorSize / 2,
                    y: (precedingSiblingPosition.y + succeedingSiblingPosition.y) / 2,
                    height:
                      (precedingSiblingPosition.height + succeedingSiblingPosition.height) / 2,
                    width: FlexReparentIndicatorSize,
                  })
                : canvasRectangle({
                    x: (precedingSiblingPosition.x + succeedingSiblingPosition.x) / 2,
                    y:
                      getSiblingMidPointPosition(
                        precedingSiblingPosition,
                        succeedingSiblingPosition,
                        'column',
                      ) -
                      FlexReparentIndicatorSize / 2,
                    width: (precedingSiblingPosition.width + succeedingSiblingPosition.width) / 2,
                    height: FlexReparentIndicatorSize,
                  })

            midInteractionCommands = midInteractionCommandsForTarget(targetLineBeforeSibling)

            interactionFinishCommands = [
              ...commandsBeforeReorder,
              reorderElement('always', newPath, absolute(newIndex)),
              ...commandsAfterReorder,
            ]
          } else {
            if (parentRect != null) {
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

              midInteractionCommands = midInteractionCommandsForTarget(targetLineBeginningOfParent)
            } else {
              // this should be an error because parentRect should never be null
              midInteractionCommands = []
            }

            interactionFinishCommands = [...commandsBeforeReorder, ...commandsAfterReorder]
          }

          return {
            commands: [...midInteractionCommands, ...interactionFinishCommands],
            customStatePatch: {},
            status: 'success',
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

  const currentParentContentBox = MetadataUtils.getParentCoordinateSystemBounds(
    EP.parentPath(target),
    targetStartingMetadata,
  )
  const newParentContentBox = MetadataUtils.getParentCoordinateSystemBounds(
    newParent,
    newParentStartingMetadata,
  )

  const offsetTL = roundPointToNearestHalf(
    pointDifference(newParentContentBox, currentParentContentBox),
  )
  const offsetBR = roundPointToNearestHalf(
    pointDifference(
      canvasPoint({
        x: currentParentContentBox.x + currentParentContentBox.width,
        y: currentParentContentBox.y + currentParentContentBox.height,
      }),
      canvasPoint({
        x: newParentContentBox.x + newParentContentBox.width,
        y: newParentContentBox.y + newParentContentBox.height,
      }),
    ),
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
        deleteProperties('always', newPath, propertiesToRemove),
        updatePropIfExists('always', newPath, PP.create(['style', 'position']), 'relative'), // SPIKE TODO only insert position: relative if there was a position nonstatic prop before
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
