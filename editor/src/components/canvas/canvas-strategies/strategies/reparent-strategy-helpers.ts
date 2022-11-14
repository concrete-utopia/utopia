import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import {
  framePointForPinnedProp,
  LayoutPinnedProp,
} from '../../../../core/layout/layout-helpers-new'
import { FlexForwardsOrBackwards, SimpleFlexDirection } from '../../../../core/layout/layout-utils'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { getStoryboardElementPath } from '../../../../core/model/scene-utils'
import { mapDropNulls, reverse } from '../../../../core/shared/array-utils'
import { isRight, right } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSXElement,
} from '../../../../core/shared/element-template'
import {
  canvasPoint,
  CanvasPoint,
  CanvasRectangle,
  canvasRectangle,
  pointDifference,
  rectContainsPoint,
  rectContainsPointInclusive,
  rectFromTwoPoints,
  roundPointToNearestHalf,
  Size,
  size,
  sizeFitsInTarget,
  zeroCanvasRect,
} from '../../../../core/shared/math-utils'
import { ElementPath, PropertyPath } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import { absolute } from '../../../../utils/utils'
import { ProjectContentTreeRoot } from '../../../assets'
import { AllElementProps, getElementFromProjectContents } from '../../../editor/store/editor-state'
import { CSSPosition } from '../../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { CSSCursor } from '../../canvas-types'
import {
  AdjustCssLengthProperty,
  adjustCssLengthProperty,
} from '../../commands/adjust-css-length-command'
import { CanvasCommand } from '../../commands/commands'
import { deleteProperties } from '../../commands/delete-properties-command'
import { reorderElement } from '../../commands/reorder-element-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setProperty } from '../../commands/set-property-command'
import { showReorderIndicator } from '../../commands/show-reorder-indicator-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import { getAllTargetsAtPointAABB } from '../../dom-lookup'
import {
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  InteractionTarget,
  StrategyApplicationResult,
} from '../canvas-strategy-types'
import { AllowSmallerParent, InteractionSession } from '../interaction-state'
import {
  getOptionalCommandToConvertDisplayInlineBlock,
  SingleAxisAutolayoutContainerDirections,
  singleAxisAutoLayoutContainerDirections,
} from './flow-reorder-helpers'
import { ifAllowedToReparent } from './reparent-helpers'
import { getReparentOutcome, pathToReparent } from './reparent-utils'
import { getDragTargets } from './shared-move-strategies-helpers'

export type ReparentStrategy = 'REPARENT_AS_ABSOLUTE' | 'REPARENT_AS_STATIC'

export type FindReparentStrategyResult = {
  strategy: ReparentStrategy
  isFallback: boolean
  target: ReparentTarget
}

export function reparentStrategyForParent(
  targetMetadata: ElementInstanceMetadataMap,
  parent: ElementPath,
  convertToAbsolute: boolean,
): {
  strategy: ReparentStrategy
  isFallback: boolean
} {
  const newParentMetadata = MetadataUtils.findElementByElementPath(targetMetadata, parent)
  const parentIsFlexLayout = MetadataUtils.isFlexLayoutedContainer(newParentMetadata)

  const flowParentReparentType = flowParentAbsoluteOrStatic(targetMetadata, parent)
  const reparentAsStatic =
    !convertToAbsolute && (parentIsFlexLayout || flowParentReparentType === 'REPARENT_AS_STATIC')

  if (reparentAsStatic) {
    return {
      strategy: 'REPARENT_AS_STATIC',
      isFallback: false,
    }
  } else {
    return {
      strategy: 'REPARENT_AS_ABSOLUTE',
      isFallback: convertToAbsolute,
    }
  }
}

function isSingleAxisAutoLayoutComaptibleWithReorder(
  metadata: ElementInstanceMetadataMap,
  parent: ElementPath,
): boolean {
  const newParentMetadata = MetadataUtils.findElementByElementPath(metadata, parent)
  const parentIsFlexLayout = MetadataUtils.isFlexLayoutedContainer(newParentMetadata)
  if (parentIsFlexLayout) {
    return true
  }

  const flowChildren = MetadataUtils.getChildren(metadata, parent).filter(
    (child) => child.specialSizeMeasurements.position !== 'absolute',
  )

  return flowChildren.length > 1
}

function autoLayoutParentAbsoluteOrStatic(
  metadata: ElementInstanceMetadataMap,
  parent: ElementPath,
): ReparentStrategy {
  const newParentMetadata = MetadataUtils.findElementByElementPath(metadata, parent)
  const parentIsFlexLayout = MetadataUtils.isFlexLayoutedContainer(newParentMetadata)

  if (parentIsFlexLayout) {
    return 'REPARENT_AS_STATIC'
  }

  return flowParentAbsoluteOrStatic(metadata, parent)
}

function flowParentAbsoluteOrStatic(
  metadata: ElementInstanceMetadataMap,
  parent: ElementPath,
): ReparentStrategy {
  const parentMetadata = MetadataUtils.findElementByElementPath(metadata, parent)
  const children = MetadataUtils.getChildren(metadata, parent)

  const storyboardRoot = EP.isStoryboardPath(parent)
  if (storyboardRoot) {
    // always reparent as absolute to the Storyboard
    return 'REPARENT_AS_ABSOLUTE'
  }

  if (parentMetadata == null) {
    throw new Error('flowParentAbsoluteOrStatic: parentMetadata was null')
  }

  const parentIsContainingBlock =
    parentMetadata.specialSizeMeasurements.providesBoundsForAbsoluteChildren
  if (!parentIsContainingBlock) {
    return 'REPARENT_AS_STATIC'
  }

  const anyChildrenFlow = children.some(
    (child) => child.specialSizeMeasurements.position === 'static',
  )
  if (anyChildrenFlow) {
    return 'REPARENT_AS_STATIC'
  }

  const allChildrenPositionedAbsolutely =
    children.length > 0 &&
    children.every((child) => child.specialSizeMeasurements.position === 'absolute')
  if (allChildrenPositionedAbsolutely) {
    return 'REPARENT_AS_ABSOLUTE'
  }

  const emptyParentWithDimensionsGreaterThanZero =
    children.length === 0 &&
    (parentMetadata.globalFrame?.width ?? 0) > 0 &&
    (parentMetadata.globalFrame?.height ?? 0) > 0
  if (emptyParentWithDimensionsGreaterThanZero) {
    return 'REPARENT_AS_ABSOLUTE'
  }

  // TODO ABSOLUTE drag onto the padded area of flow layout target parent

  // TODO is this needed?
  const emptyParentWithAnyDimensionZero =
    children.length === 0 &&
    ((parentMetadata.globalFrame?.width ?? 0) === 0 ||
      (parentMetadata.globalFrame?.height ?? 0) === 0)
  if (emptyParentWithAnyDimensionZero) {
    return 'REPARENT_AS_STATIC'
  }

  // the fallback is reparent as static
  return 'REPARENT_AS_STATIC'

  // should there be a DO_NOT_REPARENT return type here?
}

export function findReparentStrategies(
  canvasState: InteractionCanvasState,
  cmdPressed: boolean,
  pointOnCanvas: CanvasPoint,
  allowSmallerParent: AllowSmallerParent,
): Array<FindReparentStrategyResult> {
  const metadata = canvasState.startingMetadata
  const reparentSubjects = reparentSubjectsForInteractionTarget(canvasState.interactionTarget)
  const targetParent = getReparentTargetUnified(
    reparentSubjects,
    pointOnCanvas,
    cmdPressed,
    canvasState,
    metadata,
    canvasState.startingAllElementProps,
    allowSmallerParent,
  )

  if (targetParent == null) {
    return []
  }

  const strategy = {
    target: targetParent,
    strategy: targetParent.defaultReparentType,
    isFallback: false,
  }

  const fallbackStrategy: FindReparentStrategyResult = {
    isFallback: true,
    target: strategy.target,
    strategy:
      strategy.strategy === 'REPARENT_AS_ABSOLUTE'
        ? 'REPARENT_AS_STATIC' // in case of an absolute reparent to a flow parent, we want to offer a secondary option to reparent as static
        : 'REPARENT_AS_ABSOLUTE', // in case of a static reparent, we want to offer a secondary option to force absolute.
  }

  return [strategy, fallbackStrategy]
}

export interface ReparentTarget {
  shouldReparent: boolean
  newParent: ElementPath
  shouldReorder: boolean
  newIndex: number
  shouldConvertToInline: 'row' | 'column' | 'do-not-convert'
  defaultReparentType: ReparentStrategy
}

export function reparentTarget(
  shouldReparent: boolean,
  newParent: ElementPath,
  shouldReorder: boolean,
  newIndex: number,
  shouldConvertToInline: 'row' | 'column' | 'do-not-convert',
  defaultReparentType: ReparentStrategy,
): ReparentTarget {
  return {
    shouldReparent: shouldReparent,
    newParent: newParent,
    shouldReorder: shouldReorder,
    newIndex: newIndex,
    shouldConvertToInline: shouldConvertToInline,
    defaultReparentType: defaultReparentType,
  }
}

type ReparentSubjects = NewReparentSubjects | ExistingReparentSubjects

// FIXME Does it ever make sense for this to refer to more than one element?
export interface NewReparentSubjects {
  type: 'NEW_ELEMENTS'
  defaultSize: Size
}

export function newReparentSubjects(defaultSize: Size): NewReparentSubjects {
  return {
    type: 'NEW_ELEMENTS',
    defaultSize: defaultSize,
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

export function reparentSubjectsForInteractionTarget(
  interactionTarget: InteractionTarget,
): ReparentSubjects {
  switch (interactionTarget.type) {
    case 'INSERTION_SUBJECTS':
      return newReparentSubjects(interactionTarget.subjects[0].defaultSize)
    case 'TARGET_PATHS':
      return existingReparentSubjects(
        getDragTargets(getTargetPathsFromInteractionTarget(interactionTarget)),
      )
    default:
      const _exhaustiveCheck: never = interactionTarget
      throw new Error(`Unhandled interaction target type ${JSON.stringify(interactionTarget)}`)
  }
}

export function getReparentTargetUnified(
  reparentSubjects: ReparentSubjects,
  pointOnCanvas: CanvasPoint,
  cmdPressed: boolean, // TODO: this should be removed from here and replaced by meaningful flag(s) (similar to allowSmallerParent)
  canvasState: InteractionCanvasState,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  allowSmallerParent: AllowSmallerParent,
): ReparentTarget | null {
  const canvasScale = canvasState.scale

  const validTargetparentsUnderPoint = findValidTargetsUnderPoint(
    reparentSubjects,
    pointOnCanvas,
    cmdPressed,
    canvasState,
    metadata,
    allElementProps,
    allowSmallerParent,
  )

  // For Flex parents, we want to be able to insert between two children that don't have a gap between them.
  const targetParentWithPaddedInsertionZone: ReparentTarget | null =
    findParentByPaddedInsertionZone(
      metadata,
      validTargetparentsUnderPoint,
      canvasScale,
      pointOnCanvas,
    )

  if (targetParentWithPaddedInsertionZone != null) {
    return targetParentWithPaddedInsertionZone
  }

  // fall back to trying to find an absolute element, or the "background" area of an autolayout container
  const targetParentPath = validTargetparentsUnderPoint[0]
  if (targetParentPath == null) {
    // none of the targets were under the mouse, fallback return
    return null
  }

  const targetParentUnderPoint: ReparentTarget = findParentUnderPointByArea(
    targetParentPath,
    metadata,
    canvasScale,
    pointOnCanvas,
  )
  return targetParentUnderPoint
}

function findValidTargetsUnderPoint(
  reparentSubjects: ReparentSubjects,
  pointOnCanvas: CanvasPoint,
  cmdPressed: boolean, // TODO: this should be removed from here and replaced by meaningful flag(s) (similar to allowSmallerParent)
  canvasState: InteractionCanvasState,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  allowSmallerParent: AllowSmallerParent,
): Array<ElementPath> {
  const projectContents = canvasState.projectContents
  const openFile = canvasState.openFile ?? null

  const storyboardComponent = getStoryboardElementPath(projectContents, openFile)
  if (storyboardComponent == null) {
    return []
  }

  const multiselectBounds: Size =
    (reparentSubjects.type === 'EXISTING_ELEMENTS'
      ? MetadataUtils.getBoundingRectangleInCanvasCoords(reparentSubjects.elements, metadata)
      : reparentSubjects.defaultSize) ?? size(0, 0)

  const allElementsUnderPoint = [
    ...getAllTargetsAtPointAABB(
      metadata,
      [],
      [],
      'no-filter',
      pointOnCanvas,
      allElementProps,
      false,
    ),
    storyboardComponent,
  ]

  const possibleTargetParentsUnderPoint = allElementsUnderPoint.filter((target) => {
    const currentParent = isTargetAParentOfAnySubject(reparentSubjects, metadata, target)

    if (currentParent) {
      // the current parent should be included in the array of valid targets
      return true
    }

    if (!MetadataUtils.targetSupportsChildren(projectContents, openFile, metadata, target)) {
      // simply skip elements that do not support children
      return false
    }

    const sizeFitsTarget =
      allowSmallerParent === 'allow-smaller-parent' ||
      sizeFitsInTarget(
        multiselectBounds,
        MetadataUtils.getFrameInCanvasCoords(target, metadata) ?? size(0, 0),
      )

    if (!sizeFitsTarget) {
      // skip elements that are smaller than the dragged elements, unless 'allow-smaller-parent'
      return false
    }

    if (reparentSubjects.type === 'NEW_ELEMENTS') {
      return true
    }

    const selectedElementsMetadata = mapDropNulls(
      (path) => MetadataUtils.findElementByElementPath(metadata, path),
      reparentSubjects.elements,
    )

    if (
      isTargetOutsideOfContainingComponentUnderMouse(
        selectedElementsMetadata,
        allElementsUnderPoint,
        target,
      )
    ) {
      return false
    }

    const isTargetParentSiblingOrDescendantOfSubjects = selectedElementsMetadata.some(
      (maybeAncestorOrEqual) => {
        // Note: in this function, true means "not suitable for reparent"
        const isChildOfReparentSubject = EP.isDescendantOfOrEqualTo(
          target,
          maybeAncestorOrEqual.elementPath,
        )
        if (isChildOfReparentSubject) {
          // any of the dragged elements and their descendants are not game for reparenting
          return true
        }

        const targetParticipatesInAutolayout =
          maybeAncestorOrEqual.specialSizeMeasurements.position !== 'absolute' // TODO also use the shared elementParticipatesInAutoLayout Eni is making

        const isSiblingOrDescendantOfReparentSubject = EP.isDescendantOf(
          target,
          EP.parentPath(maybeAncestorOrEqual.elementPath),
        )

        if (
          !cmdPressed &&
          targetParticipatesInAutolayout &&
          isSiblingOrDescendantOfReparentSubject
        ) {
          // Filter out Autolayout-participating siblings of the reparented elements, to allow for Single Axis Autolayout Reorder
          return true
        }

        return false
      },
    )
    if (isTargetParentSiblingOrDescendantOfSubjects) {
      return false
    }

    // we found no reason to exclude this element as a target parent, congratulations!
    return true
  })
  return possibleTargetParentsUnderPoint
}

const propertiesToRemove: Array<PropertyPath> = [
  PP.create(['style', 'left']),
  PP.create(['style', 'top']),
  PP.create(['style', 'right']),
  PP.create(['style', 'bottom']),
]

function isTargetAParentOfAnySubject(
  reparentSubjects: ReparentSubjects,
  metadata: ElementInstanceMetadataMap,
  target: ElementPath,
) {
  if (reparentSubjects.type === 'NEW_ELEMENTS') {
    return false
  }
  const selectedElementsMetadata = mapDropNulls(
    (path) => MetadataUtils.findElementByElementPath(metadata, path),
    reparentSubjects.elements,
  )
  return selectedElementsMetadata.some((maybeChild) => EP.isChildOf(maybeChild.elementPath, target))
}

function isTargetOutsideOfContainingComponentUnderMouse(
  selectedElementsMetadata: Array<ElementInstanceMetadata>,
  allElementsUnderPoint: Array<ElementPath>,
  target: ElementPath,
) {
  const containingComponents = selectedElementsMetadata.map((e) =>
    EP.getContainingComponent(e.elementPath),
  )

  const containingComponentsUnderMouse = containingComponents.filter((c) =>
    allElementsUnderPoint.find((p) => EP.pathsEqual(c, p)),
  )

  return containingComponentsUnderMouse.some((c) => EP.isDescendantOf(c, target))
}

function findParentUnderPointByArea(
  targetParentPath: ElementPath,
  metadata: ElementInstanceMetadataMap,
  canvasScale: number,
  pointOnCanvas: CanvasPoint,
) {
  const autolayoutDirection = getDirectionsForSingleAxisAutoLayoutTarget(targetParentPath, metadata)
  const shouldReparentAsFlowOrStatic = autoLayoutParentAbsoluteOrStatic(metadata, targetParentPath)
  const compatibleWith1DReorder = isSingleAxisAutoLayoutComaptibleWithReorder(
    metadata,
    targetParentPath,
  )

  const targetParentUnderPoint: ReparentTarget = (() => {
    if (shouldReparentAsFlowOrStatic === 'REPARENT_AS_ABSOLUTE') {
      // TODO we now assume this is "absolute", but this is too vauge
      return {
        shouldReparent: true,
        newParent: targetParentPath,
        shouldReorder: false,
        newIndex: -1,
        shouldConvertToInline: 'do-not-convert',
        defaultReparentType: 'REPARENT_AS_ABSOLUTE',
      }
    } else if (compatibleWith1DReorder && autolayoutDirection !== 'non-single-axis-autolayout') {
      const { targetUnderMouseIndex, shouldConvertToInline } =
        findIndexForSingleAxisAutolayoutParent(
          autolayoutDirection,
          metadata,
          targetParentPath,
          canvasScale,
          pointOnCanvas,
        )

      return {
        shouldReparent: true,
        newParent: targetParentPath,
        shouldReorder: targetUnderMouseIndex !== -1,
        newIndex: targetUnderMouseIndex,
        shouldConvertToInline: shouldConvertToInline,
        defaultReparentType: 'REPARENT_AS_STATIC',
      }
    } else {
      // element is static parent but don't look for index
      return {
        shouldReparent: true,
        newParent: targetParentPath,
        shouldReorder: false,
        newIndex: -1,
        shouldConvertToInline: 'do-not-convert',
        defaultReparentType: 'REPARENT_AS_STATIC',
      }
    }
  })()
  return targetParentUnderPoint
}

function findIndexForSingleAxisAutolayoutParent(
  autolayoutDirection: SingleAxisAutolayoutContainerDirections,
  metadata: ElementInstanceMetadataMap,
  targetParentPath: ElementPath,
  canvasScale: number,
  pointOnCanvas: CanvasPoint,
): {
  targetUnderMouseIndex: number
  shouldConvertToInline: SimpleFlexDirection | 'do-not-convert'
} {
  const { direction, forwardsOrBackwards, flexOrFlow } = autolayoutDirection

  const targets: Array<{ rect: CanvasRectangle; insertionIndex: number }> =
    drawTargetRectanglesForChildrenOfElement(
      metadata,
      targetParentPath,
      'full-size',
      canvasScale,
      direction,
      forwardsOrBackwards,
    )

  const targetUnderMouseIndex =
    targets.find((target) => {
      return rectContainsPointInclusive(target.rect, pointOnCanvas)
    })?.insertionIndex ?? -1

  const shouldConvertToInline =
    flexOrFlow === 'flex' || direction == null ? 'do-not-convert' : direction

  return { targetUnderMouseIndex, shouldConvertToInline }
}

function findParentByPaddedInsertionZone(
  metadata: ElementInstanceMetadataMap,
  validTargetparentsUnderPoint: ElementPath[],
  canvasScale: number,
  pointOnCanvas: CanvasPoint,
) {
  let targetParentWithPaddedInsertionZone: ReparentTarget | null = null
  const singleAxisAutoLayoutContainersUnderPoint = mapDropNulls((element) => {
    const autolayoutDirection = getDirectionsForSingleAxisAutoLayoutTarget(element, metadata)
    if (autolayoutDirection === 'non-single-axis-autolayout') {
      return null
    }
    const shouldReparentAsFlowOrStatic = autoLayoutParentAbsoluteOrStatic(metadata, element)
    if (shouldReparentAsFlowOrStatic === 'REPARENT_AS_ABSOLUTE') {
      return null
    }
    const compatibleWith1DReorder = isSingleAxisAutoLayoutComaptibleWithReorder(metadata, element)
    if (!compatibleWith1DReorder) {
      return null
    }

    return {
      path: element,
      directions: autolayoutDirection,
    }
  }, [...validTargetparentsUnderPoint].reverse())

  // first try to find a flex element insertion area
  for (const singleAxisAutoLayoutContainer of singleAxisAutoLayoutContainersUnderPoint) {
    const { direction, forwardsOrBackwards, flexOrFlow } = singleAxisAutoLayoutContainer.directions

    const targets: Array<{ rect: CanvasRectangle; insertionIndex: number }> =
      drawTargetRectanglesForChildrenOfElement(
        metadata,
        singleAxisAutoLayoutContainer.path,
        'padded-edge',
        canvasScale,
        direction,
        forwardsOrBackwards,
      )

    const foundTarget = targets.find((target) => {
      return rectContainsPoint(target.rect, pointOnCanvas)
    })
    const targetUnderMouseIndex = foundTarget?.insertionIndex

    if (targetUnderMouseIndex != null) {
      // we found a target!
      targetParentWithPaddedInsertionZone = {
        shouldReparent: true,
        shouldReorder: true,
        newParent: singleAxisAutoLayoutContainer.path,
        newIndex: targetUnderMouseIndex,
        shouldConvertToInline:
          flexOrFlow === 'flex' || direction == null ? 'do-not-convert' : direction,
        defaultReparentType: 'REPARENT_AS_STATIC',
      }
    }
  }
  return targetParentWithPaddedInsertionZone
}

function drawTargetRectanglesForChildrenOfElement(
  metadata: ElementInstanceMetadataMap,
  singleAxisAutolayoutContainerPath: ElementPath,
  targetRectangleSize: 'padded-edge' | 'full-size',
  canvasScale: number,
  simpleFlexDirection: SimpleFlexDirection | null,
  forwardsOrBackwards: FlexForwardsOrBackwards | null,
): Array<{ rect: CanvasRectangle; insertionIndex: number }> {
  const ExtraPadding = 10 / canvasScale

  const parentBounds = MetadataUtils.getFrameInCanvasCoords(
    singleAxisAutolayoutContainerPath,
    metadata,
  )

  if (parentBounds == null || simpleFlexDirection == null || forwardsOrBackwards == null) {
    // TODO should we throw an error?
    return []
  }

  const leftOrTop = simpleFlexDirection === 'row' ? 'x' : 'y'
  const leftOrTopComplement = simpleFlexDirection === 'row' ? 'y' : 'x'
  const widthOrHeight = simpleFlexDirection === 'row' ? 'width' : 'height'
  const widthOrHeightComplement = simpleFlexDirection === 'row' ? 'height' : 'width'

  const children = MetadataUtils.getChildrenPaths(metadata, singleAxisAutolayoutContainerPath)

  interface ElemBounds {
    start: number
    size: number
    end: number
    index: number
  }

  const pseudoElementLeftOrTop: ElemBounds = {
    start: parentBounds[leftOrTop],
    size: 0,
    end: parentBounds[leftOrTop],
    index: forwardsOrBackwards === 'forward' ? -1 : children.length,
  }
  const pseudoElementRightOrBottom: ElemBounds = {
    start: parentBounds[leftOrTop] + parentBounds[widthOrHeight],
    size: 0,
    end: parentBounds[leftOrTop] + parentBounds[widthOrHeight],
    index: forwardsOrBackwards === 'forward' ? children.length : -1,
  }

  const childrenBounds: Array<ElemBounds> = mapDropNulls((childPath, index) => {
    if (
      // TODO make a MetadataUtils.elementParticipatesInLayout helper function and use it in Flow Reorder, Flex Reorder too
      MetadataUtils.isPositionAbsolute(MetadataUtils.findElementByElementPath(metadata, childPath))
    ) {
      return null
    }

    const bounds = MetadataUtils.getFrameInCanvasCoords(childPath, metadata)!
    return {
      start: bounds[leftOrTop],
      size: bounds[widthOrHeight],
      end: bounds[leftOrTop] + bounds[widthOrHeight],
      index: index,
    }
  }, children)

  const childrenBoundsAlongAxis: Array<ElemBounds> = [
    pseudoElementLeftOrTop,
    ...(forwardsOrBackwards === 'forward' ? childrenBounds : reverse(childrenBounds)),
    pseudoElementRightOrBottom,
  ]

  let flexInsertionTargets: Array<{ rect: CanvasRectangle; insertionIndex: number }> = []

  if (targetRectangleSize === 'padded-edge') {
    for (let index = 0; index < childrenBoundsAlongAxis.length; index++) {
      const bounds = childrenBoundsAlongAxis[index]

      const normalizedStart = Math.min(bounds.start, bounds.end)
      const normalizedEnd = Math.max(bounds.start, bounds.end)

      flexInsertionTargets.push(
        {
          insertionIndex: forwardsOrBackwards === 'forward' ? bounds.index : bounds.index + 1,
          rect: rectFromTwoPoints(
            {
              [leftOrTop]: normalizedStart - ExtraPadding,
              [leftOrTopComplement]: parentBounds[leftOrTopComplement],
            } as any as CanvasPoint, // TODO improve my type
            {
              [leftOrTop]: Math.min(
                normalizedStart + ExtraPadding,
                normalizedStart + bounds.size / 2,
              ),
              [leftOrTopComplement]:
                parentBounds[leftOrTopComplement] + parentBounds[widthOrHeightComplement],
            } as any as CanvasPoint, // TODO improve my type
          ),
        },
        {
          insertionIndex: forwardsOrBackwards === 'forward' ? bounds.index + 1 : bounds.index,
          rect: rectFromTwoPoints(
            {
              [leftOrTop]: Math.max(normalizedEnd - ExtraPadding, normalizedEnd - bounds.size / 2),
              [leftOrTopComplement]: parentBounds[leftOrTopComplement],
            } as any as CanvasPoint, // TODO improve my type
            {
              [leftOrTop]: normalizedEnd + ExtraPadding,
              [leftOrTopComplement]:
                parentBounds[leftOrTopComplement] + parentBounds[widthOrHeightComplement],
            } as any as CanvasPoint, // TODO improve my type
          ),
        },
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

      flexInsertionTargets.push({
        insertionIndex:
          forwardsOrBackwards === 'forward'
            ? childrenBoundsAlongAxis[index].index + 1
            : childrenBoundsAlongAxis[index].index,
        rect: rectFromTwoPoints(
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
      })
    }
  }

  return flexInsertionTargets
}

export function getSiblingMidPointPosition(
  precedingSiblingPosition: CanvasRectangle,
  succeedingSiblingPosition: CanvasRectangle,
  direction: SimpleFlexDirection,
  forwardsOrBackwards: FlexForwardsOrBackwards,
): number {
  let getStartPosition: (rect: CanvasRectangle) => number
  let getEndPosition: (rect: CanvasRectangle) => number
  switch (direction) {
    case 'row':
      getStartPosition = (rect: CanvasRectangle) => {
        return rect.x
      }
      getEndPosition = (rect: CanvasRectangle) => {
        return rect.x + rect.width
      }
      break
    case 'column':
      getStartPosition = (rect: CanvasRectangle) => {
        return rect.y
      }
      getEndPosition = (rect: CanvasRectangle) => {
        return rect.y + rect.height
      }
      break
    default:
      const _exhaustiveCheck: never = direction
      throw new Error(`Unhandled direction of ${JSON.stringify(direction)}`)
  }

  const value =
    forwardsOrBackwards === 'forward'
      ? (getEndPosition(precedingSiblingPosition) + getStartPosition(succeedingSiblingPosition)) / 2
      : (getEndPosition(succeedingSiblingPosition) + getStartPosition(precedingSiblingPosition)) / 2

  return value
}

export function siblingAndPseudoPositions(
  parentFlexDirection: SimpleFlexDirection,
  forwardsOrBackwards: FlexForwardsOrBackwards,
  parentRect: CanvasRectangle,
  siblingsOfTarget: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
): Array<CanvasRectangle> {
  const siblingsPossiblyReversed =
    forwardsOrBackwards === 'forward' ? siblingsOfTarget : reverse(siblingsOfTarget)

  const pseudoElements = createPseudoElements(
    siblingsPossiblyReversed,
    parentFlexDirection,
    parentRect,
    metadata,
  )

  const siblingPositions: Array<CanvasRectangle> = [
    pseudoElements.before,
    ...siblingsPossiblyReversed.map((sibling) => {
      return MetadataUtils.getFrameInCanvasCoords(sibling, metadata) ?? zeroCanvasRect
    }),
    pseudoElements.after,
  ]
  return forwardsOrBackwards === 'forward' ? siblingPositions : reverse(siblingPositions)
}

function createPseudoElements(
  siblings: Array<ElementPath>,
  parentFlexDirection: SimpleFlexDirection,
  parentFrame: CanvasRectangle,
  metadata: ElementInstanceMetadataMap,
): { before: CanvasRectangle; after: CanvasRectangle } {
  const firstElementPath = siblings[0]
  const lastElementPath = siblings[siblings.length - 1]

  const flexGap = MetadataUtils.getParentFlexGap(firstElementPath, metadata)

  const firstElementFrame =
    MetadataUtils.getFrameInCanvasCoords(firstElementPath, metadata) ?? zeroCanvasRect
  const firstElementMargin = MetadataUtils.getElementMargin(firstElementPath, metadata)

  const lastElementFrame =
    MetadataUtils.getFrameInCanvasCoords(lastElementPath, metadata) ?? zeroCanvasRect
  const lastElementMargin = MetadataUtils.getElementMargin(lastElementPath, metadata)

  if (parentFlexDirection === 'row') {
    const marginLeftAndGapOffset = ((firstElementMargin?.left ?? 0) + flexGap) * 2
    const marginRightAndGapOffset = ((lastElementMargin?.right ?? 0) + flexGap) * 2
    return {
      before: canvasRectangle({
        x: Math.max(firstElementFrame.x - marginLeftAndGapOffset, parentFrame.x),
        y: firstElementFrame.y,
        width: 0,
        height: firstElementFrame.height,
      }),
      after: canvasRectangle({
        x: Math.min(
          lastElementFrame.x + lastElementFrame.width + marginRightAndGapOffset,
          parentFrame.x + parentFrame.width,
        ),
        y: lastElementFrame.y,
        width: 0,
        height: lastElementFrame.height,
      }),
    }
  } else {
    const marginTopAndGapOffset = ((firstElementMargin?.top ?? 0) + flexGap) * 2
    const marginBottomAndGapOffset = ((lastElementMargin?.bottom ?? 0) + flexGap) * 2

    return {
      before: canvasRectangle({
        x: firstElementFrame.x,
        y: Math.max(firstElementFrame.y - marginTopAndGapOffset, parentFrame.y),
        height: 0,
        width: firstElementFrame.width,
      }),
      after: canvasRectangle({
        x: lastElementFrame.x,
        y: Math.min(
          lastElementFrame.y + lastElementFrame.height + marginBottomAndGapOffset,
          parentFrame.y + parentFrame.height,
        ),
        height: 0,
        width: lastElementFrame.width,
      }),
    }
  }
}

export function applyStaticReparent(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  reparentResult: ReparentTarget,
): StrategyApplicationResult {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  const filteredSelectedElements = getDragTargets(selectedElements)

  return ifAllowedToReparent(
    canvasState,
    canvasState.startingMetadata,
    filteredSelectedElements,
    () => {
      if (
        interactionSession.interactionData.type == 'DRAG' &&
        interactionSession.interactionData.drag != null
      ) {
        if (reparentResult.shouldReparent && filteredSelectedElements.length === 1) {
          const target = filteredSelectedElements[0]

          const newIndex = reparentResult.newIndex
          const newParent = reparentResult.newParent
          const parentRect =
            MetadataUtils.getFrameInCanvasCoords(newParent, canvasState.startingMetadata) ??
            zeroCanvasRect

          const siblingsOfTarget = MetadataUtils.getChildrenPaths(
            canvasState.startingMetadata,
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

            const targetMetadata = MetadataUtils.findElementByElementPath(
              canvasState.startingMetadata,
              target,
            )

            // Strip the `position`, positional and dimension properties.
            const propertyChangeCommands = getStaticReparentPropertyChanges(
              newPath,
              targetMetadata?.specialSizeMeasurements.position ?? null,
              targetMetadata?.specialSizeMeasurements.display ?? null,
              reparentResult.shouldConvertToInline,
            )

            const commandsBeforeReorder = [
              ...reparentCommands,
              updateSelectedViews('always', [newPath]),
            ]

            const commandsAfterReorder = [
              ...propertyChangeCommands,
              setElementsToRerenderCommand([target, newPath]),
              updateHighlightedViews('mid-interaction', []),
              setCursorCommand(CSSCursor.Move),
            ]

            function midInteractionCommandsForTarget(shouldReorder: boolean): Array<CanvasCommand> {
              const commonPatches = [
                wildcardPatch('mid-interaction', {
                  canvas: { controls: { parentHighlightPaths: { $set: [newParent] } } },
                }),
                newParentADescendantOfCurrentParent
                  ? wildcardPatch('mid-interaction', {
                      hiddenInstances: { $push: [target] },
                    })
                  : wildcardPatch('mid-interaction', {
                      displayNoneInstances: { $push: [target] },
                    }),
              ]
              if (shouldReorder) {
                return [
                  ...commonPatches,
                  showReorderIndicator(newParent, newIndex),
                  wildcardPatch('mid-interaction', {
                    displayNoneInstances: { $push: [newPath] },
                  }),
                ]
              } else {
                return commonPatches
              }
            }

            let interactionFinishCommands: Array<CanvasCommand>
            let midInteractionCommands: Array<CanvasCommand>

            if (reparentResult.shouldReorder && siblingsOfTarget.length > 0) {
              midInteractionCommands = midInteractionCommandsForTarget(reparentResult.shouldReorder)

              interactionFinishCommands = [
                ...commandsBeforeReorder,
                reorderElement('always', newPath, absolute(newIndex)),
                ...commandsAfterReorder,
              ]
            } else {
              if (parentRect != null) {
                midInteractionCommands = midInteractionCommandsForTarget(
                  reparentResult.shouldReorder,
                )
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
    },
  )
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

export function getStaticReparentPropertyChanges(
  newPath: ElementPath,
  targetOriginalStylePosition: CSSPosition | null,
  targetOriginalDisplayProp: string | null,
  convertToInline: 'row' | 'column' | 'do-not-convert',
): Array<CanvasCommand> {
  const optionalInlineConversionCommand = getOptionalCommandToConvertDisplayInlineBlock(
    newPath,
    targetOriginalDisplayProp,
    convertToInline,
  )

  if (targetOriginalStylePosition !== 'absolute' && targetOriginalStylePosition !== 'relative') {
    return [
      ...optionalInlineConversionCommand,
      deleteProperties('always', newPath, propertiesToRemove),
    ]
  }

  return [
    ...optionalInlineConversionCommand,
    deleteProperties('always', newPath, [...propertiesToRemove, PP.create(['style', 'position'])]),
    setProperty('always', newPath, PP.create(['style', 'contain']), 'layout'),
  ]
}

export function getReparentPropertyChanges(
  reparentStrategy: ReparentStrategy,
  target: ElementPath,
  newParent: ElementPath,
  targetStartingMetadata: ElementInstanceMetadataMap,
  newParentStartingMetadata: ElementInstanceMetadataMap,
  projectContents: ProjectContentTreeRoot,
  openFile: string | null | undefined,
  targetOriginalStylePosition: CSSPosition | null,
  targetOriginalDisplayProp: string | null,
): Array<CanvasCommand> {
  switch (reparentStrategy) {
    case 'REPARENT_AS_ABSOLUTE':
      return getAbsoluteReparentPropertyChanges(
        target,
        newParent,
        targetStartingMetadata,
        newParentStartingMetadata,
        projectContents,
        openFile,
      )
    case 'REPARENT_AS_STATIC':
      const newPath = EP.appendToPath(newParent, EP.toUid(target))
      const directions = getDirectionsForSingleAxisAutoLayoutTarget(
        newParent,
        newParentStartingMetadata,
      )

      const convertDisplayInline =
        directions === 'non-single-axis-autolayout' ||
        directions.direction == null ||
        directions.flexOrFlow === 'flex'
          ? 'do-not-convert'
          : directions.direction

      return getStaticReparentPropertyChanges(
        newPath,
        targetOriginalStylePosition,
        targetOriginalDisplayProp,
        convertDisplayInline,
      )
  }
}

function getDirectionsForSingleAxisAutoLayoutTarget(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
): SingleAxisAutolayoutContainerDirections | 'non-single-axis-autolayout' {
  const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
  if (elementMetadata == null) {
    return 'non-single-axis-autolayout'
  }

  return singleAxisAutoLayoutContainerDirections(path, metadata)
}
