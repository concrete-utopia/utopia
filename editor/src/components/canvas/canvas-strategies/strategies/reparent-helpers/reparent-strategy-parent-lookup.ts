import type { ElementSupportsChildren } from '../../../../../core/model/element-template-utils'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import { getStoryboardElementPath } from '../../../../../core/model/scene-utils'
import { mapDropNulls } from '../../../../../core/shared/array-utils'
import * as EP from '../../../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../../../core/shared/element-template'
import type { CanvasPoint, CanvasRectangle, Size } from '../../../../../core/shared/math-utils'
import {
  isInfinityRectangle,
  rectContainsPoint,
  rectContainsPointInclusive,
  size,
  sizeFitsInTarget,
  zeroRectIfNullOrInfinity,
} from '../../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../../core/shared/project-file-types'
import type { AllElementProps } from '../../../../editor/store/editor-state'
import type { Direction } from '../../../../inspector/common/css-utils'
import { getAllTargetsAtPointAABB } from '../../../dom-lookup'
import type { InteractionCanvasState } from '../../canvas-strategy-types'
import type { AllowSmallerParent } from '../../interaction-state'
import type { SingleAxisAutolayoutContainerDirections } from '../flow-reorder-helpers'
import { singleAxisAutoLayoutContainerDirections } from '../flow-reorder-helpers'
import { treatElementAsFragmentLike } from '../fragment-like-helpers'
import type {
  ReparentStrategy,
  ReparentSubjects,
  ReparentTarget,
} from './reparent-strategy-helpers'
import { drawTargetRectanglesForChildrenOfElement } from './reparent-strategy-sibling-position-helpers'
import type { ElementPathTrees } from '../../../../../core/shared/element-path-tree'
import { isConditionalWithEmptyOrTextEditableActiveBranch } from '../../../../../core/model/conditionals'
import { getInsertionPathForReparentTarget } from './reparent-helpers'
import { treatElementAsGroupLike } from '../group-helpers'
import type { PropertyControlsInfo } from '../../../../custom-code/code-file'

export type FindReparentStrategyResult = {
  strategy: ReparentStrategy
  isFallback: boolean
  target: ReparentTarget
}

export function getReparentTargetUnified(
  reparentSubjects: ReparentSubjects,
  pointOnCanvas: CanvasPoint,
  cmdPressed: boolean, // TODO: this should be removed from here and replaced by meaningful flag(s) (similar to allowSmallerParent)
  canvasState: InteractionCanvasState,
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  allowSmallerParent: AllowSmallerParent,
  elementSupportsChildren: Array<ElementSupportsChildren> = ['supportsChildren'],
  propertyControlsInfo: PropertyControlsInfo,
): ReparentTarget | null {
  const canvasScale = canvasState.scale

  const validTargetParentsUnderPoint = findValidTargetsUnderPoint(
    reparentSubjects,
    pointOnCanvas,
    cmdPressed,
    canvasState,
    metadata,
    elementPathTree,
    allElementProps,
    allowSmallerParent,
    elementSupportsChildren,
    propertyControlsInfo,
  )

  // For Flex parents, we want to be able to insert between two children that don't have a gap between them.
  const targetParentWithPaddedInsertionZone: ReparentTarget | null =
    findParentByPaddedInsertionZone(
      metadata,
      elementPathTree,
      allElementProps,
      validTargetParentsUnderPoint,
      reparentSubjects,
      canvasScale,
      pointOnCanvas,
    )

  if (targetParentWithPaddedInsertionZone != null) {
    return targetParentWithPaddedInsertionZone
  }

  // fall back to trying to find an absolute element, or the "background" area of an autolayout container
  const targetParentPath = validTargetParentsUnderPoint[0]
  if (targetParentPath == null) {
    // none of the targets were under the mouse, fallback return
    return null
  }

  const targetParentUnderPoint: ReparentTarget = findParentUnderPointByArea(
    targetParentPath,
    metadata,
    elementPathTree,
    allElementProps,
    canvasScale,
    pointOnCanvas,
  )
  return targetParentUnderPoint
}

function recursivelyFindConditionalWithEmptyOrTextEditableBranch(
  target: ElementPath,
  metadata: ElementInstanceMetadataMap,
  forTextEditing: boolean,
  elementPathTree: ElementPathTrees,
): ElementPath | null {
  const conditional = isConditionalWithEmptyOrTextEditableActiveBranch(
    target,
    metadata,
    elementPathTree,
  )
  if (conditional == null) {
    return null
  }

  const { element, isEmpty, textEditable, clause } = conditional

  if (isEmpty) {
    return target
  }
  if (forTextEditing && textEditable) {
    return target
  }

  if (clause == null) {
    return null
  }

  const branch = EP.appendToPath(
    target,
    clause === 'true-case' ? element.whenTrue.uid : element.whenFalse.uid,
  )
  return recursivelyFindConditionalWithEmptyOrTextEditableBranch(
    branch,
    metadata,
    forTextEditing,
    elementPathTree,
  )
}

function findValidTargetsUnderPoint(
  reparentSubjects: ReparentSubjects,
  pointOnCanvas: CanvasPoint,
  cmdPressed: boolean, // TODO: this should be removed from here and replaced by meaningful flag(s) (similar to allowSmallerParent)
  canvasState: InteractionCanvasState,
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  allowSmallerParent: AllowSmallerParent,
  elementSupportsChildren: Array<ElementSupportsChildren> = ['supportsChildren'],
  propertyControlsInfo: PropertyControlsInfo,
): Array<ElementPath> {
  const projectContents = canvasState.projectContents
  const openFile = canvasState.openFile ?? null

  const storyboardComponent = getStoryboardElementPath(projectContents, openFile)
  if (storyboardComponent == null) {
    return []
  }

  const multiselectBounds: Size =
    reparentSubjects.type === 'EXISTING_ELEMENTS'
      ? zeroRectIfNullOrInfinity(
          MetadataUtils.getBoundingRectangleInCanvasCoords(reparentSubjects.elements, metadata),
        )
      : reparentSubjects.defaultSize

  const allElementsUnderPoint = [
    ...getAllTargetsAtPointAABB(
      metadata,
      [],
      [],
      'no-filter',
      pointOnCanvas,
      elementPathTree,
      allElementProps,
      false,
    ),
    storyboardComponent,
  ]

  const forTextEditing =
    canvasState.interactionTarget.type === 'INSERTION_SUBJECTS' &&
    canvasState.interactionTarget.subjects.some((s) => s.textEdit)

  const possibleTargetParentsUnderPoint = mapDropNulls((target) => {
    const children = MetadataUtils.getChildrenOrdered(metadata, elementPathTree, target)
    for (const child of children) {
      const emptyConditional = recursivelyFindConditionalWithEmptyOrTextEditableBranch(
        child.elementPath,
        metadata,
        forTextEditing,
        elementPathTree,
      )
      if (emptyConditional != null) {
        return emptyConditional
      }
    }
    if (treatElementAsFragmentLike(metadata, allElementProps, elementPathTree, target)) {
      // we disallow reparenting into sizeless Fragment-like elements
      return null
    }

    if (treatElementAsGroupLike(metadata, target)) {
      // we disallow reparenting into Group-like elements
      return null
    }

    const currentParent = isTargetAParentOfAnySubject(reparentSubjects, metadata, target)

    if (currentParent) {
      // the current parent should be included in the array of valid targets
      return target
    }

    if (
      !elementSupportsChildren.includes(
        MetadataUtils.targetSupportsChildrenAlsoText(
          projectContents,
          metadata,
          target,
          elementPathTree,
          propertyControlsInfo,
        ),
      )
    ) {
      // simply skip elements that do not support children
      return null
    }

    const targetFrame = MetadataUtils.getFrameInCanvasCoords(target, metadata)
    const targetFrameSize =
      targetFrame == null
        ? size(0, 0)
        : isInfinityRectangle(targetFrame)
        ? size(Infinity, Infinity)
        : targetFrame
    const sizeFitsTarget =
      allowSmallerParent === 'allow-smaller-parent' ||
      sizeFitsInTarget(multiselectBounds, targetFrameSize)

    if (!sizeFitsTarget) {
      // skip elements that are smaller than the dragged elements, unless 'allow-smaller-parent'
      return null
    }

    if (reparentSubjects.type === 'NEW_ELEMENTS') {
      return target
    }

    const selectedElementsMetadata = mapDropNulls(
      (path) => MetadataUtils.findElementByElementPath(metadata, path),
      reparentSubjects.elements,
    )
    if (
      isTargetParentOutsideOfContainingComponentUnderMouse(
        selectedElementsMetadata,
        allElementsUnderPoint,
        target,
      )
    ) {
      return null
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
      return null
    }

    // we found no reason to exclude this element as a target parent, congratulations!
    return target
  }, allElementsUnderPoint)

  return possibleTargetParentsUnderPoint
}

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

function isTargetParentOutsideOfContainingComponentUnderMouse(
  selectedElementsMetadata: Array<ElementInstanceMetadata>,
  allElementsUnderPoint: Array<ElementPath>,
  possibleTargetParent: ElementPath,
): boolean {
  const containingComponents = selectedElementsMetadata.map((e) =>
    EP.getContainingComponent(e.elementPath),
  )

  const containingComponentsUnderMouse = containingComponents.filter((c) =>
    allElementsUnderPoint.find((p) => EP.pathsEqual(c, p)),
  )

  return containingComponentsUnderMouse.some((c) => EP.isDescendantOf(c, possibleTargetParent))
}

function findParentByPaddedInsertionZone(
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  validTargetparentsUnderPoint: ElementPath[],
  reparentSubjects: ReparentSubjects,
  canvasScale: number,
  pointOnCanvas: CanvasPoint,
): ReparentTarget | null {
  // with current parent under cursor filter ancestors from reparent targets
  const currentParentUnderCursor =
    reparentSubjects.type === 'EXISTING_ELEMENTS'
      ? validTargetparentsUnderPoint.find((targetParent) => {
          const firstReparentSubjectElement = reparentSubjects.elements.at(0)
          if (firstReparentSubjectElement == null) {
            return false
          } else {
            return EP.isParentOf(targetParent, firstReparentSubjectElement)
          }
        }) ?? null
      : null
  const validTargetparentsUnderPointFiltered =
    currentParentUnderCursor != null
      ? validTargetparentsUnderPoint.filter(
          (targetParent) => !EP.isDescendantOf(currentParentUnderCursor, targetParent),
        )
      : validTargetparentsUnderPoint

  const singleAxisAutoLayoutContainersUnderPoint = mapDropNulls((element) => {
    const autolayoutDirection = singleAxisAutoLayoutContainerDirections(
      element,
      metadata,
      elementPathTree,
    )
    if (autolayoutDirection === 'non-single-axis-autolayout') {
      return null
    }
    const shouldReparentAsAbsoluteOrStatic = autoLayoutParentAbsoluteOrStatic(
      metadata,
      allElementProps,
      elementPathTree,
      element,
    )
    if (shouldReparentAsAbsoluteOrStatic === 'REPARENT_AS_ABSOLUTE') {
      return null
    }
    const compatibleWith1DReorder = isSingleAxisAutoLayoutCompatibleWithReorder(
      metadata,
      elementPathTree,
      element,
    )
    if (!compatibleWith1DReorder) {
      return null
    }

    return {
      path: element,
      directions: autolayoutDirection,
    }
  }, [...validTargetparentsUnderPointFiltered].reverse())

  // first try to find a flex element insertion area
  for (const singleAxisAutoLayoutContainer of singleAxisAutoLayoutContainersUnderPoint) {
    const {
      direction,
      forwardOrReverse: forwardsOrBackwards,
      flexOrFlow,
    } = singleAxisAutoLayoutContainer.directions

    const targets: Array<{ rect: CanvasRectangle; insertionIndex: number }> =
      drawTargetRectanglesForChildrenOfElement(
        metadata,
        elementPathTree,
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
      // we found a first good target parent, early return
      return {
        shouldReparent: true,
        shouldShowPositionIndicator: true,
        newParent: getInsertionPathForReparentTarget(singleAxisAutoLayoutContainer.path, metadata),
        newIndex: targetUnderMouseIndex,
        shouldConvertToInline:
          flexOrFlow === 'flex' || direction == null ? 'do-not-convert' : direction,
        defaultReparentType: 'REPARENT_AS_STATIC',
      }
    }
  }
  return null
}

function findParentUnderPointByArea(
  targetParentPath: ElementPath,
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  canvasScale: number,
  pointOnCanvas: CanvasPoint,
): ReparentTarget {
  const autolayoutDirection = singleAxisAutoLayoutContainerDirections(
    targetParentPath,
    metadata,
    elementPathTree,
  )
  const shouldReparentAsAbsoluteOrStatic = autoLayoutParentAbsoluteOrStatic(
    metadata,
    allElementProps,
    elementPathTree,
    targetParentPath,
  )
  const compatibleWith1DReorder = isSingleAxisAutoLayoutCompatibleWithReorder(
    metadata,
    elementPathTree,
    targetParentPath,
  )

  const targetParentUnderPoint: ReparentTarget = (() => {
    const insertionPath = getInsertionPathForReparentTarget(targetParentPath, metadata)
    if (shouldReparentAsAbsoluteOrStatic === 'REPARENT_INTO_GRID') {
      return {
        shouldReparent: true,
        newParent: insertionPath,
        shouldShowPositionIndicator: false,
        newIndex: -1,
        shouldConvertToInline: 'do-not-convert',
        defaultReparentType: 'REPARENT_INTO_GRID',
      }
    } else if (shouldReparentAsAbsoluteOrStatic === 'REPARENT_AS_ABSOLUTE') {
      // TODO we now assume this is "absolute", but this is too vauge
      return {
        shouldReparent: true,
        newParent: insertionPath,
        shouldShowPositionIndicator: false,
        newIndex: -1,
        shouldConvertToInline: 'do-not-convert',
        defaultReparentType: 'REPARENT_AS_ABSOLUTE',
      }
    } else if (compatibleWith1DReorder && autolayoutDirection !== 'non-single-axis-autolayout') {
      const { targetUnderMouseIndex, shouldConvertToInline } =
        findIndexForSingleAxisAutolayoutParent(
          autolayoutDirection,
          metadata,
          elementPathTree,
          targetParentPath,
          canvasScale,
          pointOnCanvas,
        )

      const hasStaticChildren = MetadataUtils.hasStaticChildren(
        metadata,
        elementPathTree,
        targetParentPath,
      )

      return {
        shouldReparent: true,
        newParent: insertionPath,
        shouldShowPositionIndicator: targetUnderMouseIndex !== -1 && hasStaticChildren,
        newIndex: targetUnderMouseIndex,
        shouldConvertToInline: shouldConvertToInline,
        defaultReparentType: 'REPARENT_AS_STATIC',
      }
    } else {
      // element is static parent but don't look for index
      return {
        shouldReparent: true,
        newParent: insertionPath,
        shouldShowPositionIndicator: false,
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
  pathTrees: ElementPathTrees,
  targetParentPath: ElementPath,
  canvasScale: number,
  pointOnCanvas: CanvasPoint,
): {
  targetUnderMouseIndex: number
  shouldConvertToInline: Direction | 'do-not-convert'
} {
  const { direction, forwardOrReverse: forwardsOrBackwards, flexOrFlow } = autolayoutDirection

  const targets: Array<{ rect: CanvasRectangle; insertionIndex: number }> =
    drawTargetRectanglesForChildrenOfElement(
      metadata,
      pathTrees,
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

export function autoLayoutParentAbsoluteOrStatic(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  parent: ElementPath,
  preferAbsolute: 'prefer-absolute' | null = null,
): ReparentStrategy {
  const parentMetadata = MetadataUtils.findElementByElementPath(metadata, parent)
  const children = MetadataUtils.getChildrenOrdered(metadata, pathTrees, parent)

  const storyboardRoot = EP.isStoryboardPath(parent)
  if (storyboardRoot) {
    // always reparent as absolute to the Storyboard
    return 'REPARENT_AS_ABSOLUTE'
  }

  const parentLayout = MetadataUtils.findLayoutSystemForChildren(metadata, pathTrees, parent)

  if (parentLayout === 'flex') {
    return 'REPARENT_AS_STATIC'
  } else if (parentLayout === 'grid') {
    return 'REPARENT_INTO_GRID'
  }

  const isTextFromMetadata = MetadataUtils.isTextFromMetadata(
    MetadataUtils.findElementByElementPath(metadata, parent),
  )
  if (isTextFromMetadata) {
    return 'REPARENT_AS_STATIC'
  }

  const isFragmentLike = treatElementAsFragmentLike(metadata, allElementProps, pathTrees, parent)
  if (isFragmentLike) {
    return 'REPARENT_AS_ABSOLUTE'
  }

  if (preferAbsolute === 'prefer-absolute') {
    return 'REPARENT_AS_ABSOLUTE'
  }

  const parentIsContainingBlock =
    parentMetadata?.specialSizeMeasurements.providesBoundsForAbsoluteChildren ?? false
  if (!parentIsContainingBlock) {
    return 'REPARENT_AS_STATIC'
  }

  const allChildrenFlow =
    children.length > 0 &&
    children.every((child) => child.specialSizeMeasurements.position === 'static')

  if (allChildrenFlow) {
    return 'REPARENT_AS_STATIC'
  }

  const parentFrame = parentMetadata?.globalFrame ?? null
  const parentWidth =
    parentFrame == null ? 0 : isInfinityRectangle(parentFrame) ? Infinity : parentFrame.width
  const parentHeight =
    parentFrame == null ? 0 : isInfinityRectangle(parentFrame) ? Infinity : parentFrame.height

  const emptyParentWithDimensionsGreaterThanZero =
    children.length === 0 && parentWidth > 0 && parentHeight > 0
  if (emptyParentWithDimensionsGreaterThanZero) {
    return 'REPARENT_AS_ABSOLUTE'
  }

  // TODO ABSOLUTE drag onto the padded area of flow layout target parent

  // TODO is this needed?
  const emptyParentWithAnyDimensionZero =
    children.length === 0 && (parentWidth === 0 || parentHeight === 0)
  if (emptyParentWithAnyDimensionZero) {
    return 'REPARENT_AS_STATIC'
  }

  // the fallback is reparent as absolute
  return 'REPARENT_AS_ABSOLUTE'

  // should there be a DO_NOT_REPARENT return type here?
}

function isSingleAxisAutoLayoutCompatibleWithReorder(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  parent: ElementPath,
): boolean {
  const newParentMetadata = MetadataUtils.findElementByElementPath(metadata, parent)
  const parentIsFlexLayout = MetadataUtils.isFlexLayoutedContainer(newParentMetadata)
  if (parentIsFlexLayout) {
    return true
  }
  const flowChildren = MetadataUtils.getChildrenOrdered(metadata, pathTrees, parent).filter(
    MetadataUtils.elementParticipatesInAutoLayout,
  )
  return flowChildren.length > 1
}
