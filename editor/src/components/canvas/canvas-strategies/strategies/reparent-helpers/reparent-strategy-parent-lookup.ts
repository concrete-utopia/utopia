import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import { getStoryboardElementPath } from '../../../../../core/model/scene-utils'
import { mapDropNulls } from '../../../../../core/shared/array-utils'
import * as EP from '../../../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../../../core/shared/element-template'
import {
  CanvasPoint,
  CanvasRectangle,
  rectContainsPoint,
  rectContainsPointInclusive,
  Size,
  size,
  sizeFitsInTarget,
} from '../../../../../core/shared/math-utils'
import { ElementPath } from '../../../../../core/shared/project-file-types'
import { AllElementProps } from '../../../../editor/store/editor-state'
import { Direction } from '../../../../inspector/common/css-utils'
import { getAllTargetsAtPointAABB } from '../../../dom-lookup'
import { InteractionCanvasState } from '../../canvas-strategy-types'
import { AllowSmallerParent } from '../../interaction-state'
import {
  SingleAxisAutolayoutContainerDirections,
  singleAxisAutoLayoutContainerDirections,
} from '../flow-reorder-helpers'
import { ReparentStrategy, ReparentSubjects, ReparentTarget } from './reparent-strategy-helpers'
import { drawTargetRectanglesForChildrenOfElement } from './reparent-strategy-sibling-position-helpers'

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
  allElementProps: AllElementProps,
  allowSmallerParent: AllowSmallerParent,
  allowWithOnlyTextChildren?: boolean,
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
    allowWithOnlyTextChildren,
  )

  // For Flex parents, we want to be able to insert between two children that don't have a gap between them.
  const targetParentWithPaddedInsertionZone: ReparentTarget | null =
    findParentByPaddedInsertionZone(
      metadata,
      validTargetparentsUnderPoint,
      reparentSubjects,
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
  allowWithOnlyTextChildren?: boolean,
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

    if (
      !MetadataUtils.targetSupportsChildren(
        projectContents,
        metadata,
        target,
        allowWithOnlyTextChildren,
      )
    ) {
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

function findParentByPaddedInsertionZone(
  metadata: ElementInstanceMetadataMap,
  validTargetparentsUnderPoint: ElementPath[],
  reparentSubjects: ReparentSubjects,
  canvasScale: number,
  pointOnCanvas: CanvasPoint,
): ReparentTarget | null {
  // with current parent under cursor filter ancestors from reparent targets
  const currentParentUnderCursor =
    reparentSubjects.type === 'EXISTING_ELEMENTS'
      ? validTargetparentsUnderPoint.find((targetParent) =>
          EP.isParentOf(targetParent, reparentSubjects.elements[0]),
        ) ?? null
      : null
  const validTargetparentsUnderPointFiltered =
    currentParentUnderCursor != null
      ? validTargetparentsUnderPoint.filter(
          (targetParent) => !EP.isDescendantOf(currentParentUnderCursor, targetParent),
        )
      : validTargetparentsUnderPoint

  const singleAxisAutoLayoutContainersUnderPoint = mapDropNulls((element) => {
    const autolayoutDirection = singleAxisAutoLayoutContainerDirections(element, metadata)
    if (autolayoutDirection === 'non-single-axis-autolayout') {
      return null
    }
    const shouldReparentAsAbsoluteOrStatic = autoLayoutParentAbsoluteOrStatic(metadata, element)
    if (shouldReparentAsAbsoluteOrStatic === 'REPARENT_AS_ABSOLUTE') {
      return null
    }
    const compatibleWith1DReorder = isSingleAxisAutoLayoutCompatibleWithReorder(metadata, element)
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
        newParent: singleAxisAutoLayoutContainer.path,
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
  canvasScale: number,
  pointOnCanvas: CanvasPoint,
) {
  const autolayoutDirection = singleAxisAutoLayoutContainerDirections(targetParentPath, metadata)
  const shouldReparentAsAbsoluteOrStatic = autoLayoutParentAbsoluteOrStatic(
    metadata,
    targetParentPath,
  )
  const compatibleWith1DReorder = isSingleAxisAutoLayoutCompatibleWithReorder(
    metadata,
    targetParentPath,
  )

  const targetParentUnderPoint: ReparentTarget = (() => {
    if (shouldReparentAsAbsoluteOrStatic === 'REPARENT_AS_ABSOLUTE') {
      // TODO we now assume this is "absolute", but this is too vauge
      return {
        shouldReparent: true,
        newParent: targetParentPath,
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
          targetParentPath,
          canvasScale,
          pointOnCanvas,
        )

      const hasStaticChildren =
        MetadataUtils.getChildrenParticipatingInAutoLayout(metadata, targetParentPath).length > 0

      return {
        shouldReparent: true,
        newParent: targetParentPath,
        shouldShowPositionIndicator: targetUnderMouseIndex !== -1 && hasStaticChildren,
        newIndex: targetUnderMouseIndex,
        shouldConvertToInline: shouldConvertToInline,
        defaultReparentType: 'REPARENT_AS_STATIC',
      }
    } else {
      // element is static parent but don't look for index
      return {
        shouldReparent: true,
        newParent: targetParentPath,
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

export function flowParentAbsoluteOrStatic(
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

function isSingleAxisAutoLayoutCompatibleWithReorder(
  metadata: ElementInstanceMetadataMap,
  parent: ElementPath,
): boolean {
  const newParentMetadata = MetadataUtils.findElementByElementPath(metadata, parent)
  const parentIsFlexLayout = MetadataUtils.isFlexLayoutedContainer(newParentMetadata)
  if (parentIsFlexLayout) {
    return true
  }
  const flowChildren = MetadataUtils.getChildren(metadata, parent).filter(
    MetadataUtils.elementParticipatesInAutoLayout,
  )
  return flowChildren.length > 1
}
