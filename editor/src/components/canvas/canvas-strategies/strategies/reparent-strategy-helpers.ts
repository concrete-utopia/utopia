import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import {
  framePointForPinnedProp,
  LayoutPinnedProp,
} from '../../../../core/layout/layout-helpers-new'
import {
  flexDirectionToFlexForwardsOrBackwards,
  flexDirectionToSimpleFlexDirection,
  FlexForwardsOrBackwards,
  SimpleFlexDirection,
} from '../../../../core/layout/layout-utils'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { getStoryboardElementPath } from '../../../../core/model/scene-utils'
import { mapDropNulls, reverse, stripNulls } from '../../../../core/shared/array-utils'
import { isRight, right } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import { ElementInstanceMetadataMap, JSXElement } from '../../../../core/shared/element-template'
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
import { assertNever } from '../../../../core/shared/utils'
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
  StrategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession, MissingBoundsHandling } from '../interaction-state'
import { ifAllowedToReparent } from './reparent-helpers'
import { getReparentOutcome, pathToReparent } from './reparent-utils'
import { getDragTargets } from './shared-move-strategies-helpers'

export type ReparentStrategy = 'REPARENT_AS_ABSOLUTE' | 'REPARENT_AS_STATIC'

export type FindReparentStrategyResult = {
  strategy: ReparentStrategy
  missingBoundsHandling: MissingBoundsHandling
  isFallback: boolean
  target: ReparentTarget
}

export function reparentStrategyForParent(
  targetMetadata: ElementInstanceMetadataMap,
  parent: ElementPath,
  convertToAbsolute: boolean,
): {
  strategy: ReparentStrategy
  missingBoundsHandling: MissingBoundsHandling
  isFallback: boolean
} {
  const newParentMetadata = MetadataUtils.findElementByElementPath(targetMetadata, parent)
  const parentIsFlexLayout = MetadataUtils.isFlexLayoutedContainer(newParentMetadata)

  const parentProvidesBoundsForAbsoluteChildren =
    newParentMetadata?.specialSizeMeasurements.providesBoundsForAbsoluteChildren ?? false

  const parentIsStoryboard = EP.isStoryboardPath(parent)
  const isAbsoluteFriendlyParent = parentProvidesBoundsForAbsoluteChildren || parentIsStoryboard
  const missingBoundsHandling: MissingBoundsHandling = isAbsoluteFriendlyParent
    ? 'use-strict-bounds'
    : 'allow-missing-bounds'

  const flowParentReparentType = flowParentAbsoluteOrStatic(targetMetadata, parent)
  const reparentAsStatic =
    !convertToAbsolute && (parentIsFlexLayout || flowParentReparentType === 'REPARENT_AS_STATIC')

  if (reparentAsStatic) {
    return {
      strategy: 'REPARENT_AS_STATIC',
      missingBoundsHandling: missingBoundsHandling,
      isFallback: false,
    }
  } else {
    return {
      strategy: 'REPARENT_AS_ABSOLUTE',
      missingBoundsHandling: missingBoundsHandling,
      isFallback: convertToAbsolute,
    }
  }
}

function flowParentAbsoluteOrStatic(
  metadata: ElementInstanceMetadataMap,
  parent: ElementPath,
): 'REPARENT_AS_ABSOLUTE' | 'REPARENT_AS_STATIC' {
  const parentMetadata = MetadataUtils.findElementByElementPath(metadata, parent)
  const children = MetadataUtils.getChildren(metadata, parent)

  const storyboardRoot = EP.isStoryboardPath(parent)
  if (storyboardRoot) {
    // always reparent as absolute to the Storyboard
    return 'REPARENT_AS_ABSOLUTE'
  }

  if (parentMetadata == null) {
    // should this throw an error?
    return 'REPARENT_AS_STATIC'
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

  // TODO ABSOLUTE drag onto the padded area of display: flow target parent

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

function reparentStrategyForReparentTarget(
  targetMetadata: ElementInstanceMetadataMap,
  target: ReparentTarget,
  convertToAbsolute: boolean,
): FindReparentStrategyResult {
  return {
    ...reparentStrategyForParent(targetMetadata, target.newParent, convertToAbsolute),
    target: target,
  }
}

export function findReparentStrategies(
  canvasState: InteractionCanvasState,
  cmdPressed: boolean,
  pointOnCanvas: CanvasPoint,
): Array<FindReparentStrategyResult> {
  const metadata = canvasState.startingMetadata

  const reparentSubjects =
    canvasState.interactionTarget.type === 'INSERTION_SUBJECTS' &&
    canvasState.interactionTarget.subjects.length > 0
      ? newReparentSubjects(canvasState.interactionTarget.subjects[0].defaultSize)
      : existingReparentSubjects(
          getDragTargets(getTargetPathsFromInteractionTarget(canvasState.interactionTarget)), // uhh
        )

  const getReparentTargetInner = (missingBoundsHandling: MissingBoundsHandling) =>
    getReparentTargetUnified(
      reparentSubjects,
      pointOnCanvas,
      cmdPressed,
      canvasState,
      metadata,
      canvasState.startingAllElementProps,
      missingBoundsHandling,
    )

  const strictTarget = getReparentTargetInner('use-strict-bounds')
  const strictStrategy =
    strictTarget == null ? null : reparentStrategyForReparentTarget(metadata, strictTarget, false)

  const forcedTarget = getReparentTargetInner('allow-missing-bounds')
  const sameTargets =
    strictTarget != null &&
    forcedTarget != null &&
    EP.pathsEqual(forcedTarget.newParent, strictTarget.newParent)
  const convertToAbsolute = sameTargets && strictStrategy?.strategy === 'REPARENT_AS_STATIC'
  const skipForcedTarget = sameTargets && !convertToAbsolute
  const forcedStrategy =
    forcedTarget == null || skipForcedTarget
      ? null
      : reparentStrategyForReparentTarget(metadata, forcedTarget, convertToAbsolute)

  return stripNulls([strictStrategy, forcedStrategy])
}

export function findReparentStrategies2(
  canvasState: InteractionCanvasState,
  cmdPressed: boolean,
  pointOnCanvas: CanvasPoint,
): Array<FindReparentStrategyResult> {
  const metadata = canvasState.startingMetadata

  const reparentSubjects =
    canvasState.interactionTarget.type === 'INSERTION_SUBJECTS'
      ? newReparentSubjects(canvasState.interactionTarget.subjects[0].defaultSize)
      : existingReparentSubjects(
          getDragTargets(getTargetPathsFromInteractionTarget(canvasState.interactionTarget)), // uhh
        )

  const targetParent = getReparentTargetUnified(
    reparentSubjects,
    pointOnCanvas,
    cmdPressed,
    canvasState,
    metadata,
    canvasState.startingAllElementProps,
    'allow-missing-bounds', // TODO delete this property!
  )

  if (targetParent == null) {
    return [] // TODO is canvas a null target parent?
  }

  const strategy = reparentStrategyForReparentTarget(metadata, targetParent, false)

  const forcedAbsoluteStrategy =
    strategy?.strategy === 'REPARENT_AS_ABSOLUTE'
      ? null
      : reparentStrategyForReparentTarget(metadata, targetParent, true)

  const optionalReparentAsFlowStrategy: FindReparentStrategyResult | null =
    strategy.strategy === 'REPARENT_AS_ABSOLUTE'
      ? {
          isFallback: true, // TODO is this true?
          missingBoundsHandling: 'use-strict-bounds', // TODO is this true?
          target: strategy.target,
          strategy: 'REPARENT_AS_STATIC',
        }
      : null

  return stripNulls([strategy, forcedAbsoluteStrategy, optionalReparentAsFlowStrategy])
}

export interface ReparentTarget {
  shouldReparent: boolean
  newParent: ElementPath
  shouldReorder: boolean
  newIndex: number
}

export function reparentTarget(
  shouldReparent: boolean,
  newParent: ElementPath,
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

export function getReparentTargetUnified(
  reparentSubjects: ReparentSubjects,
  pointOnCanvas: CanvasPoint,
  cmdPressed: boolean,
  canvasState: InteractionCanvasState,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  missingBoundsHandling: MissingBoundsHandling,
): ReparentTarget | null {
  const projectContents = canvasState.projectContents
  const openFile = canvasState.openFile ?? null
  const canvasScale = canvasState.scale

  const multiselectBounds: Size =
    (reparentSubjects.type === 'EXISTING_ELEMENTS'
      ? MetadataUtils.getBoundingRectangleInCanvasCoords(reparentSubjects.elements, metadata)
      : reparentSubjects.defaultSize) ?? size(0, 0)

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
    return storyboardComponent == null
      ? null
      : {
          shouldReparent: true,
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
          !cmdPressed && maybeAncestorOrEqual.specialSizeMeasurements.position !== 'absolute' // TODO if we work on Reorder for Relative Elements, this will probably need to change
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
    return null
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
  return siblingPositions
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
  targetLayout: 'flex' | 'flow',
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

            function midInteractionCommandsForTarget(): Array<CanvasCommand> {
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
              switch (targetLayout) {
                case 'flow':
                  return commonPatches
                case 'flex':
                  return [
                    ...commonPatches,
                    showReorderIndicator(newParent, newIndex),
                    wildcardPatch('mid-interaction', {
                      displayNoneInstances: { $push: [newPath] },
                    }),
                  ]
                default:
                  assertNever(targetLayout)
              }
            }

            let interactionFinishCommands: Array<CanvasCommand>
            let midInteractionCommands: Array<CanvasCommand>

            if (reparentResult.shouldReorder && siblingsOfTarget.length > 0) {
              midInteractionCommands = midInteractionCommandsForTarget()

              interactionFinishCommands = [
                ...commandsBeforeReorder,
                reorderElement('always', newPath, absolute(newIndex)),
                ...commandsAfterReorder,
              ]
            } else {
              if (parentRect != null) {
                midInteractionCommands = midInteractionCommandsForTarget()
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
): Array<CanvasCommand> {
  if (targetOriginalStylePosition !== 'absolute' && targetOriginalStylePosition !== 'relative') {
    return [deleteProperties('always', newPath, propertiesToRemove)]
  }

  return [
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
      return getStaticReparentPropertyChanges(newPath, targetOriginalStylePosition)
  }
}
