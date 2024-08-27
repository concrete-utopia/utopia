import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../../../../core/shared/element-template'
import type { CanvasPoint, Size } from '../../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../../core/shared/project-file-types'
import type { InteractionCanvasState, InteractionTarget } from '../../canvas-strategy-types'
import { getTargetPathsFromInteractionTarget } from '../../canvas-strategy-types'
import type { AllowSmallerParent } from '../../interaction-state'
import {
  autoLayoutParentAbsoluteOrStatic,
  getReparentTargetUnified,
} from './reparent-strategy-parent-lookup'
import { flattenSelection } from '../shared-move-strategies-helpers'
import type { Direction } from '../../../../inspector/common/css-utils'
import type { ElementSupportsChildren } from '../../../../../core/model/element-template-utils'
import type { AllElementProps } from '../../../../editor/store/editor-state'
import type { InsertionPath } from '../../../../editor/store/insertion-path'
import type { ElementPathTrees } from '../../../../../core/shared/element-path-tree'
import { assertNever } from '../../../../../core/shared/utils'
import * as EP from '../../../../../core/shared/element-path'

export type ReparentAsAbsolute = 'REPARENT_AS_ABSOLUTE'
export type ReparentAsStatic = 'REPARENT_AS_STATIC'
export type ReparentIntoGrid = 'REPARENT_INTO_GRID'
export type ReparentStrategy = ReparentAsAbsolute | ReparentAsStatic | ReparentIntoGrid

export type FindReparentStrategyResult = {
  strategy: ReparentStrategy
  isFallback: boolean
  isReparentingOutFromScene: boolean
  target: ReparentTarget
}

export type StaticReparentTarget =
  | { type: ReparentAsStatic; insertionPath: InsertionPath }
  | {
      type: ReparentAsAbsolute
      insertionPath: InsertionPath
    }

export function reparentStrategyForPaste(
  currentMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  parent: ElementPath,
): ReparentStrategy {
  return autoLayoutParentAbsoluteOrStatic(
    currentMetadata,
    allElementProps,
    pathTrees,
    parent,
    'prefer-absolute',
  )
}

export function findReparentStrategies(
  canvasState: InteractionCanvasState,
  cmdPressed: boolean,
  pointOnCanvas: CanvasPoint,
  allowSmallerParent: AllowSmallerParent,
  elementSupportsChildren: Array<ElementSupportsChildren> = ['supportsChildren'],
): Array<FindReparentStrategyResult> {
  const metadata = canvasState.startingMetadata
  const reparentSubjects = reparentSubjectsForInteractionTarget(canvasState.interactionTarget)
  const targetParent = getReparentTargetUnified(
    reparentSubjects,
    pointOnCanvas,
    cmdPressed,
    canvasState,
    metadata,
    canvasState.startingElementPathTree,
    canvasState.startingAllElementProps,
    allowSmallerParent,
    elementSupportsChildren,
    canvasState.propertyControlsInfo,
  )

  if (targetParent == null) {
    return []
  }

  const isOutFromScene = isReparentingOutFromScene(
    reparentSubjects,
    canvasState.startingMetadata,
    targetParent.newParent.intendedParentPath,
  )

  const strategy = {
    target: targetParent,
    isReparentingOutFromScene: isOutFromScene,
    strategy: targetParent.defaultReparentType,
    isFallback: false,
  }

  const fallbackStrategy: FindReparentStrategyResult = {
    isFallback: true,
    isReparentingOutFromScene: isOutFromScene,
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
  newParent: InsertionPath
  shouldShowPositionIndicator: boolean
  newIndex: number
  shouldConvertToInline: Direction | 'do-not-convert'
  defaultReparentType: ReparentStrategy
}

export type ReparentSubjects = NewReparentSubjects | ExistingReparentSubjects

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

export function getExistingElementsFromReparentSubjects(
  reparentSubjects: ReparentSubjects,
): Array<ElementPath> {
  switch (reparentSubjects.type) {
    case 'EXISTING_ELEMENTS':
      return reparentSubjects.elements
    case 'NEW_ELEMENTS':
      return []
    default:
      assertNever(reparentSubjects)
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
        flattenSelection(getTargetPathsFromInteractionTarget(interactionTarget)),
      )
    default:
      const _exhaustiveCheck: never = interactionTarget
      throw new Error(`Unhandled interaction target type ${JSON.stringify(interactionTarget)}`)
  }
}

function isReparentingOutFromScene(
  reparentSubjects: ReparentSubjects,
  metadata: ElementInstanceMetadataMap,
  targetParent: ElementPath,
) {
  const reparentSubjectScenes =
    reparentSubjects.type === 'EXISTING_ELEMENTS'
      ? reparentSubjects.elements.map((s) => MetadataUtils.findSceneOfTarget(s, metadata))
      : []

  const reparentTargetScene = MetadataUtils.findSceneOfTarget(targetParent, metadata)

  return reparentSubjectScenes.some((s) => !EP.pathsEqual(s, reparentTargetScene))
}
