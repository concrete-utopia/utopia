import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../../../core/shared/element-template'
import { CanvasPoint, Size } from '../../../../../core/shared/math-utils'
import { ElementPath } from '../../../../../core/shared/project-file-types'
import {
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  InteractionTarget,
} from '../../canvas-strategy-types'
import { AllowSmallerParent } from '../../interaction-state'
import {
  flowParentAbsoluteOrStatic,
  getReparentTargetUnified,
} from './reparent-strategy-parent-lookup'
import { flattenSelection } from '../shared-move-strategies-helpers'
import { Direction } from '../../../../inspector/common/css-utils'
import { ElementSupportsChildren } from '../../../../../core/model/element-template-utils'
import { AllElementProps } from '../../../../editor/store/editor-state'
import { InsertionPath } from '../../../../editor/store/insertion-path'

export type ReparentAsAbsolute = 'REPARENT_AS_ABSOLUTE'
export type ReparentAsStatic = 'REPARENT_AS_STATIC'
export type ReparentStrategy = ReparentAsAbsolute | ReparentAsStatic

export type FindReparentStrategyResult = {
  strategy: ReparentStrategy
  isFallback: boolean
  target: ReparentTarget
}

export function reparentStrategyForPaste(
  currentMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  parent: ElementPath,
): ReparentStrategy {
  const newParentMetadata = MetadataUtils.findElementByElementPath(currentMetadata, parent)
  const parentIsFlexLayout = MetadataUtils.isFlexLayoutedContainer(newParentMetadata)

  const flowParentReparentType = flowParentAbsoluteOrStatic(
    currentMetadata,
    allElementProps,
    parent,
  )
  const reparentAsStatic = parentIsFlexLayout || flowParentReparentType === 'REPARENT_AS_STATIC'
  return reparentAsStatic ? 'REPARENT_AS_STATIC' : 'REPARENT_AS_ABSOLUTE'
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
    canvasState.nodeModules,
    canvasState.startingAllElementProps,
    allowSmallerParent,
    elementSupportsChildren,
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
