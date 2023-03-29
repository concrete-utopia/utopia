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

export type ReparentStrategy = 'REPARENT_AS_ABSOLUTE' | 'REPARENT_AS_STATIC'

export type FindReparentStrategyResult = {
  strategy: ReparentStrategy
  isFallback: boolean
  target: ReparentTarget
}

export function reparentStrategyForPaste(
  targetMetadata: ElementInstanceMetadataMap,
  parent: ElementPath,
): {
  strategy: ReparentStrategy
  isFallback: boolean
} {
  const newParentMetadata = MetadataUtils.findElementByElementPath(targetMetadata, parent)
  const parentIsFlexLayout = MetadataUtils.isFlexLayoutedContainer(newParentMetadata)

  const flowParentReparentType = flowParentAbsoluteOrStatic(targetMetadata, parent)
  const reparentAsStatic = parentIsFlexLayout || flowParentReparentType === 'REPARENT_AS_STATIC'
  if (reparentAsStatic) {
    return {
      strategy: 'REPARENT_AS_STATIC',
      isFallback: false,
    }
  } else {
    return {
      strategy: 'REPARENT_AS_ABSOLUTE',
      isFallback: false,
    }
  }
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
  newParent: ElementPath
  shouldShowPositionIndicator: boolean
  newIndex: number
  shouldConvertToInline: Direction | 'do-not-convert'
  defaultReparentType: ReparentStrategy
}

export function reparentTarget(
  shouldReparent: boolean,
  newParent: ElementPath,
  shouldShowPositionIndicator: boolean,
  newIndex: number,
  shouldConvertToInline: Direction | 'do-not-convert',
  defaultReparentType: ReparentStrategy,
): ReparentTarget {
  return {
    shouldReparent: shouldReparent,
    newParent: newParent,
    shouldShowPositionIndicator: shouldShowPositionIndicator,
    newIndex: newIndex,
    shouldConvertToInline: shouldConvertToInline,
    defaultReparentType: defaultReparentType,
  }
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
