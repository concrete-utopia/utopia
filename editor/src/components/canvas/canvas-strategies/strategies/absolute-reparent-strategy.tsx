import type { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import type { ElementPath, NodeModules } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import type { IndexPosition } from '../../../../utils/utils'
import type { ProjectContentTreeRoot } from '../../../assets'
import type { AllElementProps } from '../../../editor/store/editor-state'
import type { InsertionPath } from '../../../editor/store/insertion-path'
import { CSSCursor } from '../../canvas-types'
import type { CanvasCommand } from '../../commands/commands'
import { setCursorCommand } from '../../commands/set-cursor-command'

import { setProperty } from '../../commands/set-property-command'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import { ParentBounds } from '../../controls/parent-bounds'
import { ParentOutlines } from '../../controls/parent-outlines'
import { ZeroSizedElementControls } from '../../controls/zero-sized-element-controls'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import type {
  CanvasStrategy,
  CustomStrategyState,
  InteractionCanvasState,
  InteractionLifecycle,
} from '../canvas-strategy-types'
import {
  controlWithProps,
  defaultCustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession, UpdatedPathMap } from '../interaction-state'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { honoursPropsPosition, shouldKeepMovingDraggedGroupChildren } from './absolute-utils'
import { replaceFragmentLikePathsWithTheirChildrenRecursive } from './fragment-like-helpers'
import type { ShouldAddContainLayout } from './reparent-helpers/reparent-helpers'
import { ifAllowedToReparent, isAllowedToReparent } from './reparent-helpers/reparent-helpers'
import type { ForcePins } from './reparent-helpers/reparent-property-changes'
import { getAbsoluteReparentPropertyChanges } from './reparent-helpers/reparent-property-changes'
import type { ReparentTarget } from './reparent-helpers/reparent-strategy-helpers'
import { getReparentOutcome, pathToReparent } from './reparent-utils'
import { flattenSelection } from './shared-move-strategies-helpers'

export function baseAbsoluteReparentStrategy(
  reparentTarget: ReparentTarget,
  fitness: number,
  customStrategyState: CustomStrategyState,
): CanvasStrategyFactory {
  return (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
  ): CanvasStrategy | null => {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (
      selectedElements.length === 0 ||
      interactionSession == null ||
      interactionSession.interactionData.type !== 'DRAG'
    ) {
      return null
    }

    const filteredSelectedElements = flattenSelection(selectedElements)
    const isApplicable = replaceFragmentLikePathsWithTheirChildrenRecursive(
      canvasState.startingMetadata,
      canvasState.startingAllElementProps,
      canvasState.startingElementPathTree,
      filteredSelectedElements,
    ).every((element) => {
      const elementMetadata = MetadataUtils.findElementByElementPath(
        canvasState.startingMetadata,
        element,
      )

      return (
        elementMetadata?.specialSizeMeasurements.position === 'absolute' &&
        honoursPropsPosition(canvasState, element)
      )
    })
    if (!isApplicable) {
      return null
    }
    return {
      id: `ABSOLUTE_REPARENT`,
      name: `Reparent (Abs)`,
      descriptiveLabel: 'Reparenting Absolute Elements',
      icon: {
        category: 'modalities',
        type: 'reparent-large',
      },
      controlsToRender: controlsForAbsoluteReparent(reparentTarget),
      fitness: shouldKeepMovingDraggedGroupChildren(
        canvasState.startingMetadata,
        selectedElements,
        reparentTarget.newParent,
      )
        ? 1
        : fitness,
      apply: applyAbsoluteReparent(
        canvasState,
        interactionSession,
        customStrategyState,
        reparentTarget,
        filteredSelectedElements,
      ),
    }
  }
}

export function controlsForAbsoluteReparent(reparentTarget: ReparentTarget) {
  return [
    controlWithProps({
      control: ParentOutlines,
      props: { targetParent: reparentTarget.newParent.intendedParentPath },
      key: 'parent-outlines-control',
      show: 'visible-only-while-active',
    }),
    controlWithProps({
      control: ParentBounds,
      props: { targetParent: reparentTarget.newParent.intendedParentPath },
      key: 'parent-bounds-control',
      show: 'visible-only-while-active',
    }),
    controlWithProps({
      control: ZeroSizedElementControls,
      props: { showAllPossibleElements: true },
      key: 'zero-size-control',
      show: 'visible-only-while-active',
    }),
  ]
}

export function applyAbsoluteReparent(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  customStrategyState: CustomStrategyState,
  reparentTarget: ReparentTarget,
  selectedElements: ElementPath[],
) {
  return (strategyLifecycle: InteractionLifecycle) => {
    if (
      selectedElements.length === 0 ||
      interactionSession == null ||
      interactionSession.interactionData.type !== 'DRAG'
    ) {
      return emptyStrategyApplicationResult
    }
    const dragInteractionData = interactionSession.interactionData

    const { projectContents, nodeModules } = canvasState
    const newParent = reparentTarget.newParent
    return ifAllowedToReparent(
      canvasState,
      canvasState.startingMetadata,
      selectedElements,
      newParent.intendedParentPath,
      () => {
        if (dragInteractionData.drag == null) {
          return emptyStrategyApplicationResult
        }

        const allowedToReparent = selectedElements.every((selectedElement) => {
          return isAllowedToReparent(
            canvasState.projectContents,
            canvasState.startingMetadata,
            selectedElement,
            newParent.intendedParentPath,
          )
        })

        if (reparentTarget.shouldReparent && allowedToReparent) {
          const commands = mapDropNulls(
            (selectedElement) =>
              createAbsoluteReparentAndOffsetCommands(
                selectedElement,
                newParent,
                null,
                canvasState.startingMetadata,
                canvasState.startingElementPathTree,
                canvasState.startingAllElementProps,
                canvasState.builtInDependencies,
                projectContents,
                nodeModules,
                'force-pins',
                shouldAddContainLayout(
                  canvasState.startingMetadata,
                  canvasState.startingAllElementProps,
                  canvasState.startingElementPathTree,
                  newParent.intendedParentPath,
                ),
              ),
            selectedElements,
          )

          let newPaths: Array<ElementPath> = []
          let updatedTargetPaths: UpdatedPathMap = {}

          commands.forEach((c) => {
            newPaths.push(c.newPath)
            updatedTargetPaths[EP.toString(c.oldPath)] = c.newPath
          })

          const moveCommands =
            absoluteMoveStrategy(
              canvasState,
              {
                ...interactionSession,
                updatedTargetPaths: updatedTargetPaths,
              },
              { ...defaultCustomStrategyState(), action: 'reparent' },
            )?.strategy.apply(strategyLifecycle).commands ?? []

          const elementsToRerender = EP.uniqueElementPaths([
            ...customStrategyState.elementsToRerender,
            ...newPaths,
            ...newPaths.map(EP.parentPath),
            ...selectedElements.map(EP.parentPath),
          ])
          return strategyApplicationResult(
            [
              ...moveCommands,
              ...commands.flatMap((c) => c.commands),
              updateSelectedViews('always', newPaths),
              ...maybeAddContainLayout(
                canvasState.startingMetadata,
                canvasState.startingAllElementProps,
                canvasState.startingElementPathTree,
                newParent.intendedParentPath,
              ),
              setCursorCommand(CSSCursor.Reparent),
            ],
            elementsToRerender,
            {
              elementsToRerender,
            },
          )
        } else {
          return (
            absoluteMoveStrategy(canvasState, interactionSession, {
              ...defaultCustomStrategyState(),
              action: 'reparent',
            })?.strategy.apply(strategyLifecycle) ?? emptyStrategyApplicationResult
          )
        }
      },
    )
  }
}

export function createAbsoluteReparentAndOffsetCommands(
  target: ElementPath,
  newParent: InsertionPath,
  indexPosition: IndexPosition | null,
  metadata: ElementInstanceMetadataMap,
  pathTree: ElementPathTrees,
  elementProps: AllElementProps,
  builtInDependencies: BuiltInDependencies,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  forcePins: ForcePins,
  containLayout: ShouldAddContainLayout,
) {
  const reparentResult = getReparentOutcome(
    metadata,
    pathTree,
    elementProps,
    builtInDependencies,
    projectContents,
    nodeModules,
    pathToReparent(target),
    newParent,
    'always',
    indexPosition,
  )

  if (reparentResult == null) {
    return null
  } else {
    const replacedPaths = replaceFragmentLikePathsWithTheirChildrenRecursive(
      metadata,
      elementProps,
      pathTree,
      [target],
    )
    const offsetCommands = replacedPaths.flatMap((p) => {
      return getAbsoluteReparentPropertyChanges(
        p,
        newParent.intendedParentPath,
        metadata,
        metadata,
        projectContents,
        forcePins,
        containLayout,
      )
    })

    const { commands: reparentCommands, newPath } = reparentResult
    return {
      oldPath: target,
      newPath: newPath,
      commands: [...offsetCommands, ...reparentCommands],
    }
  }
}

export function shouldAddContainLayout(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  path: ElementPath,
): ShouldAddContainLayout {
  const closestNonFragmentParent = MetadataUtils.getClosestNonFragmentParent(
    metadata,
    allElementProps,
    pathTrees,
    path,
  )

  if (EP.isStoryboardPath(closestNonFragmentParent)) {
    return 'add-contain-layout'
  }

  const parentProvidesBoundsForAbsoluteChildren =
    MetadataUtils.findElementByElementPath(metadata, closestNonFragmentParent)
      ?.specialSizeMeasurements.providesBoundsForAbsoluteChildren === true

  return parentProvidesBoundsForAbsoluteChildren ? 'dont-add-contain-layout' : 'add-contain-layout'
}

function maybeAddContainLayout(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  path: ElementPath,
): CanvasCommand[] {
  return shouldAddContainLayout(metadata, allElementProps, pathTrees, path) === 'add-contain-layout'
    ? [setProperty('always', path, PP.create('style', 'contain'), 'layout')]
    : []
}
