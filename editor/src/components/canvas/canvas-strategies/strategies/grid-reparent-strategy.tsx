import type { NodeModules, ProjectContentTreeRoot } from 'utopia-shared/src/types'
import type { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import {
  type ElementInstanceMetadata,
  type ElementInstanceMetadataMap,
} from '../../../../core/shared/element-template'
import type { CanvasRectangle } from '../../../../core/shared/math-utils'
import { isInfinityRectangle } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import type { AllElementProps } from '../../../editor/store/editor-state'
import type { InsertionPath } from '../../../editor/store/insertion-path'
import { CSSCursor } from '../../canvas-types'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { propertyToSet, updateBulkProperties } from '../../commands/set-property-command'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import { controlsForGridPlaceholders } from '../../controls/grid-controls'
import { ParentBounds } from '../../controls/parent-bounds'
import { ParentOutlines } from '../../controls/parent-outlines'
import { ZeroSizedElementControls } from '../../controls/zero-sized-element-controls'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import type {
  CanvasStrategy,
  ControlWithProps,
  CustomStrategyState,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { DragInteractionData, InteractionSession, UpdatedPathMap } from '../interaction-state'
import { honoursPropsPosition, shouldKeepMovingDraggedGroupChildren } from './absolute-utils'
import { replaceFragmentLikePathsWithTheirChildrenRecursive } from './fragment-like-helpers'
import { getMetadataWithGridCellBounds, runGridRearrangeMove } from './grid-helpers'
import { ifAllowedToReparent, isAllowedToReparent } from './reparent-helpers/reparent-helpers'
import { removeAbsolutePositioningProps } from './reparent-helpers/reparent-property-changes'
import type { ReparentTarget } from './reparent-helpers/reparent-strategy-helpers'
import { getReparentOutcome, pathToReparent } from './reparent-utils'
import { flattenSelection } from './shared-move-strategies-helpers'

export function gridReparentStrategy(
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

    const gridFrame = MetadataUtils.findElementByElementPath(
      canvasState.startingMetadata,
      reparentTarget.newParent.intendedParentPath,
    )?.globalFrame
    if (gridFrame == null || isInfinityRectangle(gridFrame)) {
      return null
    }

    const dragInteractionData = interactionSession.interactionData
    const filteredSelectedElements = flattenSelection(selectedElements)
    const isApplicable = replaceFragmentLikePathsWithTheirChildrenRecursive(
      canvasState.startingMetadata,
      canvasState.startingAllElementProps,
      canvasState.startingElementPathTree,
      filteredSelectedElements,
    ).every((element) => {
      return honoursPropsPosition(canvasState, element)
    })
    if (!isApplicable) {
      return null
    }
    return {
      id: `GRID_REPARENT`,
      name: `Reparent (Grid)`,
      descriptiveLabel: 'Reparent (Grid)',
      icon: {
        category: 'modalities',
        type: 'reparent-large',
      },
      controlsToRender: controlsForGridReparent(reparentTarget),
      fitness: shouldKeepMovingDraggedGroupChildren(
        canvasState.startingMetadata,
        selectedElements,
        reparentTarget.newParent,
      )
        ? 1
        : fitness,
      apply: applyGridReparent(
        canvasState,
        dragInteractionData,
        interactionSession,
        customStrategyState,
        reparentTarget,
        filteredSelectedElements,
        gridFrame,
      ),
    }
  }
}

export function controlsForGridReparent(reparentTarget: ReparentTarget): ControlWithProps<any>[] {
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
    controlsForGridPlaceholders(reparentTarget.newParent.intendedParentPath),
  ]
}

export function applyGridReparent(
  canvasState: InteractionCanvasState,
  interactionData: DragInteractionData,
  interactionSession: InteractionSession,
  customStrategyState: CustomStrategyState,
  reparentTarget: ReparentTarget,
  selectedElements: ElementPath[],
  gridFrame: CanvasRectangle,
) {
  return () => {
    if (interactionData.drag == null || selectedElements.length === 0) {
      return emptyStrategyApplicationResult
    }

    const { projectContents, nodeModules } = canvasState
    const newParent = reparentTarget.newParent
    return ifAllowedToReparent(
      canvasState,
      canvasState.startingMetadata,
      selectedElements,
      newParent.intendedParentPath,
      () => {
        if (interactionData.drag == null || selectedElements.length === 0) {
          return emptyStrategyApplicationResult
        }

        const { metadata: grid, customStrategyState: updatedCustomState } =
          getMetadataWithGridCellBounds(
            newParent.intendedParentPath,
            canvasState.startingMetadata,
            interactionSession.latestMetadata,
            customStrategyState,
          )

        if (grid == null) {
          return strategyApplicationResult([], [newParent.intendedParentPath])
        }

        const allowedToReparent = selectedElements.every((selectedElement) => {
          return isAllowedToReparent(
            canvasState.projectContents,
            canvasState.startingMetadata,
            selectedElement,
            newParent.intendedParentPath,
          )
        })

        if (!(reparentTarget.shouldReparent && allowedToReparent)) {
          return emptyStrategyApplicationResult
        }

        const outcomes = mapDropNulls(
          (selectedElement) =>
            gridReparentCommands(
              canvasState.startingMetadata,
              canvasState.startingElementPathTree,
              canvasState.startingAllElementProps,
              canvasState.builtInDependencies,
              projectContents,
              nodeModules,
              selectedElement,
              newParent,
              interactionData,
              grid,
            ),
          selectedElements,
        )

        let newPaths: Array<ElementPath> = []
        let updatedTargetPaths: UpdatedPathMap = {}

        outcomes.forEach((c) => {
          newPaths.push(c.newPath)
          updatedTargetPaths[EP.toString(c.oldPath)] = c.newPath
        })

        const gridContainerCommands = updateBulkProperties(
          'mid-interaction',
          reparentTarget.newParent.intendedParentPath,
          [
            propertyToSet(PP.create('style', 'width'), gridFrame.width),
            propertyToSet(PP.create('style', 'height'), gridFrame.height),
          ],
        )

        const elementsToRerender = EP.uniqueElementPaths([
          ...customStrategyState.elementsToRerender,
          ...newPaths,
          ...newPaths.map(EP.parentPath),
          ...selectedElements.map(EP.parentPath),
          newParent.intendedParentPath,
        ])

        const baseCustomState = updatedCustomState ?? customStrategyState
        const customStrategyStatePatch = {
          ...baseCustomState,
          elementsToRerender: elementsToRerender,
        }

        return strategyApplicationResult(
          [
            ...outcomes.flatMap((c) => c.commands),
            gridContainerCommands,
            updateSelectedViews('always', newPaths),
            setCursorCommand(CSSCursor.Reparent),
          ],
          elementsToRerender,
          customStrategyStatePatch,
        )
      },
    )
  }
}

function gridReparentCommands(
  jsxMetadata: ElementInstanceMetadataMap,
  tree: ElementPathTrees,
  allElementProps: AllElementProps,
  builtinDependencies: BuiltInDependencies,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  target: ElementPath,
  newParent: InsertionPath,
  interactionData: DragInteractionData,
  grid: ElementInstanceMetadata,
) {
  const reparentResult = getReparentOutcome(
    jsxMetadata,
    tree,
    allElementProps,
    builtinDependencies,
    projectContents,
    nodeModules,
    pathToReparent(target),
    newParent,
    'always',
    null,
  )

  if (reparentResult == null) {
    return null
  }
  const gridPropsCommands = runGridRearrangeMove(target, target, jsxMetadata, interactionData, grid)

  if (gridPropsCommands == null) {
    return null
  }

  const { commands: reparentCommands, newPath } = reparentResult

  const removeAbsolutePositioningPropsCommands = removeAbsolutePositioningProps('always', newPath)

  return {
    commands: [...gridPropsCommands, ...reparentCommands, removeAbsolutePositioningPropsCommands],
    newPath: newPath,
    oldPath: target,
  }
}
