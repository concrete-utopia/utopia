import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { CanvasPoint, CanvasVector } from '../../../../core/shared/math-utils'
import {
  canvasPoint,
  canvasRectangle,
  zeroCanvasRect,
  zeroRectIfNullOrInfinity,
} from '../../../../core/shared/math-utils'
import { assertNever } from '../../../../core/shared/utils'
import { absolute } from '../../../../utils/utils'
import { CSSCursor } from '../../canvas-types'
import type { CanvasCommand } from '../../commands/commands'
import { reorderElement } from '../../commands/reorder-element-command'
import { activeFrameTargetRect, setActiveFrames } from '../../commands/set-active-frames-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { showReorderIndicator } from '../../commands/show-reorder-indicator-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import { ParentBounds } from '../../controls/parent-bounds'
import { ParentOutlines } from '../../controls/parent-outlines'
import { FlexReparentTargetIndicator } from '../../controls/select-mode/flex-reparent-target-indicator'
import { StaticReparentTargetOutlineIndicator } from '../../controls/select-mode/static-reparent-target-outline'
import { ZeroSizedElementControls } from '../../controls/zero-sized-element-controls'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import type {
  CanvasStrategy,
  CustomStrategyState,
  InteractionCanvasState,
  StrategyApplicationResult,
} from '../canvas-strategy-types'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
} from '../canvas-strategy-types'
import type { DragInteractionData, InteractionSession } from '../interaction-state'
import { shouldKeepMovingDraggedGroupChildren } from './absolute-utils'
import { ifAllowedToReparent } from './reparent-helpers/reparent-helpers'
import { getStaticReparentPropertyChanges } from './reparent-helpers/reparent-property-changes'
import type { ReparentTarget } from './reparent-helpers/reparent-strategy-helpers'
import { getReparentOutcome, pathToReparent, placeholderCloneCommands } from './reparent-utils'
import { flattenSelection } from './shared-move-strategies-helpers'

export function baseReparentAsStaticStrategy(
  reparentTarget: ReparentTarget,
  fitness: number,
  targetLayout: 'flex' | 'flow',
): CanvasStrategyFactory {
  return (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
    customStrategyState: CustomStrategyState,
  ): CanvasStrategy | null => {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    const filteredSelectedElements = flattenSelection(selectedElements)
    if (
      filteredSelectedElements.length !== 1 ||
      interactionSession == null ||
      interactionSession.interactionData.type !== 'DRAG'
    ) {
      return null
    }

    return {
      ...getDescriptivePropertiesOfReparentToStaticStrategy(targetLayout),
      controlsToRender: controlsForStaticReparent(reparentTarget),
      fitness: shouldKeepMovingDraggedGroupChildren(
        canvasState.startingMetadata,
        selectedElements,
        reparentTarget.newParent,
      )
        ? 1
        : fitness,
      apply: () => {
        return applyStaticReparent(
          canvasState,
          interactionSession,
          customStrategyState,
          reparentTarget,
        )
      },
    }
  }
}

export function controlsForStaticReparent(reparentTarget: ReparentTarget) {
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
      control: FlexReparentTargetIndicator,
      props: {},
      key: 'flex-reparent-target-indicator',
      show: 'visible-only-while-active',
    }),
    controlWithProps({
      control: ZeroSizedElementControls,
      props: { showAllPossibleElements: true },
      key: 'zero-size-control',
      show: 'visible-only-while-active',
    }),
    controlWithProps({
      control: StaticReparentTargetOutlineIndicator,
      props: {},
      key: 'parent-outline-highlight',
      show: 'visible-only-while-active',
    }),
  ]
}

function getDescriptivePropertiesOfReparentToStaticStrategy(
  targetLayout: 'flex' | 'flow',
): Pick<CanvasStrategy, 'id' | 'name' | 'descriptiveLabel' | 'icon'> {
  switch (targetLayout) {
    case 'flex':
      return {
        id: 'REPARENT_TO_FLEX',
        name: 'Reparent (Flex)',
        descriptiveLabel: 'Reparenting Into A Flex Element',
        icon: {
          category: 'modalities',
          type: 'reparent-large',
        },
      }
    case 'flow':
      return {
        id: 'REPARENT_TO_FLOW',
        name: 'Reparent (Flow)',
        descriptiveLabel: 'Reparenting Into A Flow Element',
        icon: {
          category: 'modalities',
          type: 'reparent-large',
        },
      }
    default:
      assertNever(targetLayout)
  }
}

export function applyStaticReparent(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  customStrategyState: CustomStrategyState,
  reparentResult: ReparentTarget,
): StrategyApplicationResult {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  const filteredSelectedElements = flattenSelection(selectedElements)
  const newParent = reparentResult.newParent

  return ifAllowedToReparent(
    canvasState,
    canvasState.startingMetadata,
    filteredSelectedElements,
    newParent.intendedParentPath,
    () => {
      if (
        interactionSession.interactionData.type == 'DRAG' &&
        interactionSession.interactionData.drag != null
      ) {
        if (reparentResult.shouldReparent && filteredSelectedElements.length === 1) {
          const target = filteredSelectedElements[0]

          const newIndex = reparentResult.newIndex
          const parentRect =
            MetadataUtils.getFrameInCanvasCoords(
              newParent.intendedParentPath,
              canvasState.startingMetadata,
            ) ?? zeroCanvasRect

          const siblingsOfTarget = MetadataUtils.getChildrenPathsOrdered(
            canvasState.startingElementPathTree,
            newParent.intendedParentPath,
          )

          // Reparent the element.
          const outcomeResult = getReparentOutcome(
            canvasState.startingMetadata,
            canvasState.startingElementPathTree,
            canvasState.startingAllElementProps,
            canvasState.builtInDependencies,
            canvasState.projectContents,
            canvasState.nodeModules,
            pathToReparent(target),
            newParent,
            'always',
            null,
          )
          let duplicatedElementNewUids: { [elementPath: string]: string } = {}
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

            const elementsToRerender = EP.uniqueElementPaths([
              ...customStrategyState.elementsToRerender,
              target,
              EP.parentPath(newPath),
              EP.parentPath(target),
            ])

            const commandsAfterReorder = [
              ...propertyChangeCommands,

              updateHighlightedViews('mid-interaction', []),
              setCursorCommand(CSSCursor.Move),
            ]

            function midInteractionCommandsForTarget(
              shouldShowPositionIndicator: boolean,
            ): Array<CanvasCommand> {
              // we want to keep a placeholder element where the original dragged element was to avoid the new parent shifting around on the screen
              const placeholderResult = placeholderCloneCommands(
                canvasState,
                customStrategyState,
                filteredSelectedElements,
                newParent.intendedParentPath,
              )

              const commonPatches: Array<CanvasCommand> = [
                wildcardPatch('mid-interaction', {
                  canvas: {
                    controls: { parentHighlightPaths: { $set: [newParent.intendedParentPath] } },
                  },
                }),
                wildcardPatch('mid-interaction', {
                  displayNoneInstances: { $push: [newPath] },
                }),
                ...placeholderResult.commands,
              ]
              duplicatedElementNewUids = placeholderResult.duplicatedElementNewUids

              commonPatches.push(
                setActiveFrames(
                  filteredSelectedElements.map((path) => {
                    const source = zeroRectIfNullOrInfinity(
                      MetadataUtils.getFrameOrZeroRectInCanvasCoords(
                        path,
                        canvasState.startingMetadata,
                      ),
                    )

                    function getTargetCoord(
                      interactionData: DragInteractionData,
                      axis: 'x' | 'y',
                    ): number {
                      return (
                        interactionData.originalDragStart[axis] +
                        (interactionData.drag?.[axis] ?? 0) -
                        (interactionData.originalDragStart[axis] - source[axis])
                      )
                    }

                    const targetPosition =
                      interactionSession.interactionData.type === 'DRAG'
                        ? canvasPoint({
                            x: getTargetCoord(interactionSession.interactionData, 'x'),
                            y: getTargetCoord(interactionSession.interactionData, 'y'),
                          })
                        : canvasPoint(source)

                    return {
                      action: 'reparent',
                      target: activeFrameTargetRect(
                        canvasRectangle({
                          x: targetPosition.x,
                          y: targetPosition.y,
                          width: source.width,
                          height: source.height,
                        }),
                      ),
                      source: source,
                    }
                  }),
                ),
              )

              if (shouldShowPositionIndicator) {
                return [
                  ...commonPatches,
                  showReorderIndicator(newParent.intendedParentPath, newIndex),
                ]
              } else {
                return [
                  ...commonPatches,
                  wildcardPatch('mid-interaction', {
                    canvas: {
                      controls: { parentOutlineHighlight: { $set: newParent.intendedParentPath } },
                    },
                  }),
                ]
              }
            }

            let interactionFinishCommands: Array<CanvasCommand>
            let midInteractionCommands: Array<CanvasCommand>

            if (reparentResult.shouldShowPositionIndicator && siblingsOfTarget.length > 0) {
              midInteractionCommands = midInteractionCommandsForTarget(
                reparentResult.shouldShowPositionIndicator,
              )

              interactionFinishCommands = [
                ...commandsBeforeReorder,
                reorderElement('always', newPath, absolute(newIndex)),
                ...commandsAfterReorder,
              ]
            } else {
              if (parentRect != null) {
                midInteractionCommands = midInteractionCommandsForTarget(
                  reparentResult.shouldShowPositionIndicator,
                )
              } else {
                // this should be an error because parentRect should never be null
                midInteractionCommands = []
              }

              interactionFinishCommands = [...commandsBeforeReorder, ...commandsAfterReorder]
            }

            return {
              commands: [...midInteractionCommands, ...interactionFinishCommands],
              elementsToRerender: 'rerender-all-elements',
              customStatePatch: { duplicatedElementNewUids, elementsToRerender },
              status: 'success',
            }
          }
        }
      }
      return emptyStrategyApplicationResult
    },
  )
}
