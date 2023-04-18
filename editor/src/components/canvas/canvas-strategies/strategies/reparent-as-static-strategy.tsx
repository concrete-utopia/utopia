import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { zeroCanvasRect } from '../../../../core/shared/math-utils'
import { assertNever } from '../../../../core/shared/utils'
import { absolute } from '../../../../utils/utils'
import { arrayInsertionPath } from '../../../editor/store/reparent-target'
import { CSSCursor } from '../../canvas-types'
import { CanvasCommand } from '../../commands/commands'
import { reorderElement } from '../../commands/reorder-element-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { showReorderIndicator } from '../../commands/show-reorder-indicator-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import { ParentBounds } from '../../controls/parent-bounds'
import { ParentOutlines } from '../../controls/parent-outlines'
import { FlexReparentTargetIndicator } from '../../controls/select-mode/flex-reparent-target-indicator'
import { StaticReparentTargetOutlineIndicator } from '../../controls/select-mode/static-reparent-target-outline'
import { ZeroSizedElementControls } from '../../controls/zero-sized-element-controls'
import { CanvasStrategyFactory } from '../canvas-strategies'
import {
  CanvasStrategy,
  controlWithProps,
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  StrategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { ifAllowedToReparent } from './reparent-helpers/reparent-helpers'
import { getStaticReparentPropertyChanges } from './reparent-helpers/reparent-property-changes'
import { ReparentTarget } from './reparent-helpers/reparent-strategy-helpers'
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
      ...getIdAndNameOfReparentToStaticStrategy(targetLayout),
      controlsToRender: [
        controlWithProps({
          control: ParentOutlines,
          props: { targetParent: reparentTarget.newParent },
          key: 'parent-outlines-control',
          show: 'visible-only-while-active',
        }),
        controlWithProps({
          control: ParentBounds,
          props: { targetParent: reparentTarget.newParent },
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
      ],
      fitness: fitness,
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

function getIdAndNameOfReparentToStaticStrategy(targetLayout: 'flex' | 'flow'): {
  id: string
  name: string
} {
  switch (targetLayout) {
    case 'flex':
      return {
        id: 'REPARENT_TO_FLEX',
        name: 'Reparent (Flex)',
      }
    case 'flow':
      return {
        id: 'REPARENT_TO_FLOW',
        name: 'Reparent (Flow)',
      }
    default:
      assertNever(targetLayout)
  }
}

function applyStaticReparent(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  customStrategyState: CustomStrategyState,
  reparentResult: ReparentTarget,
): StrategyApplicationResult {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  const filteredSelectedElements = flattenSelection(selectedElements)

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

          const siblingsOfTarget = MetadataUtils.getChildrenPathsUnordered(
            canvasState.startingMetadata,
            newParent,
          )

          // Reparent the element.
          const outcomeResult = getReparentOutcome(
            canvasState.builtInDependencies,
            canvasState.projectContents,
            canvasState.nodeModules,
            canvasState.openFile,
            pathToReparent(target),
            arrayInsertionPath(newParent, 'children', null),
            'always',
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

            const commandsAfterReorder = [
              ...propertyChangeCommands,
              setElementsToRerenderCommand([target, newPath]), // TODO THIS LIST IS INCOMPLETE, KEEPS GHOSTS IN THE NAVIGATOR
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
                newParent,
              )

              const commonPatches: Array<CanvasCommand> = [
                wildcardPatch('mid-interaction', {
                  canvas: { controls: { parentHighlightPaths: { $set: [newParent] } } },
                }),
                wildcardPatch('mid-interaction', {
                  displayNoneInstances: { $push: [newPath] },
                }),
                ...placeholderResult.commands,
              ]
              duplicatedElementNewUids = placeholderResult.duplicatedElementNewUids

              if (shouldShowPositionIndicator) {
                return [...commonPatches, showReorderIndicator(newParent, newIndex)]
              } else {
                return [
                  ...commonPatches,
                  wildcardPatch('mid-interaction', {
                    canvas: { controls: { parentOutlineHighlight: { $set: newParent } } },
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
              customStatePatch: { duplicatedElementNewUids: duplicatedElementNewUids },
              status: 'success',
            }
          }
        }
      }
      return emptyStrategyApplicationResult
    },
  )
}
