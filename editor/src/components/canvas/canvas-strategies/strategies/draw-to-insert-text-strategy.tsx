import { EditorModes } from '../../../../components/editor/editor-modes'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { updateSelectedRightMenuTab } from '../../../editor/actions/actions'
import { CSSCursor } from '../../canvas-types'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import { canvasPointToWindowPoint } from '../../dom-lookup'
import type { MetaCanvasStrategy } from '../canvas-strategies'
import type {
  CanvasStrategy,
  CustomStrategyState,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import {
  emptyStrategyApplicationResult,
  getInsertionSubjectsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { drawToInsertFitness, drawToInsertStrategyFactory } from './draw-to-insert-metastrategy'
import { getApplicableReparentFactories } from './reparent-metastrategy'

export const DRAW_TO_INSERT_TEXT_STRATEGY_ID = 'draw-to-insert-text'

export const drawToInsertTextStrategy: MetaCanvasStrategy = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): Array<CanvasStrategy> => {
  const name = 'Draw to insert (Text)'

  if (
    interactionSession == null ||
    !(
      interactionSession.interactionData.type === 'DRAG' ||
      interactionSession.interactionData.type === 'HOVER'
    )
  ) {
    return []
  }
  const insertionSubjects = getInsertionSubjectsFromInteractionTarget(canvasState.interactionTarget)
  if (insertionSubjects.length != 1) {
    return []
  }

  const pointOnCanvas =
    interactionSession.interactionData.type === 'DRAG'
      ? interactionSession.interactionData.originalDragStart
      : interactionSession.interactionData.point

  const insertionSubject = insertionSubjects[0]

  return [
    {
      id: DRAW_TO_INSERT_TEXT_STRATEGY_ID,
      name: name,
      descriptiveLabel: 'Drawing To Insert Text',
      icon: { category: 'tools', type: 'pointer' },
      controlsToRender: [],
      fitness: insertionSubject.textEdit && drawToInsertFitness(interactionSession) ? 1 : 0,
      apply: (s) => {
        if (interactionSession.interactionData.type !== 'DRAG') {
          return emptyStrategyApplicationResult
        }
        const applicableReparentFactories = getApplicableReparentFactories(
          canvasState,
          pointOnCanvas,
          true, // cmd is necessary to allow reparenting
          true,
          'allow-smaller-parent',
          customStrategyState,
          ['hasOnlyTextChildren', 'supportsChildren'],
        )
        if (applicableReparentFactories.length < 1) {
          return strategyApplicationResult([])
        }

        const factory = applicableReparentFactories[0]

        const { targetParent } = factory

        const textEditableAndHasText = MetadataUtils.targetTextEditableAndHasText(
          canvasState.startingMetadata,
          canvasState.startingElementPathTree,
          targetParent.intendedParentPath,
        )

        const targetParentPathParts =
          targetParent.intendedParentPath.parts.length > 0
            ? targetParent.intendedParentPath.parts[0].length
            : 0
        const isRoot = targetParentPathParts === 1
        const isClick = s === 'end-interaction' && interactionSession.interactionData.drag == null
        if (!isRoot && textEditableAndHasText && isClick) {
          return strategyApplicationResult([
            updateSelectedViews('on-complete', [targetParent.intendedParentPath]),
            setCursorCommand(CSSCursor.Select),
            wildcardPatch('on-complete', {
              mode: {
                $set: EditorModes.textEditMode(
                  targetParent.intendedParentPath,
                  canvasPointToWindowPoint(
                    pointOnCanvas,
                    canvasState.scale,
                    canvasState.canvasOffset,
                  ),
                  'existing',
                  'no-text-selection',
                ),
              },
            }),
          ])
        }

        const strategy = drawToInsertStrategyFactory(
          canvasState,
          interactionSession,
          customStrategyState,
          factory.factory,
          name,
          factory.fitness,
          targetParent,
          factory.targetIndex,
        )
        if (strategy == null) {
          return strategyApplicationResult([])
        }

        const targetElement = EP.appendToPath(targetParent.intendedParentPath, insertionSubject.uid)

        const result = strategy.apply(s)
        result.commands.push(
          updateSelectedViews('on-complete', [targetElement]),
          wildcardPatch('on-complete', {
            mode: {
              $set: EditorModes.textEditMode(targetElement, null, 'new', 'no-text-selection'),
            },
          }),
        )

        return result
      },
    },
  ]
}
