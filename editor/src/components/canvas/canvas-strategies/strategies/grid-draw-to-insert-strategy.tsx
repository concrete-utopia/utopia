import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { stripNulls } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import type { CanvasPoint, CanvasRectangle, Size } from '../../../../core/shared/math-utils'
import {
  canvasPoint,
  canvasRectangle,
  pointDifference,
  roundRectangleToNearestWhole,
  size,
} from '../../../../core/shared/math-utils'
import * as PP from '../../../../core/shared/property-path'
import { assertNever } from '../../../../core/shared/utils'
import { EditorModes, type InsertionSubject } from '../../../editor/editor-modes'
import { childInsertionPath } from '../../../editor/store/insertion-path'
import { deleteProperties } from '../../commands/delete-properties-command'
import type { InsertElementInsertionSubject } from '../../commands/insert-element-insertion-subject'
import { insertElementInsertionSubject } from '../../commands/insert-element-insertion-subject'
import { showGridControls } from '../../commands/show-grid-controls-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import { controlsForGridPlaceholders } from '../../controls/grid-controls-for-strategies'
import { canvasPointToWindowPoint } from '../../dom-lookup'
import {
  getWrapperWithGeneratedUid,
  getWrappingCommands,
  type CanvasStrategyFactory,
} from '../canvas-strategies'
import {
  emptyStrategyApplicationResult,
  getInsertionSubjectsFromInteractionTarget,
  strategyApplicationResult,
  type CustomStrategyState,
  type InteractionCanvasState,
} from '../canvas-strategy-types'
import type {
  DragInteractionData,
  HoverInteractionData,
  InteractionSession,
} from '../interaction-state'
import {
  getStyleAttributesForFrameInAbsolutePosition,
  updateInsertionSubjectWithAttributes,
} from './draw-to-insert-metastrategy'
import { getCommandsForGridItemPlacement } from './grid-helpers'
import { newReparentSubjects } from './reparent-helpers/reparent-strategy-helpers'
import { getReparentTargetUnified } from './reparent-helpers/reparent-strategy-parent-lookup'
import { getGridCellUnderMouseFromMetadata } from './grid-cell-bounds'
import { nukeAllAbsolutePositioningPropsCommands } from '../../../inspector/inspector-common'
import { gridContainerIdentifier } from '../../../editor/store/editor-state'

export const gridDrawToInsertText: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
) => {
  const insertionSubject = getInsertionSubjectsFromInteractionTarget(
    canvasState.interactionTarget,
  ).at(0)

  if (insertionSubject == null) {
    return null
  }
  if (insertionSubject.textEdit) {
    return gridDrawToInsertStrategyInner({
      name: 'Draw to insert (Text)',
      id: 'draw-text-into-grid',
      insertionSubject: insertionSubject,
    })(canvasState, interactionSession, customStrategyState)
  }

  return null
}

export const gridDrawToInsertStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
) => {
  const insertionSubject = getInsertionSubjectsFromInteractionTarget(
    canvasState.interactionTarget,
  ).at(0)

  if (insertionSubject == null) {
    return null
  }
  if (insertionSubject.textEdit) {
    return null
  }

  return gridDrawToInsertStrategyInner({
    name: 'Draw to Insert (Grid)',
    id: 'draw-into-grid',
    insertionSubject: insertionSubject,
  })(canvasState, interactionSession, customStrategyState)
}

const gridDrawToInsertStrategyInner =
  ({
    name,
    id,
    insertionSubject,
  }: {
    name: string
    id: string
    insertionSubject: InsertionSubject
  }): CanvasStrategyFactory =>
  (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
    customStrategyState: CustomStrategyState,
  ) => {
    if (interactionSession == null || interactionSession.interactionData.type === 'KEYBOARD') {
      return null
    }

    const { interactionData } = interactionSession

    const pointOnCanvas =
      interactionData.type === 'DRAG' ? interactionData.originalDragStart : interactionData.point

    const targetParent = getReparentTargetUnified(
      newReparentSubjects(insertionSubject.defaultSize),
      pointOnCanvas,
      true, // cmd is necessary to allow reparenting,
      canvasState,
      canvasState.startingMetadata,
      canvasState.startingElementPathTree,
      canvasState.startingAllElementProps,
      'allow-smaller-parent',
      ['supportsChildren'],
      canvasState.propertyControlsInfo,
    )?.newParent.intendedParentPath

    const parent = MetadataUtils.findElementByElementPath(
      canvasState.startingMetadata,
      targetParent,
    )

    if (targetParent == null || parent == null || !MetadataUtils.isGridLayoutedContainer(parent)) {
      return null
    }

    return {
      id: id,
      name: name,
      descriptiveLabel: name,
      icon: {
        category: 'tools',
        type: 'pointer',
      },
      controlsToRender: [controlsForGridPlaceholders(gridContainerIdentifier(targetParent))],
      fitness: 5,
      apply: (strategyLifecycle) => {
        const canvasPointToUse =
          interactionData.type === 'DRAG' ? interactionData.dragStart : interactionData.point

        const newTargetCell = getGridCellUnderMouseFromMetadata(parent, canvasPointToUse)

        if (strategyLifecycle === 'mid-interaction' && interactionData.type === 'HOVER') {
          return strategyApplicationResult(
            [
              wildcardPatch('mid-interaction', {
                selectedViews: { $set: [] },
              }),
              showGridControls(
                'mid-interaction',
                gridContainerIdentifier(targetParent),
                newTargetCell?.gridCellCoordinates ?? null,
                null,
              ),
              updateHighlightedViews('mid-interaction', [targetParent]),
            ],
            [],
          )
        }

        if (newTargetCell == null) {
          return emptyStrategyApplicationResult
        }

        const { gridCellCoordinates, cellCanvasRectangle } = newTargetCell

        const defaultSize =
          interactionData.type === 'DRAG' &&
          interactionData.drag == null &&
          strategyLifecycle === 'end-interaction'
            ? insertionSubject.defaultSize
            : size(0, 0)

        const insertionCommand = getInsertionCommand(
          targetParent,
          insertionSubject,
          getFrameForInsertion(interactionData, defaultSize, cellCanvasRectangle),
        )

        const gridTemplate = parent.specialSizeMeasurements.containerGridProperties

        const insertedElementPath = EP.appendToPath(targetParent, insertionSubject.uid)

        const maybeWrapperWithUid = getWrapperWithGeneratedUid(customStrategyState, canvasState, [
          insertionSubject,
        ])

        const wrappingCommands =
          maybeWrapperWithUid == null
            ? []
            : getWrappingCommands(insertedElementPath, maybeWrapperWithUid)

        return strategyApplicationResult(
          [
            insertionCommand,
            ...nukeAllAbsolutePositioningPropsCommands(insertedElementPath), // do not use absolute positioning in grid cells
            ...getCommandsForGridItemPlacement(insertedElementPath, gridTemplate, {
              gridRowStart: { numericalPosition: gridCellCoordinates.row },
              gridColumnStart: { numericalPosition: gridCellCoordinates.column },
              gridRowEnd: { numericalPosition: gridCellCoordinates.row + 1 },
              gridColumnEnd: { numericalPosition: gridCellCoordinates.column + 1 },
              // TODO! this is currently going to assign the element to the cell the interaction started in,
              // however it would be good to instead assign the element to _all_ cells overlapping with the final
              // inserted frame.
            }),
            ...wrappingCommands,
            ...stripNulls([
              insertionSubject.textEdit
                ? wildcardPatch('on-complete', {
                    mode: {
                      $set: EditorModes.textEditMode(
                        insertedElementPath,
                        canvasPointToWindowPoint(
                          pointOnCanvas,
                          canvasState.scale,
                          canvasState.canvasOffset,
                        ),
                        'existing',
                        'no-text-selection',
                      ),
                    },
                  })
                : null,
            ]),
          ],
          [targetParent],
          {
            strategyGeneratedUidsCache: {
              [insertionSubject.uid]: maybeWrapperWithUid?.uid,
            },
          },
        )
      },
    }
  }

function getFrameForInsertion(
  interactionData: DragInteractionData | HoverInteractionData,
  defaultSize: Size,
  cellOrigin: CanvasPoint,
): CanvasRectangle {
  if (interactionData.type === 'DRAG') {
    const dragStart = interactionData.dragStart
    const mouseAt = {
      x: interactionData.dragStart.x + (interactionData.drag?.x ?? 0),
      y: interactionData.dragStart.y + (interactionData.drag?.y ?? 0),
    }
    const width = Math.abs(interactionData.drag?.x ?? defaultSize.width)
    const height = Math.abs(interactionData.drag?.y ?? defaultSize.height)

    const origin = canvasPoint({
      x: Math.min(dragStart.x, mouseAt.x),
      y: Math.min(dragStart.y, mouseAt.y),
    })
    const { x, y } = pointDifference(cellOrigin, origin)

    return roundRectangleToNearestWhole(canvasRectangle({ x, y, width, height }))
  }

  if (interactionData.type === 'HOVER') {
    const pointOnCanvas = interactionData.point

    return roundRectangleToNearestWhole(
      canvasRectangle({
        x: pointOnCanvas.x,
        y: pointOnCanvas.y,
        width: defaultSize.width,
        height: defaultSize.height,
      }),
    )
  }

  assertNever(interactionData)
}

function getInsertionCommand(
  parentPath: ElementPath,
  subject: InsertionSubject,
  frame: CanvasRectangle,
): InsertElementInsertionSubject {
  const updatedAttributesWithPosition = getStyleAttributesForFrameInAbsolutePosition(subject, frame)

  const updatedInsertionSubject = updateInsertionSubjectWithAttributes(
    subject,
    updatedAttributesWithPosition,
  )

  const insertionPath = childInsertionPath(parentPath)

  return insertElementInsertionSubject('always', updatedInsertionSubject, insertionPath)
}
