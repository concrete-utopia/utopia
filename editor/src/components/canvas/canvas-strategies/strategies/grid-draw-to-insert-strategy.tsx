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
import { controlsForGridPlaceholders } from '../../controls/grid-controls'
import { canvasPointToWindowPoint, windowToCanvasCoordinates } from '../../dom-lookup'
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
import { getTargetCell, setGridPropsCommands } from './grid-helpers'
import { newReparentSubjects } from './reparent-helpers/reparent-strategy-helpers'
import { getReparentTargetUnified } from './reparent-helpers/reparent-strategy-parent-lookup'

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
      controlsToRender: [controlsForGridPlaceholders(targetParent)],
      fitness: 5,
      apply: (strategyLifecycle) => {
        const newTargetCell = getGridCellUnderCursor(
          interactionData,
          canvasState,
          customStrategyState,
        )

        if (strategyLifecycle === 'mid-interaction' && interactionData.type === 'HOVER') {
          return strategyApplicationResult(
            [
              wildcardPatch('mid-interaction', {
                selectedViews: { $set: [] },
              }),
              showGridControls('mid-interaction', targetParent),
              updateHighlightedViews('mid-interaction', [targetParent]),
            ],
            {
              ...customStrategyState,
              grid: {
                ...customStrategyState.grid,
                // this is added here during the hover interaction so that
                // `GridControls` can render the hover highlight based on the
                // coordinates in `targetCellData`
                targetCellData: newTargetCell ?? customStrategyState.grid.targetCellData,
              },
            },
          )
        }

        if (newTargetCell == null) {
          return emptyStrategyApplicationResult
        }

        const { gridCellCoordinates, cellWindowRectangle } = newTargetCell

        const cellCanvasOrigin = windowToCanvasCoordinates(
          canvasState.scale,
          canvasState.canvasOffset,
          cellWindowRectangle,
        ).canvasPositionRounded

        const defaultSize =
          interactionData.type === 'DRAG' &&
          interactionData.drag == null &&
          strategyLifecycle === 'end-interaction'
            ? insertionSubject.defaultSize
            : size(0, 0)

        const insertionCommand = getInsertionCommand(
          targetParent,
          insertionSubject,
          getFrameForInsertion(interactionData, defaultSize, cellCanvasOrigin),
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

        const isClick = interactionData.type === 'DRAG' && interactionData.drag == null

        // for click-to-insert, don't use absolute positioning
        const clickToInsertCommands = isClick
          ? [
              deleteProperties('always', insertedElementPath, [
                PP.create('style', 'position'),
                PP.create('style', 'top'),
                PP.create('style', 'left'),
                PP.create('style', 'bottom'),
                PP.create('style', 'right'),
              ]),
            ]
          : []

        return strategyApplicationResult(
          [
            insertionCommand,
            ...clickToInsertCommands,
            ...setGridPropsCommands(insertedElementPath, gridTemplate, {
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

function getGridCellUnderCursor(
  interactionData: DragInteractionData | HoverInteractionData,
  canvasState: InteractionCanvasState,
  customStrategyState: CustomStrategyState,
) {
  const windowPointToUse =
    interactionData.type === 'DRAG' ? interactionData.dragStart : interactionData.point

  const mouseWindowPoint = canvasPointToWindowPoint(
    windowPointToUse,
    canvasState.scale,
    canvasState.canvasOffset,
  )

  return getTargetCell(
    customStrategyState.grid.targetCellData?.gridCellCoordinates ?? null,
    false,
    mouseWindowPoint,
  )
}
