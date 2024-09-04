import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type {
  CanvasPoint,
  CanvasRectangle,
  Size,
  WindowRectangle,
} from '../../../../core/shared/math-utils'
import {
  canvasPoint,
  canvasRectangle,
  canvasVector,
  offsetPoint,
  roundRectangleToNearestWhole,
  size,
} from '../../../../core/shared/math-utils'
import { assertNever } from '../../../../core/shared/utils'
import { EditorModes, type InsertionSubject } from '../../../editor/editor-modes'
import { childInsertionPath } from '../../../editor/store/insertion-path'
import type { InsertElementInsertionSubject } from '../../commands/insert-element-insertion-subject'
import { insertElementInsertionSubject } from '../../commands/insert-element-insertion-subject'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import { GridControls } from '../../controls/grid-controls'
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
import { getTargetCell, setGridPropsCommands } from './grid-helpers'
import { newReparentSubjects } from './reparent-helpers/reparent-strategy-helpers'
import { getReparentTargetUnified } from './reparent-helpers/reparent-strategy-parent-lookup'
import { stripNulls } from '../../../../core/shared/array-utils'
import { showGridControls } from '../../commands/show-grid-controls-command'

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
      controlsToRender: [
        {
          control: GridControls,
          props: { targets: [targetParent] },
          key: `draw-into-grid-strategy-controls`,
          show: 'always-visible',
          priority: 'bottom',
        },
      ],
      fitness: 5,
      apply: (strategyLifecycle) => {
        if (strategyLifecycle === 'mid-interaction' && interactionData.type === 'HOVER') {
          return strategyApplicationResult([
            wildcardPatch('mid-interaction', {
              selectedViews: { $set: [] },
            }),
            showGridControls('mid-interaction', targetParent),
            updateHighlightedViews('mid-interaction', [targetParent]),
          ])
        }

        const newTargetCell = getGridCellUnderCursor(
          interactionData,
          canvasState,
          customStrategyState,
        )

        if (newTargetCell == null) {
          return emptyStrategyApplicationResult
        }

        const { gridCellCoordinates, cellWindowRectangle } = newTargetCell

        const offset = getOffsetFromGridCell(interactionData, canvasState, cellWindowRectangle)

        const defaultSize =
          interactionData.type === 'DRAG' &&
          interactionData.drag == null &&
          strategyLifecycle === 'end-interaction'
            ? insertionSubject.defaultSize
            : size(0, 0)

        const insertionCommand = getInsertionCommand(
          targetParent,
          insertionSubject,
          getFrameForInsertion(interactionData, defaultSize, offset),
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
            ...setGridPropsCommands(insertedElementPath, gridTemplate, {
              gridRowStart: { numericalPosition: gridCellCoordinates.row },
              gridColumnStart: { numericalPosition: gridCellCoordinates.column },
              gridRowEnd: { numericalPosition: gridCellCoordinates.row + 1 },
              gridColumnEnd: { numericalPosition: gridCellCoordinates.column + 1 },
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
  offset: CanvasPoint,
): CanvasRectangle {
  if (interactionData.type === 'DRAG') {
    const origin = interactionData.drag ?? { x: defaultSize.width / 2, y: defaultSize.height / 2 }

    const { x, y } = { x: offset.x - origin.x, y: offset.y - origin.y }

    const { width, height } =
      interactionData.drag == null
        ? defaultSize
        : { width: interactionData.drag.x, height: interactionData.drag.y }

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

function getOffsetFromGridCell(
  interactionData: DragInteractionData | HoverInteractionData,
  canvasState: InteractionCanvasState,
  cellWindowRectangle: WindowRectangle,
) {
  const windowPointToUse =
    interactionData.type === 'DRAG'
      ? offsetPoint(interactionData.dragStart, interactionData.drag ?? canvasVector({ x: 0, y: 0 }))
      : interactionData.point

  const mouseWindowPoint = canvasPointToWindowPoint(
    windowPointToUse,
    canvasState.scale,
    canvasState.canvasOffset,
  )

  return canvasPoint({
    x: mouseWindowPoint.x - cellWindowRectangle.x,
    y: mouseWindowPoint.y - cellWindowRectangle.y,
  })
}
