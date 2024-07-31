import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { CanvasPoint, CanvasRectangle, Size } from '../../../../core/shared/math-utils'
import {
  canvasPoint,
  canvasRectangle,
  canvasVector,
  offsetPoint,
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
import type { CanvasStrategyFactory } from '../canvas-strategies'
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

  const parent = MetadataUtils.findElementByElementPath(canvasState.startingMetadata, targetParent)

  if (targetParent == null || parent == null || !MetadataUtils.isGridLayoutedContainer(parent)) {
    return null
  }

  const name = `Draw into Grid${insertionSubject.textEdit ? ' (text)' : ''}`

  return {
    id: 'grid-draw-to-insert-strategy',
    name: name,
    descriptiveLabel: name,
    icon: {
      category: 'tools',
      type: 'pointer',
    },
    controlsToRender: [
      {
        control: GridControls,
        props: {},
        key: `draw-into-grid-strategy-controls`,
        show: 'always-visible',
      },
    ],
    fitness: 5,
    apply: (strategyLifecycle) => {
      if (strategyLifecycle === 'mid-interaction' && interactionData.type === 'HOVER') {
        return strategyApplicationResult([
          wildcardPatch('mid-interaction', {
            selectedViews: { $set: [] },
          }),
          updateHighlightedViews('mid-interaction', [targetParent]),
        ])
      }

      const windowPointToUse =
        interactionData.type === 'DRAG'
          ? offsetPoint(
              interactionData.dragStart,
              interactionData.drag ?? canvasVector({ x: 0, y: 0 }),
            )
          : interactionData.point

      const mouseWindowPoint = canvasPointToWindowPoint(
        windowPointToUse,
        canvasState.scale,
        canvasState.canvasOffset,
      )

      const newTargetCell = getTargetCell(
        customStrategyState.grid.targetCell,
        canvasState.scale,
        false,
        mouseWindowPoint,
      )

      if (newTargetCell == null) {
        return emptyStrategyApplicationResult
      }

      const { gridCellCoordinates, cellWindowRectangle } = newTargetCell

      const offset: CanvasPoint = canvasPoint({
        x: mouseWindowPoint.x - cellWindowRectangle.x,
        y: mouseWindowPoint.y - cellWindowRectangle.y,
      })

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

      return strategyApplicationResult([
        insertionCommand,
        ...setGridPropsCommands(EP.appendToPath(targetParent, insertionSubject.uid), gridTemplate, {
          gridRowStart: { numericalPosition: gridCellCoordinates.row },
          gridColumnStart: { numericalPosition: gridCellCoordinates.column },
          gridRowEnd: { numericalPosition: gridCellCoordinates.row + 1 },
          gridColumnEnd: { numericalPosition: gridCellCoordinates.column + 1 },
        }),
        ...stripNulls([
          insertionSubject.textEdit
            ? wildcardPatch('on-complete', {
                mode: {
                  $set: EditorModes.textEditMode(
                    EP.appendToPath(targetParent, insertionSubject.uid),
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
      ])
    },
  }
}

function getFrameForInsertion(
  interactionData: DragInteractionData | HoverInteractionData,
  defaultSize: Size,
  offset: CanvasPoint,
): CanvasRectangle {
  if (interactionData.type === 'DRAG') {
    const frame =
      interactionData.drag ?? canvasVector({ x: defaultSize.width, y: defaultSize.height })

    return canvasRectangle({
      x: offset.x - frame.x,
      y: offset.y - frame.y,
      width: frame.x,
      height: frame.y,
    })
  }

  if (interactionData.type === 'HOVER') {
    const pointOnCanvas = interactionData.point

    return canvasRectangle({
      x: pointOnCanvas.x,
      y: pointOnCanvas.y,
      width: defaultSize.width,
      height: defaultSize.height,
    })
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
