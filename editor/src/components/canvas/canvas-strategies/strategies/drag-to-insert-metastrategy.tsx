import type { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { LayoutHelpers } from '../../../../core/layout/layout-helpers'
import {
  createFakeMetadataForElement,
  getRootPath,
} from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { isLeft } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { getJSXElementNameLastPart } from '../../../../core/shared/element-template'
import type { CanvasRectangle } from '../../../../core/shared/math-utils'
import {
  canvasRectangle,
  offsetPoint,
  roundPointToNearestWhole,
  zeroCanvasPoint,
} from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { CSSCursor } from '../../../../uuiui-deps'
import type { InsertionSubject } from '../../../editor/editor-modes'
import type { EditorState, EditorStatePatch } from '../../../editor/store/editor-state'
import { foldAndApplyCommandsInner } from '../../commands/commands'
import type { InsertElementInsertionSubject } from '../../commands/insert-element-insertion-subject'
import { insertElementInsertionSubject } from '../../commands/insert-element-insertion-subject'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { updateFunctionCommand } from '../../commands/update-function-command'
import { ParentBounds } from '../../controls/parent-bounds'
import { ParentOutlines } from '../../controls/parent-outlines'
import { FlexReparentTargetIndicator } from '../../controls/select-mode/flex-reparent-target-indicator'
import type { CanvasStrategyFactory, MetaCanvasStrategy } from '../canvas-strategies'
import {
  getWrapperWithGeneratedUid,
  pickCanvasStateFromEditorStateWithMetadata,
} from '../canvas-strategies'
import type {
  CanvasStrategy,
  CustomStrategyState,
  InteractionCanvasState,
  InteractionLifecycle,
} from '../canvas-strategy-types'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  getInsertionSubjectsFromInteractionTarget,
  strategyApplicationResult,
  targetPaths,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import type { ParentDisplayType } from './reparent-metastrategy'
import { getApplicableReparentFactories } from './reparent-metastrategy'
import type { ReparentStrategy } from './reparent-helpers/reparent-strategy-helpers'
import { styleStringInArray } from '../../../../utils/common-constants'
import {
  DragOutlineControl,
  dragTargetsFrame,
} from '../../controls/select-mode/drag-outline-control'
import { wrapInContainerCommand } from '../../commands/wrap-in-container-command'
import type { InsertionPath } from '../../../editor/store/insertion-path'
import { childInsertionPath } from '../../../editor/store/insertion-path'

/**
 * NOTE: this strategy was mostly used for legacy insert menu interactions, but it's kept
 * intact since it's still used for image insertion from other places.
 */
export const dragToInsertMetaStrategy: MetaCanvasStrategy = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): Array<CanvasStrategy> => {
  if (
    interactionSession == null ||
    !(
      interactionSession.interactionData.type === 'DRAG' ||
      interactionSession.interactionData.type === 'HOVER'
    )
  ) {
    return []
  }

  const { interactionData } = interactionSession

  const insertionSubjects = getInsertionSubjectsFromInteractionTarget(canvasState.interactionTarget)
  if (insertionSubjects.length === 0) {
    return []
  }

  const pointOnCanvas =
    interactionData.type === 'DRAG'
      ? offsetPoint(interactionData.originalDragStart, interactionData.drag ?? zeroCanvasPoint)
      : interactionData.point

  const applicableReparentFactories = getApplicableReparentFactories(
    canvasState,
    pointOnCanvas,
    true, // cmd is necessary to allow reparenting
    true,
    'allow-smaller-parent',
    customStrategyState,
  )

  return mapDropNulls((result): CanvasStrategy | null => {
    const name = getDragToInsertStrategyName(result.strategyType, result.targetParentDisplayType)

    return dragToInsertStrategyFactory(
      canvasState,
      interactionSession,
      customStrategyState,
      insertionSubjects,
      result.factory,
      name,
      result.fitness,
      result.targetParent,
    )
  }, applicableReparentFactories)
}

function getDragToInsertStrategyName(
  strategyType: ReparentStrategy,
  parentDisplayType: ParentDisplayType,
): string {
  switch (strategyType) {
    case 'REPARENT_INTO_GRID':
      return 'Drag to Insert (Grid)'
    case 'REPARENT_AS_ABSOLUTE':
      return 'Drag to Insert (Abs)'
    case 'REPARENT_AS_STATIC':
      if (parentDisplayType === 'flex') {
        return 'Drag to Insert (Flex)'
      } else {
        return 'Drag to Insert (Flow)'
      }
  }
}

function dragToInsertStrategyFactory(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  customStrategyState: CustomStrategyState,
  insertionSubjects: Array<InsertionSubject>,
  reparentStrategyToUse: CanvasStrategyFactory,
  name: string,
  fitness: number,
  targetParent: InsertionPath,
): CanvasStrategy | null {
  if (canvasState.interactionTarget.type !== 'INSERTION_SUBJECTS') {
    return null
  }
  const insertionSubjectsWithFrames = (() => {
    if (
      interactionSession.interactionData.type !== 'DRAG' ||
      interactionSession.interactionData.drag === null
    ) {
      return []
    }

    const pointOnCanvas = roundPointToNearestWhole(interactionSession.interactionData.dragStart)
    return insertionSubjects.map((s) => ({
      subject: s,
      frame: canvasRectangle({
        x: pointOnCanvas.x - s.defaultSize.width / 2,
        y: pointOnCanvas.y - s.defaultSize.height / 2,
        width: s.defaultSize.width,
        height: s.defaultSize.height,
      }),
    }))
  })()

  // we don't want outline for images for now
  const nonImageInsertionSubjectsWithFrames = insertionSubjectsWithFrames.filter(
    (s) => getJSXElementNameLastPart(s.subject.element.name) !== 'img',
  )

  return {
    id: name,
    name: name,
    descriptiveLabel: 'Dragging To Insert',
    icon: { category: 'tools', type: 'pointer' },
    controlsToRender: [
      controlWithProps({
        control: ParentOutlines,
        props: { targetParent: targetParent.intendedParentPath },
        key: 'parent-outlines-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: ParentBounds,
        props: { targetParent: targetParent.intendedParentPath },
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
        control: DragOutlineControl,
        props: dragTargetsFrame(nonImageInsertionSubjectsWithFrames.map((s) => s.frame)),
        key: 'ghost-outline-control',
        show: 'visible-only-while-active',
      }),
    ],
    fitness:
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'BOUNDING_AREA'
        ? fitness
        : 0,
    apply: (strategyLifecycle) => {
      const rootPath = getRootPath(canvasState.startingMetadata)
      if (
        interactionSession.interactionData.type === 'DRAG' &&
        interactionSession.interactionData.drag != null
      ) {
        if (insertionSubjects.length === 0) {
          return strategyApplicationResult(
            [setCursorCommand(CSSCursor.NotPermitted)],
            {},
            'failure',
          )
        } else {
          const insertionCommandsWithFrames = insertionSubjectsWithFrames.flatMap((s) => {
            if (rootPath == null) {
              throw new Error('Missing root path for drag interaction')
            }
            return getInsertionCommandsWithFrames(rootPath, s.subject, s.frame)
          })

          const maybeWrapperWithUid = getWrapperWithGeneratedUid(
            customStrategyState,
            canvasState,
            insertionSubjects,
          )

          const reparentCommand = updateFunctionCommand(
            'always',
            (editorState, transient): Array<EditorStatePatch> => {
              return runTargetStrategiesForFreshlyInsertedElement(
                reparentStrategyToUse,
                canvasState.builtInDependencies,
                editorState,
                customStrategyState,
                interactionSession,
                transient,
                insertionCommandsWithFrames,
                strategyLifecycle,
              )
            },
          )

          const newPath = EP.appendToPath(targetParent.intendedParentPath, insertionSubjects[0].uid)

          const optionalWrappingCommand =
            maybeWrapperWithUid != null
              ? [
                  updateFunctionCommand(
                    'always',
                    (editorState, lifecycle): Array<EditorStatePatch> =>
                      foldAndApplyCommandsInner(
                        editorState,
                        [],
                        [
                          wrapInContainerCommand(
                            'always',
                            newPath,
                            maybeWrapperWithUid.uid,
                            maybeWrapperWithUid.wrapper,
                          ),
                        ],
                        lifecycle,
                      ).statePatches,
                  ),
                ]
              : []

          return strategyApplicationResult(
            [
              ...insertionCommandsWithFrames.map((c) => c.command),
              reparentCommand,
              ...optionalWrappingCommand,
            ],
            {
              strategyGeneratedUidsCache: {
                [insertionSubjects[0].uid]: maybeWrapperWithUid?.uid,
              },
            },
          )
        }
      }

      return emptyStrategyApplicationResult
    },
  }
}

function getInsertionCommandsWithFrames(
  storyboardPath: ElementPath,
  subject: InsertionSubject,
  frame: CanvasRectangle,
): Array<{ command: InsertElementInsertionSubject; frame: CanvasRectangle }> {
  const updatedAttributesWithPosition = getStyleAttributesForFrameInAbsolutePosition(subject, frame)

  const updatedInsertionSubject = {
    ...subject,
    element: {
      ...subject.element,
      props: updatedAttributesWithPosition,
    },
  }

  const insertionPath = childInsertionPath(storyboardPath)

  return [
    {
      command: insertElementInsertionSubject('always', updatedInsertionSubject, insertionPath),
      frame: frame,
    },
  ]
}

function getStyleAttributesForFrameInAbsolutePosition(
  subject: InsertionSubject,
  frame: CanvasRectangle,
) {
  const updatedAttributes = LayoutHelpers.updateLayoutPropsWithFrame(
    false,
    null,
    subject.element.props,
    {
      left: frame.x,
      top: frame.y,
      width: frame.width,
      height: frame.height,
    },
    styleStringInArray,
  )

  if (isLeft(updatedAttributes)) {
    throw new Error(`Problem setting drag frame on an element we just created.`)
  }

  return updatedAttributes.value
}

function runTargetStrategiesForFreshlyInsertedElement(
  reparentStrategyToUse: CanvasStrategyFactory,
  builtInDependencies: BuiltInDependencies,
  editorState: EditorState,
  customStrategyState: CustomStrategyState,
  interactionSession: InteractionSession,
  commandLifecycle: InteractionLifecycle,
  insertionCommandsWithFrames: Array<{
    command: InsertElementInsertionSubject
    frame: CanvasRectangle
  }>,
  strategyLifeCycle: InteractionLifecycle,
): Array<EditorStatePatch> {
  const rootPath = getRootPath(editorState.jsxMetadata)
  if (rootPath == null) {
    throw new Error('Missing root path when running drag strategy')
  }

  const patchedMetadata: ElementInstanceMetadataMap = insertionCommandsWithFrames.reduce(
    (
      acc: ElementInstanceMetadataMap,
      curr: { command: InsertElementInsertionSubject; frame: CanvasRectangle },
    ): ElementInstanceMetadataMap => {
      const element = curr.command.subject.element
      const path = EP.appendToPath(rootPath, element.uid)

      const fakeMetadata = createFakeMetadataForElement(
        path,
        element,
        curr.frame,
        editorState.jsxMetadata,
      )

      return {
        ...acc,
        [EP.toString(path)]: fakeMetadata,
      }
    },
    editorState.jsxMetadata,
  )

  // IMPORTANT! This canvas state is using an elementPathTree that does not include the newly inserted
  // element as the canvas state's startingElementPathTree. As it happens, this is fine right now,
  // because that element is inserted to the storyboard before reparenting to the correct location,
  // so its index amongst its starting siblings isn't relevant.
  const canvasState = pickCanvasStateFromEditorStateWithMetadata(
    editorState.selectedViews,
    editorState,
    builtInDependencies,
    patchedMetadata,
  )

  const patchedCanvasState: InteractionCanvasState = {
    ...canvasState,
    interactionTarget: targetPaths(
      insertionCommandsWithFrames.map((s) => EP.appendToPath(rootPath, s.command.subject.uid)),
    ),
  }

  const strategy = reparentStrategyToUse(
    patchedCanvasState,
    interactionSession,
    customStrategyState,
  )

  if (strategy == null) {
    return []
  } else {
    const reparentCommands = strategy.apply(strategyLifeCycle).commands

    return foldAndApplyCommandsInner(editorState, [], reparentCommands, commandLifecycle)
      .statePatches
  }
}
