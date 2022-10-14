import { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { LayoutHelpers } from '../../../../core/layout/layout-helpers'
import {
  createFakeMetadataForElement,
  MetadataUtils,
} from '../../../../core/model/element-metadata-utils'
import { isImg } from '../../../../core/model/project-file-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { foldEither } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import { elementPath } from '../../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import {
  canvasPoint,
  canvasRectangle,
  CanvasRectangle,
  Size,
} from '../../../../core/shared/math-utils'
import { maybeToArray } from '../../../../core/shared/optional-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { cmdModifier } from '../../../../utils/modifiers'
import { InsertionSubject } from '../../../editor/editor-modes'
import { EditorState, EditorStatePatch } from '../../../editor/store/editor-state'
import { CanvasCommand, foldAndApplyCommandsInner } from '../../commands/commands'
import {
  InsertElementInsertionSubject,
  insertElementInsertionSubject,
} from '../../commands/insert-element-insertion-subject'
import { showReorderIndicator } from '../../commands/show-reorder-indicator-command'
import { updateFunctionCommand } from '../../commands/update-function-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { ParentBoundsForInsertion } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { DragOutlineControl } from '../../controls/select-mode/drag-outline-control'
import { FlexReparentTargetIndicator } from '../../controls/select-mode/flex-reparent-target-indicator'
import {
  CanvasStrategyFactory,
  findCanvasStrategy,
  MetaCanvasStrategy,
  pickCanvasStateFromEditorState,
  pickCanvasStateFromEditorStateWithMetadata,
  RegisteredCanvasStrategies,
} from '../canvas-strategies'
import {
  CanvasStrategy,
  controlWithProps,
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getInsertionSubjectsFromInteractionTarget,
  InteractionCanvasState,
  InteractionLifecycle,
  strategyApplicationResult,
  targetPaths,
} from '../canvas-strategy-types'
import { boundingArea, InteractionSession, MissingBoundsHandling } from '../interaction-state'
import { getApplicableReparentFactories } from './reparent-metastrategy'
import { ReparentStrategy } from './reparent-strategy-helpers'

export const drawToInsertMetaStrategy: MetaCanvasStrategy = (
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
  const pointOnCanvas =
    interactionSession.interactionData.type === 'DRAG'
      ? interactionSession.interactionData.originalDragStart
      : interactionSession.interactionData.point

  const cmdPressed = interactionSession.interactionData.modifiers.cmd

  const applicableReparentFactories = getApplicableReparentFactories(
    canvasState,
    pointOnCanvas,
    cmdPressed,
    true, // <- TODO this is an important assumption, make sure this is still true when inserting into a flex storyboard
  )

  return mapDropNulls((result): CanvasStrategy | null => {
    const name = getDrawToInsertStrategyName(result.strategyType, result.missingBoundsHandling)

    return drawToInsertStrategyFactory(
      canvasState,
      interactionSession,
      customStrategyState,
      result.factory,
      name,
      result.fitness,
      result.targetParent,
      result.targetIndex,
    )
  }, applicableReparentFactories)
}

function getDrawToInsertStrategyName(
  strategyType: ReparentStrategy,
  missingBoundsHandling: MissingBoundsHandling,
) {
  switch (strategyType) {
    case 'REPARENT_TO_ABSOLUTE':
      if (missingBoundsHandling === 'use-strict-bounds') {
        return 'Draw to Insert (Abs)'
      } else {
        return 'Draw to Insert (Abs, Forced)'
      }
    case 'REPARENT_TO_FLEX':
      return 'Draw to Insert (Flex)'
  }
}

function drawToInsertStrategyFactory(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
  reparentStrategyToUse: CanvasStrategyFactory,
  name: string,
  fitness: number,
  targetParent: ElementPath | null,
  targetIndex: number | null,
): CanvasStrategy | null {
  const insertionSubjects = getInsertionSubjectsFromInteractionTarget(canvasState.interactionTarget)
  if (insertionSubjects.length !== 1) {
    return null
  }
  const insertionSubject = insertionSubjects[0]
  const predictedElementPath =
    targetParent == null
      ? null // TODO does the Storyboard Path need to be used here?
      : EP.appendToPath(targetParent, insertionSubject.uid)
  return {
    id: name, // TODO review this before merge
    name: name,
    controlsToRender: [
      // TODO the controlsToRender should instead use the controls of the actual canvas strategy -> to achieve that, this should be a function of the StrategyState here
      controlWithProps({
        control: ImmediateParentOutlines,
        props: { targets: maybeToArray(targetParent) },
        key: 'parent-outlines-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: ParentBoundsForInsertion,
        props: { targetParents: maybeToArray(targetParent) },
        key: 'parent-bounds-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: DragOutlineControl,
        props: { targets: maybeToArray(predictedElementPath) },
        key: 'ghost-outline-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: FlexReparentTargetIndicator,
        props: {},
        key: 'flex-reparent-target-indicator',
        show: 'visible-only-while-active',
      }),
    ], // Uses existing hooks in select-mode-hooks.tsx
    fitness:
      interactionSession != null &&
      ((interactionSession.interactionData.type === 'DRAG' &&
        interactionSession.activeControl.type === 'RESIZE_HANDLE') ||
        interactionSession.interactionData.type === 'HOVER')
        ? fitness
        : 0, // TODO is this still needed here?
    apply: (strategyLifecycle) => {
      if (interactionSession != null) {
        if (interactionSession.interactionData.type === 'DRAG') {
          if (interactionSession.interactionData.drag != null) {
            const insertionCommand = getInsertionCommands(
              insertionSubject,
              interactionSession,
              insertionSubject.defaultSize,
              'zero-size',
            )

            if (insertionCommand != null) {
              const reparentCommand = updateFunctionCommand(
                'always',
                (editorState): Array<EditorStatePatch> => {
                  return runTargetStrategiesForFreshlyInsertedElementToReparent(
                    reparentStrategyToUse,
                    canvasState.builtInDependencies,
                    editorState,
                    customStrategyState,
                    interactionSession,
                    insertionSubject,
                    insertionCommand.frame,
                    strategyLifecycle,
                    canvasState.startingMetadata,
                  )
                },
              )

              const resizeCommand = updateFunctionCommand(
                'always',
                (editorState, commandLifecycle): Array<EditorStatePatch> => {
                  return runTargetStrategiesForFreshlyInsertedElementToResize(
                    canvasState.builtInDependencies,
                    editorState,
                    customStrategyState,
                    interactionSession,
                    commandLifecycle,
                    insertionSubject,
                    insertionCommand.frame,
                    strategyLifecycle,
                  )
                },
              )

              return strategyApplicationResult([
                insertionCommand.command,
                reparentCommand,
                resizeCommand,
              ])
            }
          } else if (strategyLifecycle === 'end-interaction') {
            const insertionCommand = getInsertionCommands(
              insertionSubject,
              interactionSession,
              insertionSubject.defaultSize,
              'default-size',
            )

            if (insertionCommand != null) {
              const reparentCommand = updateFunctionCommand(
                'always',
                (editorState): Array<EditorStatePatch> => {
                  return runTargetStrategiesForFreshlyInsertedElementToReparent(
                    reparentStrategyToUse,
                    canvasState.builtInDependencies,
                    editorState,
                    customStrategyState,
                    interactionSession,
                    insertionSubject,
                    insertionCommand.frame,
                    strategyLifecycle,
                    canvasState.startingMetadata,
                  )
                },
              )

              return strategyApplicationResult([insertionCommand.command, reparentCommand])
            }
          } else {
            // drag is null, the cursor is not moved yet, but the mousedown already happened
            return strategyApplicationResult(
              getHighlightAndReorderIndicatorCommands(targetParent, targetIndex),
            )
          }
        } else if (interactionSession.interactionData.type === 'HOVER') {
          return strategyApplicationResult(
            getHighlightAndReorderIndicatorCommands(targetParent, targetIndex),
          )
        }
      }
      // Fallback for when the checks above are not satisfied.
      return emptyStrategyApplicationResult
    },
  }
}

function getHighlightAndReorderIndicatorCommands(
  targetParent: ElementPath | null,
  targetIndex: number | null,
): Array<CanvasCommand> {
  if (targetParent != null) {
    const highlightParentCommand = updateHighlightedViews('mid-interaction', [targetParent])

    if (targetIndex != null) {
      return [highlightParentCommand, showReorderIndicator(targetParent, targetIndex)]
    } else {
      return [highlightParentCommand]
    }
  } else {
    return []
  }
}

function getInsertionCommands(
  subject: InsertionSubject,
  interactionSession: InteractionSession,
  insertionSubjectSize: Size,
  sizing: 'zero-size' | 'default-size',
): { command: InsertElementInsertionSubject; frame: CanvasRectangle } | null {
  if (
    interactionSession.interactionData.type === 'DRAG' &&
    (sizing === 'default-size' || interactionSession.interactionData.drag != null)
  ) {
    const pointOnCanvas = interactionSession.interactionData.dragStart

    const frame =
      sizing === 'zero-size'
        ? canvasRectangle({
            x: pointOnCanvas.x,
            y: pointOnCanvas.y,
            width: 0,
            height: 0,
          })
        : canvasRectangle({
            x: pointOnCanvas.x - insertionSubjectSize.width / 2,
            y: pointOnCanvas.y - insertionSubjectSize.height / 2,
            width: insertionSubjectSize.width,
            height: insertionSubjectSize.height,
          })

    const updatedAttributesWithPosition = getStyleAttributesForFrameInAbsolutePosition(
      subject,
      frame,
    )

    const updatedInsertionSubject: InsertionSubject = {
      ...subject,
      parent: subject.parent,
      element: {
        ...subject.element,
        props: updatedAttributesWithPosition,
      },
    }

    return {
      command: insertElementInsertionSubject('always', updatedInsertionSubject),
      frame: frame,
    }
  } else if (interactionSession.interactionData.type === 'HOVER') {
    const pointOnCanvas = interactionSession.interactionData.point

    const frame = canvasRectangle({
      x: pointOnCanvas.x,
      y: pointOnCanvas.y,
      width: 0,
      height: 0,
    })

    const updatedAttributesWithPosition = getStyleAttributesForFrameInAbsolutePosition(
      subject,
      frame,
    )

    const updatedInsertionSubject: InsertionSubject = {
      ...subject,
      parent: subject.parent,
      element: {
        ...subject.element,
        props: updatedAttributesWithPosition,
      },
    }

    return {
      command: insertElementInsertionSubject('always', updatedInsertionSubject),
      frame: frame,
    }
  }
  return null
}

function getStyleAttributesForFrameInAbsolutePosition(
  subject: InsertionSubject,
  frame: CanvasRectangle,
) {
  return foldEither(
    (_) => {
      throw new Error(`Problem setting drag frame on an element we just created.`)
    },
    (attr) => attr,
    LayoutHelpers.updateLayoutPropsWithFrame(
      false,
      null,
      subject.element.props,
      {
        left: frame.x,
        top: frame.y,
        width: frame.width,
        height: frame.height,
      },
      ['style'],
    ),
  )
}

function runTargetStrategiesForFreshlyInsertedElementToReparent(
  reparentStrategyToUse: CanvasStrategyFactory,
  builtInDependencies: BuiltInDependencies,
  editorState: EditorState,
  customStrategyState: CustomStrategyState,
  interactionSession: InteractionSession,
  insertionSubject: InsertionSubject,
  frame: CanvasRectangle,
  strategyLifecycle: InteractionLifecycle,
  startingMetadata: ElementInstanceMetadataMap,
): Array<EditorStatePatch> {
  const canvasState = pickCanvasStateFromEditorState(editorState, builtInDependencies)

  const storyboard = MetadataUtils.getStoryboardMetadata(startingMetadata)
  const rootPath = storyboard != null ? storyboard.elementPath : elementPath([])

  const element = insertionSubject.element
  const path = EP.appendToPath(rootPath, element.uid)

  const fakeMetadata = createFakeMetadataForElement(path, element, frame, startingMetadata)

  const patchedMetadata: ElementInstanceMetadataMap = {
    ...startingMetadata,
    [EP.toString(path)]: fakeMetadata,
  }

  const interactionData = interactionSession.interactionData
  // patching the interaction with the cmd modifier is just temporarily needed because reparenting is not default without
  const patchedInteractionData =
    interactionData.type === 'DRAG'
      ? {
          ...interactionData,
          drag: canvasPoint({ x: 0, y: 0 }),
          modifiers: cmdModifier,
        }
      : interactionData

  const patchedInteractionSession: InteractionSession = {
    ...interactionSession,
    activeControl: boundingArea(),
    interactionData: patchedInteractionData,
    startingTargetParentsToFilterOut: null,
  }

  const patchedCanvasState: InteractionCanvasState = {
    ...canvasState,
    interactionTarget: targetPaths(editorState.selectedViews),
    startingMetadata: patchedMetadata,
  }

  const strategy = reparentStrategyToUse(
    patchedCanvasState,
    patchedInteractionSession,
    customStrategyState,
  )

  if (strategy == null) {
    return []
  }
  const reparentCommands = strategy.apply(strategyLifecycle).commands

  return foldAndApplyCommandsInner(editorState, [], reparentCommands, 'end-interaction') // TODO HACK-HACK 'end-interaction' is here so it is not just the reorder indicator which is rendered
    .statePatches
}

function runTargetStrategiesForFreshlyInsertedElementToResize(
  builtInDependencies: BuiltInDependencies,
  editorState: EditorState,
  customStrategyState: CustomStrategyState,
  interactionSession: InteractionSession,
  commandLifecycle: InteractionLifecycle,
  insertionSubject: InsertionSubject,
  frame: CanvasRectangle,
  strategyLifecycle: InteractionLifecycle,
): Array<EditorStatePatch> {
  const element = insertionSubject.element
  const path = editorState.selectedViews[0]

  const fakeMetadata = createFakeMetadataForElement(path, element, frame, editorState.jsxMetadata)
  const patchedMetadata: ElementInstanceMetadataMap = {
    ...editorState.jsxMetadata,
    [EP.toString(path)]: fakeMetadata,
  }

  const patchedInteractionSession: InteractionSession = {
    ...interactionSession,
    startingTargetParentsToFilterOut: null,
    aspectRatioLock: isImg(insertionSubject.element.name)
      ? insertionSubject.defaultSize.width / insertionSubject.defaultSize.height
      : null,
  }

  const canvasState = pickCanvasStateFromEditorStateWithMetadata(
    editorState,
    builtInDependencies,
    patchedMetadata,
  )

  const patchedCanvasState: InteractionCanvasState = {
    ...canvasState,
    interactionTarget: targetPaths(editorState.selectedViews),
  }

  const { strategy: resizeStrategy } = findCanvasStrategy(
    RegisteredCanvasStrategies,
    patchedCanvasState,
    patchedInteractionSession,
    customStrategyState,
    null,
  )

  const resizeCommands =
    resizeStrategy != null ? resizeStrategy.strategy.apply(strategyLifecycle).commands : []

  return foldAndApplyCommandsInner(editorState, [], resizeCommands, commandLifecycle).statePatches
}
