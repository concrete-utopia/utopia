import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
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
} from './canvas-strategy-types'
import { boundingArea, InteractionSession } from './interaction-state'
import { ElementInsertionSubject } from '../../editor/editor-modes'
import { LayoutHelpers } from '../../../core/layout/layout-helpers'
import { foldEither } from '../../../core/shared/either'
import {
  InsertElementInsertionSubject,
  insertElementInsertionSubject,
} from '../commands/insert-element-insertion-subject'
import { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import {
  findCanvasStrategy,
  pickCanvasStateFromEditorState,
  pickCanvasStateFromEditorStateWithMetadata,
  RegisteredCanvasStrategies,
} from './canvas-strategies'
import { CanvasCommand, foldAndApplyCommandsInner } from '../commands/commands'
import { updateFunctionCommand } from '../commands/update-function-command'
import {
  createFakeMetadataForElement,
  MetadataUtils,
} from '../../../core/model/element-metadata-utils'
import { elementPath } from '../../../core/shared/element-path'
import * as EP from '../../../core/shared/element-path'
import {
  CanvasPoint,
  canvasRectangle,
  canvasPoint,
  CanvasRectangle,
  Size,
} from '../../../core/shared/math-utils'
import { cmdModifier } from '../../../utils/modifiers'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import { FlexReparentTargetIndicator } from '../controls/select-mode/flex-reparent-target-indicator'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { getReparentTargetUnified, newReparentSubjects } from './reparent-strategy-helpers'
import { showReorderIndicator } from '../commands/show-reorder-indicator-command'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { isImg } from '../../../core/model/project-file-utils'

export const drawToInsertStrategy: CanvasStrategy = {
  id: 'DRAW_TO_INSERT',
  name: () => 'Draw to insert',
  isApplicable: (canvasState, interactionSession, metadata) => {
    const insertionSubjects = getInsertionSubjectsFromInteractionTarget(
      canvasState.interactionTarget,
    )
    const insertionElementSubjects = insertionSubjects.filter((s) => s.type === 'Element')
    return insertionElementSubjects.length === 1
  },
  controlsToRender: [
    // TODO the controlsToRender should instead use the controls of the actual canvas strategy -> to achieve that, this should be a function of the StrategyState here
    controlWithProps({
      control: ParentOutlines,
      props: {},
      key: 'parent-outlines-control',
      show: 'visible-only-while-active',
    }),
    controlWithProps({
      control: ParentBounds,
      props: {},
      key: 'parent-bounds-control',
      show: 'visible-only-while-active',
    }),
    controlWithProps({
      control: DragOutlineControl,
      props: {},
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
  fitness: (canvasState, interactionSession, customStrategyState) => {
    return drawToInsertStrategy.isApplicable(
      canvasState,
      interactionSession,
      canvasState.startingMetadata,
      canvasState.startingAllElementProps,
    ) &&
      ((interactionSession.interactionData.type === 'DRAG' &&
        interactionSession.activeControl.type === 'RESIZE_HANDLE') ||
        interactionSession.interactionData.type === 'HOVER')
      ? 1
      : 0
  },
  apply: (canvasState, interactionSession, customStrategyState, strategyLifecycle) => {
    if (
      canvasState.interactionTarget.type === 'INSERTION_SUBJECTS' &&
      canvasState.interactionTarget.subjects.length === 1 &&
      canvasState.interactionTarget.subjects[0].type === 'Element'
    ) {
      if (interactionSession.interactionData.type === 'DRAG') {
        if (interactionSession.interactionData.drag != null) {
          const insertionSubject = canvasState.interactionTarget.subjects[0]

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
          const insertionSubject = canvasState.interactionTarget.subjects[0]

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
          const pointOnCanvas = interactionSession.interactionData.dragStart
          return strategyApplicationResult(
            getHighlightAndReorderIndicatorCommands(canvasState, pointOnCanvas),
          )
        }
      } else if (interactionSession.interactionData.type === 'HOVER') {
        const pointOnCanvas = interactionSession.interactionData.point
        return strategyApplicationResult(
          getHighlightAndReorderIndicatorCommands(canvasState, pointOnCanvas),
        )
      }
    }
    // Fallback for when the checks above are not satisfied.
    return emptyStrategyApplicationResult
  },
}

function getHighlightAndReorderIndicatorCommands(
  canvasState: InteractionCanvasState,
  pointOnCanvas: CanvasPoint,
): Array<CanvasCommand> {
  const parent = getReparentTargetUnified(
    newReparentSubjects(),
    pointOnCanvas,
    true,
    canvasState,
    canvasState.startingMetadata,
    canvasState.startingAllElementProps,
    'allow-missing-bounds',
  )

  if (parent != null && parent.shouldReparent && parent.newParent != null) {
    const highlightParentCommand = updateHighlightedViews('mid-interaction', [parent.newParent])

    if (parent.newIndex !== -1) {
      return [highlightParentCommand, showReorderIndicator(parent.newParent, parent.newIndex)]
    } else {
      return [highlightParentCommand]
    }
  } else {
    return []
  }
}

function getInsertionCommands(
  subject: ElementInsertionSubject,
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

    const updatedInsertionSubject: ElementInsertionSubject = {
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

    const updatedInsertionSubject: ElementInsertionSubject = {
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
  subject: ElementInsertionSubject,
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
  builtInDependencies: BuiltInDependencies,
  editorState: EditorState,
  customStrategyState: CustomStrategyState,
  interactionSession: InteractionSession,
  insertionSubject: ElementInsertionSubject,
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

  const { strategy } = findCanvasStrategy(
    RegisteredCanvasStrategies,
    patchedCanvasState,
    patchedInteractionSession,
    customStrategyState,
    null,
  )

  if (strategy == null) {
    return []
  }
  const reparentCommands = strategy.strategy.apply(
    patchedCanvasState,
    patchedInteractionSession,
    customStrategyState,
    strategyLifecycle,
  ).commands

  return foldAndApplyCommandsInner(editorState, [], [], reparentCommands, 'end-interaction') // TODO HACK-HACK 'end-interaction' is here so it is not just the reorder indicator which is rendered
    .statePatches
}

function runTargetStrategiesForFreshlyInsertedElementToResize(
  builtInDependencies: BuiltInDependencies,
  editorState: EditorState,
  customStrategyState: CustomStrategyState,
  interactionSession: InteractionSession,
  commandLifecycle: InteractionLifecycle,
  insertionSubject: ElementInsertionSubject,
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
  }

  const patchedCustomStrategyState: CustomStrategyState = {
    ...customStrategyState,
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
    patchedCustomStrategyState,
    null,
  )

  const resizeCommands =
    resizeStrategy != null
      ? resizeStrategy.strategy.apply(
          patchedCanvasState,
          patchedInteractionSession,
          patchedCustomStrategyState,
          strategyLifecycle,
        ).commands
      : []

  return foldAndApplyCommandsInner(editorState, [], [], resizeCommands, commandLifecycle)
    .statePatches
}
