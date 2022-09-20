import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  getInsertionSubjectsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
  targetPaths,
} from './canvas-strategy-types'
import { boundingArea, InteractionSession, StrategyState } from './interaction-state'
import { ElementInsertionSubject } from '../../editor/editor-modes'
import { LayoutHelpers } from '../../../core/layout/layout-helpers'
import { foldEither, isLeft } from '../../../core/shared/either'
import {
  InsertElementInsertionSubject,
  insertElementInsertionSubject,
} from '../commands/insert-element-insertion-subject'
import { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import {
  findCanvasStrategy,
  pickCanvasStateFromEditorState,
  RegisteredCanvasStrategies,
} from './canvas-strategies'
import { foldAndApplyCommandsInner } from '../commands/commands'
import { updateFunctionCommand } from '../commands/update-function-command'
import {
  createFakeMetadataForElement,
  MetadataUtils,
} from '../../../core/model/element-metadata-utils'
import { elementPath } from '../../../core/shared/element-path'
import * as EP from '../../../core/shared/element-path'
import { canvasPoint, CanvasRectangle, canvasRectangle } from '../../../core/shared/math-utils'
import { cmdModifier } from '../../../utils/modifiers'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import { FlexReparentTargetIndicator } from '../controls/select-mode/flex-reparent-target-indicator'
import { ElementPath } from '../../../core/shared/project-file-types'

export const drawToInsertStrategy: CanvasStrategy = {
  id: 'DRAW_TO_INSERT',
  name: () => 'Draw to insert',
  isApplicable: (canvasState, _interactionState, metadata) => {
    const insertionSubjects = getInsertionSubjectsFromInteractionTarget(
      canvasState.interactionTarget,
    )
    const insertionElementSubjects = insertionSubjects.filter((s) => s.type === 'Element')
    return insertionElementSubjects.length === 1
  },
  controlsToRender: [
    // TODO the controlsToRender should instead use the controls of the actual canvas strategy -> to achieve that, this should be a function of the StrategyState here
    {
      control: ParentOutlines,
      key: 'parent-outlines-control',
      show: 'visible-only-while-active',
    },
    {
      control: ParentBounds,
      key: 'parent-bounds-control',
      show: 'visible-only-while-active',
    },
    {
      control: DragOutlineControl,
      key: 'ghost-outline-control',
      show: 'visible-only-while-active',
    },
    {
      control: FlexReparentTargetIndicator,
      key: 'flex-reparent-target-indicator',
      show: 'visible-only-while-active',
    },
  ], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState, strategyState) => {
    return drawToInsertStrategy.isApplicable(
      canvasState,
      interactionState,
      strategyState.startingMetadata,
      strategyState.startingAllElementProps,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'RESIZE_HANDLE'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, strategyState) => {
    if (
      canvasState.interactionTarget.type === 'INSERTION_SUBJECTS' &&
      canvasState.interactionTarget.subjects.length === 1 &&
      canvasState.interactionTarget.subjects[0].type === 'Element' &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.drag != null
    ) {
      const insertionSubject = canvasState.interactionTarget.subjects[0]

      const insertionCommand = getInsertionCommands(insertionSubject, interactionState, null)

      if (insertionCommand != null) {
        const reparentCommand = updateFunctionCommand(
          'always',
          (editorState): Array<EditorStatePatch> => {
            return runTargetStrategiesForFreshlyInsertedElementToReparent(
              canvasState.builtInDependencies,
              editorState,
              strategyState,
              interactionState,
              insertionSubject,
              insertionCommand.frame,
            )
          },
        )

        const resizeCommand = updateFunctionCommand(
          'always',
          (editorState, commandLifecycle): Array<EditorStatePatch> => {
            return runTargetStrategiesForFreshlyInsertedElementToResize(
              canvasState.builtInDependencies,
              editorState,
              strategyState,
              interactionState,
              commandLifecycle,
              insertionSubject,
              insertionCommand.frame,
            )
          },
        )

        return strategyApplicationResult([insertionCommand.command, reparentCommand, resizeCommand])
      }
    }
    // Fallback for when the checks above are not satisfied.
    return emptyStrategyApplicationResult
  },
}

function getInsertionCommands(
  subject: ElementInsertionSubject,
  interactionState: InteractionSession,
  parent: ElementPath | null,
): { command: InsertElementInsertionSubject; frame: CanvasRectangle } | null {
  if (
    interactionState.interactionData.type === 'DRAG' &&
    interactionState.interactionData.drag != null
  ) {
    const pointOnCanvas = interactionState.interactionData.dragStart

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
      parent: parent
        ? { target: parent, staticTarget: EP.dynamicPathToStaticPath(parent) }
        : subject.parent,
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
  strategyState: StrategyState,
  interactionState: InteractionSession,
  insertionSubject: ElementInsertionSubject,
  frame: CanvasRectangle,
): Array<EditorStatePatch> {
  const canvasState = pickCanvasStateFromEditorState(editorState, builtInDependencies)

  const storyboard = MetadataUtils.getStoryboardMetadata(strategyState.startingMetadata)
  const rootPath = storyboard != null ? storyboard.elementPath : elementPath([])

  const element = insertionSubject.element
  const path = EP.appendToPath(rootPath, element.uid)

  const fakeMetadata = createFakeMetadataForElement(
    path,
    element,
    frame,
    strategyState.startingMetadata,
  )

  const patchedMetadata = {
    ...strategyState.startingMetadata,
    [EP.toString(path)]: fakeMetadata,
  }

  const patchedStrategyState = {
    ...strategyState,
    startingMetadata: patchedMetadata,
  }

  const interactionData = interactionState.interactionData
  // patching the interaction with the cmd modifier is just temporarily needed because reparenting is not default without
  const patchedInteractionData =
    interactionData.type === 'DRAG'
      ? {
          ...interactionData,
          drag: canvasPoint({ x: 0, y: 0 }),
          modifiers: cmdModifier,
        }
      : interactionData

  const patchedInteractionState = {
    ...interactionState,
    activeControl: boundingArea(),
    interactionData: patchedInteractionData,
  }

  const patchedCanvasState: InteractionCanvasState = {
    ...canvasState,
    interactionTarget: targetPaths(editorState.selectedViews),
  }

  const { strategy } = findCanvasStrategy(
    RegisteredCanvasStrategies,
    patchedCanvasState,
    patchedInteractionState,
    patchedStrategyState,
    null,
  )

  if (strategy == null) {
    return []
  }
  const reparentCommands = strategy.strategy.apply(
    patchedCanvasState,
    patchedInteractionState,
    patchedStrategyState,
  ).commands

  return foldAndApplyCommandsInner(editorState, [], [], reparentCommands, 'end-interaction') // TODO HACK-HACK 'end-interaction' is here so it is not just the reorder indicator which is rendered
    .statePatches
}

function runTargetStrategiesForFreshlyInsertedElementToResize(
  builtInDependencies: BuiltInDependencies,
  editorState: EditorState,
  strategyState: StrategyState,
  interactionState: InteractionSession,
  commandLifecycle: 'mid-interaction' | 'end-interaction',
  insertionSubject: ElementInsertionSubject,
  frame: CanvasRectangle,
): Array<EditorStatePatch> {
  const canvasState = pickCanvasStateFromEditorState(editorState, builtInDependencies)

  const element = insertionSubject.element
  const path = editorState.selectedViews[0]

  const fakeMetadata = createFakeMetadataForElement(
    path,
    element,
    frame,
    strategyState.startingMetadata,
  )

  const patchedMetadata = {
    ...strategyState.startingMetadata,
    [EP.toString(path)]: fakeMetadata,
  }

  const patchedStrategyState = {
    ...strategyState,
    startingMetadata: patchedMetadata,
  }

  const patchedCanvasState: InteractionCanvasState = {
    ...canvasState,
    interactionTarget: targetPaths(editorState.selectedViews),
  }

  const { strategy: resizeStrategy } = findCanvasStrategy(
    RegisteredCanvasStrategies,
    patchedCanvasState,
    interactionState,
    patchedStrategyState,
    null,
  )

  const resizeCommands =
    resizeStrategy != null
      ? resizeStrategy.strategy.apply(patchedCanvasState, interactionState, patchedStrategyState)
          .commands
      : []

  return foldAndApplyCommandsInner(editorState, [], [], resizeCommands, commandLifecycle)
    .statePatches
}
