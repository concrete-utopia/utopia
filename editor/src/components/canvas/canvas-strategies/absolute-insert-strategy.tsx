import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  getInsertionSubjectsFromInteractionTarget,
  InteractionCanvasState,
  selectedElements,
} from './canvas-strategy-types'
import { InteractionSession, StrategyState } from './interaction-state'
import { ElementInsertionSubject, InsertionSubject } from '../../editor/editor-modes'
import { LayoutHelpers } from '../../../core/layout/layout-helpers'
import { isLeft, right } from '../../../core/shared/either'
import { InsertElement, insertElement } from '../commands/insert-element-command'
import { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { pickCanvasStateFromEditorState } from './canvas-strategies'
import { foldAndApplyCommandsInner } from '../commands/commands'
import { absoluteReparentStrategy } from './absolute-reparent-strategy'
import { updateFunctionCommand } from '../commands/update-function-command'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { elementPath } from '../../../core/shared/element-path'
import * as EP from '../../../core/shared/element-path'
import { CanvasRectangle, canvasRectangle, localRectangle } from '../../../core/shared/math-utils'
import {
  elementInstanceMetadata,
  ElementInstanceMetadataMap,
  emptySpecialSizeMeasurements,
} from '../../../core/shared/element-template'

export const absoluteInsertStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_INSERT',
  name: 'Absolute Insert (Delta-based)',
  isApplicable: (canvasState, _interactionState, metadata) => {
    const insertionSubjects = getInsertionSubjectsFromInteractionTarget(
      canvasState.interactionTarget,
    )
    const insertionElementSubjects = insertionSubjects.filter((s) => s.type === 'Element')
    return insertionElementSubjects.length > 0
  },
  controlsToRender: [
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
  ], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState, strategyState) => {
    return absoluteInsertStrategy.isApplicable(
      canvasState,
      interactionState,
      strategyState.startingMetadata,
      strategyState.startingAllElementProps,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, strategyState) => {
    const insertionSubjects = getInsertionSubjectsFromInteractionTarget(
      canvasState.interactionTarget,
    )
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.drag != null
    ) {
      const insertionCommands = insertionSubjects.flatMap((s) =>
        getInsertionCommands(s, interactionState),
      )

      const reparentCommand = updateFunctionCommand(
        'always',
        (editorState, transient): Array<EditorStatePatch> => {
          return runAbsoluteReparentStrategyForFreshlyConvertedElement(
            canvasState.builtInDependencies,
            editorState,
            strategyState,
            interactionState,
            transient,
            insertionCommands,
          )
        },
      )

      return {
        commands: [...insertionCommands.map((c) => c.command), reparentCommand],
        customState: null,
      }
    }
    // Fallback for when the checks above are not satisfied.
    return emptyStrategyApplicationResult
  },
}

function getInsertionCommands(
  subject: InsertionSubject,
  interactionState: InteractionSession,
): Array<{ command: InsertElement; frame: CanvasRectangle }> {
  if (subject.type !== 'Element') {
    // non-element subjects are not supported
    return []
  }
  if (
    interactionState.interactionData.type === 'DRAG' &&
    interactionState.interactionData.drag != null
  ) {
    const pointOnCanvas = interactionState.interactionData.dragStart
    const rect = canvasRectangle({
      x: pointOnCanvas.x - 50,
      y: pointOnCanvas.y - 50,
      width: 100,
      height: 100,
    })
    const updatedAttributes = LayoutHelpers.updateLayoutPropsWithFrame(
      false,
      null,
      subject.element.props,
      {
        left: rect.x,
        top: rect.y,
        width: rect.width,
        height: rect.height,
      },
      ['style'],
    )

    if (isLeft(updatedAttributes)) {
      throw new Error(`Problem setting drag frame on an element we just created.`)
    }

    const updatedInsertionSubject = {
      ...subject,
      element: {
        ...subject.element,
        props: updatedAttributes.value,
      },
    }

    return [
      {
        command: insertElement('always', updatedInsertionSubject),
        frame: rect,
      },
    ]
  }
  return []
}

function runAbsoluteReparentStrategyForFreshlyConvertedElement(
  builtInDependencies: BuiltInDependencies,
  editorState: EditorState,
  strategyState: StrategyState,
  interactionState: InteractionSession,
  commandLifecycle: 'mid-interaction' | 'end-interaction',
  insertionSubjects: Array<{ command: InsertElement; frame: CanvasRectangle }>,
): Array<EditorStatePatch> {
  const canvasState = pickCanvasStateFromEditorState(editorState, builtInDependencies)

  const storyboard = MetadataUtils.getStoryboardMetadata(strategyState.startingMetadata)
  const rootPath = storyboard != null ? storyboard.elementPath : elementPath([])

  const patchedMetadata = insertionSubjects.reduce(
    (
      acc: ElementInstanceMetadataMap,
      curr: { command: InsertElement; frame: CanvasRectangle },
    ): ElementInstanceMetadataMap => {
      const element = curr.command.subject.element
      const path = EP.appendToPath(rootPath, element.uid)
      return {
        ...acc,
        [EP.toString(path)]: elementInstanceMetadata(
          path,
          right(element),
          curr.frame,
          localRectangle(curr.frame),
          false,
          false,
          emptySpecialSizeMeasurements,
          null,
          null,
          null,
          null,
        ),
      }
    },
    strategyState.startingMetadata,
  )

  const patchedStrategyState = {
    ...strategyState,
    startingMetadata: patchedMetadata,
  }

  const patchedCanvasState: InteractionCanvasState = {
    ...canvasState,
    interactionTarget: selectedElements(
      insertionSubjects.map((s) => EP.appendToPath(rootPath, s.command.subject.uid)),
    ),
  }

  const reparentCommands = absoluteReparentStrategy.apply(
    patchedCanvasState,
    interactionState,
    patchedStrategyState,
  ).commands

  return foldAndApplyCommandsInner(editorState, [], [], reparentCommands, commandLifecycle)
    .statePatches
}
