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
import { InteractionSession } from './interaction-state'
import { LayoutHelpers } from '../../../core/layout/layout-helpers'
import { isLeft } from '../../../core/shared/either'
import {
  InsertElementInsertionSubject,
  insertElementInsertionSubject,
} from '../commands/insert-element-insertion-subject'
import { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import {
  findCanvasStrategy,
  pickCanvasStateFromEditorStateWithMetadata,
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
import { CanvasRectangle, canvasRectangle, Size } from '../../../core/shared/math-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { cmdModifier } from '../../../utils/modifiers'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import { FlexReparentTargetIndicator } from '../controls/select-mode/flex-reparent-target-indicator'
import { InsertionSubject } from '../../editor/editor-modes'
import { Utils } from '../../../uuiui-deps'

export function dragToInsertStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): CanvasStrategy | null {
  const insertionSubjects = getInsertionSubjectsFromInteractionTarget(canvasState.interactionTarget)
  if (insertionSubjects.length === 0) {
    return null
  }

  return {
    id: 'DRAG_TO_INSERT',
    name: 'Insert',
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
    fitness:
      interactionSession != null &&
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'BOUNDING_AREA'
        ? 1
        : 0,
    apply: (strategyLifecycle) => {
      if (
        interactionSession != null &&
        interactionSession.interactionData.type === 'DRAG' &&
        interactionSession.interactionData.drag != null
      ) {
        const insertionCommands = insertionSubjects.flatMap((s) => {
          const size = s.defaultSize
          return getInsertionCommands(s, interactionSession, size)
        })

        const reparentCommand = updateFunctionCommand(
          'always',
          (editorState, transient): Array<EditorStatePatch> => {
            return runTargetStrategiesForFreshlyInsertedElement(
              canvasState.builtInDependencies,
              editorState,
              customStrategyState,
              interactionSession,
              transient,
              insertionCommands,
              strategyLifecycle,
            )
          },
        )

        return strategyApplicationResult([
          ...insertionCommands.map((c) => c.command),
          reparentCommand,
        ])
      }
      // Fallback for when the checks above are not satisfied.
      return emptyStrategyApplicationResult
    },
  }
}

function getInsertionCommands(
  subject: InsertionSubject,
  interactionSession: InteractionSession,
  size: Size,
): Array<{ command: InsertElementInsertionSubject; frame: CanvasRectangle }> {
  if (
    interactionSession.interactionData.type === 'DRAG' &&
    interactionSession.interactionData.drag != null
  ) {
    const pointOnCanvas = Utils.roundPointToNearestHalf(
      interactionSession.interactionData.dragStart,
    )

    const frame = canvasRectangle({
      x: pointOnCanvas.x - size.width / 2,
      y: pointOnCanvas.y - size.height / 2,
      width: size.width,
      height: size.height,
    })

    const updatedAttributesWithPosition = getStyleAttributesForFrameInAbsolutePosition(
      subject,
      frame,
    )

    const updatedInsertionSubject = {
      ...subject,
      element: {
        ...subject.element,
        props: updatedAttributesWithPosition,
      },
    }

    return [
      {
        command: insertElementInsertionSubject('always', updatedInsertionSubject),
        frame: frame,
      },
    ]
  }
  return []
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
    ['style'],
  )

  if (isLeft(updatedAttributes)) {
    throw new Error(`Problem setting drag frame on an element we just created.`)
  }

  return updatedAttributes.value
}

function runTargetStrategiesForFreshlyInsertedElement(
  builtInDependencies: BuiltInDependencies,
  editorState: EditorState,
  customStrategyState: CustomStrategyState,
  interactionSession: InteractionSession,
  commandLifecycle: InteractionLifecycle,
  insertionSubjects: Array<{ command: InsertElementInsertionSubject; frame: CanvasRectangle }>,
  strategyLifeCycle: InteractionLifecycle,
): Array<EditorStatePatch> {
  const storyboard = MetadataUtils.getStoryboardMetadata(editorState.jsxMetadata)
  const rootPath = storyboard != null ? storyboard.elementPath : elementPath([])

  const patchedMetadata: ElementInstanceMetadataMap = insertionSubjects.reduce(
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

  const canvasState = pickCanvasStateFromEditorStateWithMetadata(
    editorState,
    builtInDependencies,
    patchedMetadata,
  )

  const patchedCanvasState: InteractionCanvasState = {
    ...canvasState,
    interactionTarget: targetPaths(
      insertionSubjects.map((s) => EP.appendToPath(rootPath, s.command.subject.uid)),
    ),
  }

  const interactionData = interactionSession.interactionData
  // patching the interaction with the cmd modifier is just temporarily needed because reparenting is not default without
  const patchedInteractionData =
    interactionData.type === 'DRAG'
      ? { ...interactionData, modifiers: cmdModifier }
      : interactionData

  const patchedInteractionSession: InteractionSession = {
    ...interactionSession,
    interactionData: patchedInteractionData,
    startingTargetParentsToFilterOut: null,
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
  } else {
    const reparentCommands = strategy.strategy.apply(strategyLifeCycle).commands

    return foldAndApplyCommandsInner(editorState, [], [], reparentCommands, commandLifecycle)
      .statePatches
  }
}
