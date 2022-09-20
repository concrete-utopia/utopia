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
import { InteractionSession, StrategyState } from './interaction-state'
import { ElementInsertionSubject, InsertionSubject } from '../../editor/editor-modes'
import { LayoutHelpers } from '../../../core/layout/layout-helpers'
import { isLeft, right } from '../../../core/shared/either'
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
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { elementPath } from '../../../core/shared/element-path'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { CanvasRectangle, canvasRectangle, localRectangle } from '../../../core/shared/math-utils'
import {
  elementInstanceMetadata,
  ElementInstanceMetadataMap,
  emptyComments,
  emptySpecialSizeMeasurements,
  getJSXAttribute,
  jsxAttributeValue,
  setJSXAttributesAttribute,
} from '../../../core/shared/element-template'
import { cmdModifier } from '../../../utils/modifiers'
import { setJSXValueInAttributeAtPath } from '../../../core/shared/jsx-attributes'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import { FlexReparentTargetIndicator } from '../controls/select-mode/flex-reparent-target-indicator'

export const dragToInsertStrategy: CanvasStrategy = {
  id: 'DRAG_TO_INSERT',
  name: () => 'Insert',
  isApplicable: (canvasState, _interactionState, metadata) => {
    const insertionSubjects = getInsertionSubjectsFromInteractionTarget(
      canvasState.interactionTarget,
    )
    const insertionElementSubjects = insertionSubjects.filter((s) => s.type === 'Element')
    return insertionElementSubjects.length > 0
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
    return dragToInsertStrategy.isApplicable(
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
          return runTargetStrategiesForFreshlyInsertedElement(
            canvasState.builtInDependencies,
            editorState,
            strategyState,
            interactionState,
            transient,
            insertionCommands,
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

const DefaultWidth = 100
const DefaultHeight = 100

function getInsertionCommands(
  subject: InsertionSubject,
  interactionState: InteractionSession,
): Array<{ command: InsertElementInsertionSubject; frame: CanvasRectangle }> {
  if (subject.type !== 'Element') {
    // non-element subjects are not supported
    return []
  }
  if (
    interactionState.interactionData.type === 'DRAG' &&
    interactionState.interactionData.drag != null
  ) {
    const pointOnCanvas = interactionState.interactionData.dragStart

    const frame = canvasRectangle({
      x: pointOnCanvas.x - DefaultWidth / 2,
      y: pointOnCanvas.y - DefaultHeight / 2,
      width: DefaultWidth,
      height: DefaultHeight,
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
  subject: ElementInsertionSubject,
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
  strategyState: StrategyState,
  interactionState: InteractionSession,
  commandLifecycle: 'mid-interaction' | 'end-interaction',
  insertionSubjects: Array<{ command: InsertElementInsertionSubject; frame: CanvasRectangle }>,
): Array<EditorStatePatch> {
  const canvasState = pickCanvasStateFromEditorState(editorState, builtInDependencies)

  const storyboard = MetadataUtils.getStoryboardMetadata(strategyState.startingMetadata)
  const rootPath = storyboard != null ? storyboard.elementPath : elementPath([])

  const patchedMetadata = insertionSubjects.reduce(
    (
      acc: ElementInstanceMetadataMap,
      curr: { command: InsertElementInsertionSubject; frame: CanvasRectangle },
    ): ElementInstanceMetadataMap => {
      const element = curr.command.subject.element
      const path = EP.appendToPath(rootPath, element.uid)
      const specialSizeMeasurements = { ...emptySpecialSizeMeasurements }
      specialSizeMeasurements.position = 'absolute'
      return {
        ...acc,
        [EP.toString(path)]: elementInstanceMetadata(
          path,
          right(element),
          curr.frame,
          localRectangle(curr.frame),
          false,
          false,
          specialSizeMeasurements,
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
    interactionTarget: targetPaths(
      insertionSubjects.map((s) => EP.appendToPath(rootPath, s.command.subject.uid)),
    ),
  }

  const interactionData = interactionState.interactionData
  // patching the interaction with the cmd modifier is just temporarily needed because reparenting is not default without
  const patchedInteractionData =
    interactionData.type === 'DRAG'
      ? { ...interactionData, modifiers: cmdModifier }
      : interactionData

  const patchedInteractionState = {
    ...interactionState,
    interactionData: patchedInteractionData,
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
  } else {
    const reparentCommands = strategy.strategy.apply(
      patchedCanvasState,
      interactionState,
      patchedStrategyState,
    ).commands

    return foldAndApplyCommandsInner(editorState, [], [], reparentCommands, commandLifecycle)
      .statePatches
  }
}
