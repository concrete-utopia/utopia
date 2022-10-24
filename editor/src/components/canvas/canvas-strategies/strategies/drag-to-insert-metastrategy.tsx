import { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { LayoutHelpers } from '../../../../core/layout/layout-helpers'
import {
  createFakeMetadataForElement,
  MetadataUtils,
} from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { isLeft } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import { elementPath } from '../../../../core/shared/element-path'
import {
  ElementInstanceMetadataMap,
  getJSXElementNameLastPart,
} from '../../../../core/shared/element-template'
import {
  CanvasRectangle,
  canvasRectangle,
  offsetPoint,
  zeroCanvasPoint,
} from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { Utils } from '../../../../uuiui-deps'
import { InsertionSubject } from '../../../editor/editor-modes'
import { EditorState, EditorStatePatch } from '../../../editor/store/editor-state'
import { foldAndApplyCommandsInner } from '../../commands/commands'
import {
  InsertElementInsertionSubject,
  insertElementInsertionSubject,
} from '../../commands/insert-element-insertion-subject'
import { updateFunctionCommand } from '../../commands/update-function-command'
import { ParentBounds } from '../../controls/parent-bounds'
import { ParentOutlines } from '../../controls/parent-outlines'
import {
  DragOutlineControl,
  dragTargetsFrame,
} from '../../controls/select-mode/drag-outline-control'
import { FlexReparentTargetIndicator } from '../../controls/select-mode/flex-reparent-target-indicator'
import {
  CanvasStrategyFactory,
  MetaCanvasStrategy,
  pickCanvasStateFromEditorStateWithMetadata,
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
import { InteractionSession, MissingBoundsHandling } from '../interaction-state'
import { getApplicableReparentFactories } from './reparent-metastrategy'
import { ReparentStrategy } from './reparent-strategy-helpers'

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

  const cmdPressed = interactionData.modifiers.cmd

  const applicableReparentFactories = getApplicableReparentFactories(
    canvasState,
    pointOnCanvas,
    cmdPressed,
    true,
  )

  return mapDropNulls((result): CanvasStrategy | null => {
    const name = getDragToInsertStrategyName(result.strategyType, result.missingBoundsHandling)

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
  missingBoundsHandling: MissingBoundsHandling,
): string {
  switch (strategyType) {
    case 'REPARENT_AS_ABSOLUTE':
      if (missingBoundsHandling === 'use-strict-bounds') {
        return 'Drag to Insert (Abs)'
      } else {
        return 'Drag to Insert (Abs, Forced)'
      }
    case 'REPARENT_AS_STATIC':
      return 'Drag to Insert (Flex)'
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
  targetParent: ElementPath,
): CanvasStrategy | null {
  const insertionSubjectsWithFrames = (() => {
    if (
      interactionSession.interactionData.type !== 'DRAG' ||
      interactionSession.interactionData.drag === null
    ) {
      return []
    }

    const pointOnCanvas = Utils.roundPointToNearestHalf(
      interactionSession.interactionData.dragStart,
    )
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
    controlsToRender: [
      controlWithProps({
        control: ParentOutlines,
        props: { targetParent: targetParent },
        key: 'parent-outlines-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: ParentBounds,
        props: { targetParent: targetParent },
        key: 'parent-bounds-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: DragOutlineControl,
        props: dragTargetsFrame(nonImageInsertionSubjectsWithFrames.map((s) => s.frame)),
        key: 'ghost-outline-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: FlexReparentTargetIndicator,
        props: {},
        key: 'flex-reparent-target-indicator',
        show: 'visible-only-while-active',
      }),
    ],
    fitness:
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'BOUNDING_AREA'
        ? fitness
        : 0,
    apply: (strategyLifecycle) => {
      if (
        interactionSession.interactionData.type === 'DRAG' &&
        interactionSession.interactionData.drag != null
      ) {
        const insertionCommandsWithFrames = insertionSubjectsWithFrames.flatMap((s) => {
          return getInsertionCommandsWithFrames(s.subject, s.frame)
        })

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

        return strategyApplicationResult([
          ...insertionCommandsWithFrames.map((c) => c.command),
          reparentCommand,
        ])
      }

      return emptyStrategyApplicationResult
    },
  }
}

function getInsertionCommandsWithFrames(
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

  return [
    {
      command: insertElementInsertionSubject('always', updatedInsertionSubject),
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
    ['style'],
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
  const storyboard = MetadataUtils.getStoryboardMetadata(editorState.jsxMetadata)
  const rootPath = storyboard != null ? storyboard.elementPath : elementPath([])

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

  const canvasState = pickCanvasStateFromEditorStateWithMetadata(
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

  const patchedInteractionSession: InteractionSession = {
    ...interactionSession,
    startingTargetParentsToFilterOut: null,
  }

  const strategy = reparentStrategyToUse(
    patchedCanvasState,
    patchedInteractionSession,
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
