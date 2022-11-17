import { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { LayoutHelpers } from '../../../../core/layout/layout-helpers'
import {
  createFakeMetadataForElement,
  MetadataUtils,
} from '../../../../core/model/element-metadata-utils'
import { isImg } from '../../../../core/model/project-file-utils'
import { getStoryboardElementPath } from '../../../../core/model/scene-utils'
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
import { ElementPath } from '../../../../core/shared/project-file-types'
import { cmdModifier, emptyModifiers } from '../../../../utils/modifiers'
import { insertionParent, InsertionSubject } from '../../../editor/editor-modes'
import { TOGGLE_BACKGROUND_SHORTCUT } from '../../../editor/shortcut-definitions'
import { EditorState, EditorStatePatch } from '../../../editor/store/editor-state'
import { CanvasCommand, foldAndApplyCommandsInner } from '../../commands/commands'
import {
  InsertElementInsertionSubject,
  insertElementInsertionSubject,
} from '../../commands/insert-element-insertion-subject'
import { showReorderIndicator } from '../../commands/show-reorder-indicator-command'
import { updateFunctionCommand } from '../../commands/update-function-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { ParentBounds } from '../../controls/parent-bounds'
import { ParentOutlines } from '../../controls/parent-outlines'
import {
  DragOutlineControl,
  dragTargetsElementPaths,
} from '../../controls/select-mode/drag-outline-control'
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
import { boundingArea, InteractionSession, UIInteractionData } from '../interaction-state'
import {
  ReparentStrategy,
  reparentStrategyForPaste,
} from './reparent-helpers/reparent-strategy-helpers'
import {
  getApplicableReparentFactories,
  getApplicableReparentFactories2,
} from './reparent-metastrategy'

export const uiInsertMetaStrategy: MetaCanvasStrategy = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): Array<CanvasStrategy> => {
  if (interactionSession == null || interactionSession.interactionData.type !== 'UI') {
    return []
  }
  const insertionSubjects = getInsertionSubjectsFromInteractionTarget(canvasState.interactionTarget)
  if (insertionSubjects.length !== 1) {
    return []
  }
  const parent =
    insertionSubjects[0].parent?.target ??
    getStoryboardElementPath(canvasState.projectContents, canvasState.openFile)
  if (parent == null) {
    return []
  }

  const reparentStrategy = reparentStrategyForPaste(canvasState.startingMetadata, parent)
  const applicableReparentFactories = getApplicableReparentFactories2(
    canvasState,
    reparentStrategy.strategy,
    reparentStrategy.isFallback,
    parent,
  )

  return mapDropNulls(
    (result): CanvasStrategy | null => {
      const name = getUIInsertStrategyName(result.strategyType, result.targetParentDisplayType)

      return uiInsertStrategyFactory(
        canvasState,
        interactionSession,
        customStrategyState,
        result.factory,
        name,
        result.fitness,
      )
    },
    [applicableReparentFactories],
  )
}

function getUIInsertStrategyName(
  strategyType: ReparentStrategy,
  parentDisplayType: 'flex' | 'flow',
) {
  switch (strategyType) {
    case 'REPARENT_AS_ABSOLUTE':
      return 'Absolute Insert'
    case 'REPARENT_AS_STATIC':
      if (parentDisplayType === 'flex') {
        return 'Flex Insert'
      } else {
        return 'Flow Insert'
      }
  }
}

function uiInsertStrategyFactory(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
  reparentStrategyToUse: CanvasStrategyFactory,
  name: string,
  fitness: number,
): CanvasStrategy | null {
  const insertionSubjects = getInsertionSubjectsFromInteractionTarget(canvasState.interactionTarget)
  if (insertionSubjects.length !== 1) {
    return null
  }
  const insertionSubject = insertionSubjects[0]
  return {
    id: name,
    name: name,
    controlsToRender: [], // Uses existing hooks in select-mode-hooks.tsx
    fitness:
      interactionSession != null && interactionSession.interactionData.type === 'UI' ? fitness : 0,
    apply: (strategyLifecycle) => {
      if (interactionSession != null && interactionSession.interactionData.type === 'UI') {
        const insertionCommand = getInsertionCommands(
          insertionSubject,
          interactionSession,
          insertionSubject.defaultSize,
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
      }
      // Fallback for when the checks above are not satisfied.
      return emptyStrategyApplicationResult
    },
  }
}

function getInsertionCommands(
  subject: InsertionSubject,
  interactionSession: InteractionSession,
  insertionSubjectSize: Size,
): { command: InsertElementInsertionSubject; frame: CanvasRectangle } | null {
  if (interactionSession.interactionData.type !== 'UI') {
    return null
  }

  const frame = canvasRectangle({
    x: 0,
    y: 0,
    width: insertionSubjectSize.width,
    height: insertionSubjectSize.height,
  })

  const updatedAttributesWithPosition = getStyleAttributesForFrameInAbsolutePosition(subject, frame)

  const updatedInsertionSubject: InsertionSubject = {
    ...subject,
    parent: insertionParent(null, null),
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

  const patchedInteractionData: UIInteractionData = { type: 'UI' }

  const patchedInteractionSession: InteractionSession = {
    ...interactionSession,
    activeControl: boundingArea(),
    interactionData: patchedInteractionData,
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

  // We only want the commands that are applied at the end of the reparent, as we'll be resizing afterwards
  return foldAndApplyCommandsInner(editorState, [], reparentCommands, 'end-interaction')
    .statePatches
}
