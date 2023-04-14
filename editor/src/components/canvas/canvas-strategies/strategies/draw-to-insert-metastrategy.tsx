import { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { LayoutHelpers } from '../../../../core/layout/layout-helpers'
import {
  createFakeMetadataForElement,
  MetadataUtils,
} from '../../../../core/model/element-metadata-utils'
import { isImg } from '../../../../core/model/project-file-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { Either, foldEither } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import { elementPath } from '../../../../core/shared/element-path'
import {
  ElementInstanceMetadataMap,
  emptyComments,
  JSXAttributes,
  jsExpressionValue,
} from '../../../../core/shared/element-template'
import {
  canvasPoint,
  canvasRectangle,
  CanvasRectangle,
  Size,
} from '../../../../core/shared/math-utils'
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
import { ParentBounds } from '../../controls/parent-bounds'
import { ParentOutlines } from '../../controls/parent-outlines'
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
import { boundingArea, InteractionSession } from '../interaction-state'
import { getApplicableReparentFactories } from './reparent-metastrategy'
import { ReparentStrategy } from './reparent-helpers/reparent-strategy-helpers'
import { styleStringInArray } from '../../../../utils/common-constants'
import { setJSXValuesAtPaths, ValueAtPath } from '../../../../core/shared/jsx-attributes'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { LayoutPinnedProp, LayoutPinnedProps } from '../../../../core/layout/layout-helpers-new'
import { MapLike } from 'typescript'
import { FullFrame } from '../../../frame'
import { MaxContent } from '../../../inspector/inspector-common'
import { wrapInConditionalCommand } from '../../commands/wrap-in-conditional-command'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import { generateUidWithExistingComponents } from '../../../../core/model/element-template-utils'

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

  const insertionSubjects = getInsertionSubjectsFromInteractionTarget(canvasState.interactionTarget)
  if (insertionSubjects.length != 1) {
    return []
  }

  const pointOnCanvas =
    interactionSession.interactionData.type === 'DRAG'
      ? interactionSession.interactionData.originalDragStart
      : interactionSession.interactionData.point

  const applicableReparentFactories = getApplicableReparentFactories(
    canvasState,
    pointOnCanvas,
    interactionSession.interactionData.modifiers.cmd,
    true,
    'allow-smaller-parent',
  )

  return mapDropNulls((result): CanvasStrategy | null => {
    const name = getDrawToInsertStrategyName(result.strategyType, result.targetParentDisplayType)

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

export function getDrawToInsertStrategyName(
  strategyType: ReparentStrategy,
  parentDisplayType: 'flex' | 'flow',
) {
  switch (strategyType) {
    case 'REPARENT_AS_ABSOLUTE':
      return 'Draw to Insert (Abs)'
    case 'REPARENT_AS_STATIC':
      if (parentDisplayType === 'flex') {
        return 'Draw to Insert (Flex)'
      } else {
        return 'Draw to Insert (Flow)'
      }
  }
}

export function drawToInsertFitness(interactionSession: InteractionSession | null): boolean {
  return (
    interactionSession != null &&
    ((interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'RESIZE_HANDLE') ||
      interactionSession.interactionData.type === 'HOVER')
  )
}

export function drawToInsertStrategyFactory(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
  reparentStrategyToUse: CanvasStrategyFactory,
  name: string,
  fitness: number,
  targetParent: ElementPath,
  targetIndex: number | null,
): CanvasStrategy | null {
  const insertionSubjects = getInsertionSubjectsFromInteractionTarget(canvasState.interactionTarget)
  if (insertionSubjects.length !== 1) {
    return null
  }
  const insertionSubject = insertionSubjects[0]
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
        control: FlexReparentTargetIndicator,
        props: {},
        key: 'flex-reparent-target-indicator',
        show: 'visible-only-while-active',
      }),
    ], // Uses existing hooks in select-mode-hooks.tsx
    fitness: !insertionSubject.textEdit && drawToInsertFitness(interactionSession) ? fitness : 0,
    apply: (strategyLifecycle) => {
      if (interactionSession != null) {
        if (interactionSession.interactionData.type === 'DRAG') {
          const wrapperUID =
            insertionSubject.wrapInConditional === true
              ? customStrategyState.duplicatedElementNewUids[insertionSubject.uid] ??
                generateUidWithExistingComponents(canvasState.projectContents)
              : 'empty'
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

              const newPath = EP.appendToPath(targetParent, insertionSubject.uid)

              const optionalWrappingCommand = insertionSubject.wrapInConditional
                ? [
                    updateFunctionCommand(
                      'always',
                      (editorState, lifecycle): Array<EditorStatePatch> =>
                        foldAndApplyCommandsInner(
                          editorState,
                          [],
                          [wrapInConditionalCommand('always', newPath, wrapperUID)],
                          lifecycle,
                        ).statePatches,
                    ),
                  ]
                : []

              return strategyApplicationResult(
                [
                  insertionCommand.command,
                  reparentCommand,
                  resizeCommand,
                  ...optionalWrappingCommand,
                ],
                {
                  duplicatedElementNewUids: {
                    [insertionSubject.uid]: wrapperUID,
                  },
                },
              )
            }
          } else if (strategyLifecycle === 'end-interaction') {
            const defaultSizeType = insertionSubject.textEdit ? 'hug' : 'default-size'
            const insertionCommand = getInsertionCommands(
              insertionSubject,
              interactionSession,
              insertionSubject.defaultSize,
              defaultSizeType,
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

              const newPath = EP.appendToPath(targetParent, insertionSubject.uid)

              const optionalWrappingCommand = insertionSubject.wrapInConditional
                ? [
                    updateFunctionCommand(
                      'always',
                      (editorState, lifecycle): Array<EditorStatePatch> =>
                        foldAndApplyCommandsInner(
                          editorState,
                          [],
                          [wrapInConditionalCommand('always', newPath, wrapperUID)],
                          lifecycle,
                        ).statePatches,
                    ),
                  ]
                : []

              return strategyApplicationResult(
                [insertionCommand.command, reparentCommand, ...optionalWrappingCommand],
                {
                  duplicatedElementNewUids: {
                    [insertionSubject.uid]: wrapperUID,
                  },
                },
              )
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

    if (targetIndex != null && targetIndex > -1) {
      return [
        highlightParentCommand,
        showReorderIndicator(targetParent, targetIndex),
        clearSelectionCommand,
      ]
    } else {
      return [highlightParentCommand, clearSelectionCommand]
    }
  } else {
    return [clearSelectionCommand]
  }
}

const clearSelectionCommand: CanvasCommand = wildcardPatch('mid-interaction', {
  selectedViews: { $set: [] },
})

function getInsertionCommands(
  subject: InsertionSubject,
  interactionSession: InteractionSession,
  insertionSubjectSize: Size,
  sizing: 'zero-size' | 'default-size' | 'hug',
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

    const updatedInsertionSubject = updateInsertionSubjectWithAttributes(
      subject,
      updatedAttributesWithPosition,
    )

    return {
      command: insertElementInsertionSubject('always', updatedInsertionSubject),
      frame: frame,
    }
  } else if (interactionSession.interactionData.type === 'DRAG' && sizing === 'hug') {
    const pointOnCanvas = interactionSession.interactionData.dragStart
    const frame = canvasRectangle({
      x: pointOnCanvas.x,
      y: pointOnCanvas.y,
      width: 0,
      height: 0,
    })

    const updatedAttributesWithPosition = getStyleAttributesForFixedPositionAndSizeHug(
      subject,
      frame,
    )

    const updatedInsertionSubject = updateInsertionSubjectWithAttributes(
      subject,
      updatedAttributesWithPosition,
    )

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

    const updatedInsertionSubject = updateInsertionSubjectWithAttributes(
      subject,
      updatedAttributesWithPosition,
    )

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
      styleStringInArray,
    ),
  )
}

function getStyleAttributesForFixedPositionAndSizeHug(
  subject: InsertionSubject,
  frame: CanvasRectangle,
): JSXAttributes {
  const propsToSet: Array<ValueAtPath> = [
    {
      path: stylePropPathMappingFn('left', ['style']),
      value: jsExpressionValue(frame.x, emptyComments),
    },
    {
      path: stylePropPathMappingFn('top', ['style']),
      value: jsExpressionValue(frame.y, emptyComments),
    },
    {
      path: stylePropPathMappingFn('width', ['style']),
      value: jsExpressionValue(MaxContent, emptyComments),
    },
    {
      path: stylePropPathMappingFn('height', ['style']),
      value: jsExpressionValue(MaxContent, emptyComments),
    },
  ]

  const layoutProps = setJSXValuesAtPaths(subject.element.props, propsToSet)
  // Assign the new properties
  return foldEither(
    (_) => {
      throw new Error(`Problem setting frame on an element we just created.`)
    },
    (attr) => attr,
    layoutProps,
  )
}

function updateInsertionSubjectWithAttributes(
  subject: InsertionSubject,
  updatedAttributes: JSXAttributes,
): InsertionSubject {
  return {
    ...subject,
    parent: subject.parent,
    element: {
      ...subject.element,
      props: updatedAttributes,
    },
  }
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
