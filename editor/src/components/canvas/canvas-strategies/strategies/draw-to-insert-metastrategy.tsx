import type { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { LayoutHelpers } from '../../../../core/layout/layout-helpers'
import {
  createFakeMetadataForElement,
  getRootPath,
  MetadataUtils,
} from '../../../../core/model/element-metadata-utils'
import { isImg } from '../../../../core/model/project-file-utils'
import { mapDropNulls, stripNulls } from '../../../../core/shared/array-utils'
import { foldEither } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import type {
  ElementInstanceMetadataMap,
  JSXAttributes,
} from '../../../../core/shared/element-template'
import { emptyComments, jsExpressionValue } from '../../../../core/shared/element-template'
import type { CanvasRectangle, Size } from '../../../../core/shared/math-utils'
import { canvasPoint, canvasRectangle } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { cmdModifier } from '../../../../utils/modifiers'
import type { InsertionSubject } from '../../../editor/editor-modes'
import type { EditorState, EditorStatePatch } from '../../../editor/store/editor-state'
import type { CanvasCommand } from '../../commands/commands'
import { foldAndApplyCommandsInner } from '../../commands/commands'
import type { InsertElementInsertionSubject } from '../../commands/insert-element-insertion-subject'
import { insertElementInsertionSubject } from '../../commands/insert-element-insertion-subject'
import { showReorderIndicator } from '../../commands/show-reorder-indicator-command'
import { updateFunctionCommand } from '../../commands/update-function-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { ParentBounds } from '../../controls/parent-bounds'
import { ParentOutlines } from '../../controls/parent-outlines'
import { FlexReparentTargetIndicator } from '../../controls/select-mode/flex-reparent-target-indicator'
import type { CanvasStrategyFactory, MetaCanvasStrategy } from '../canvas-strategies'
import {
  findCanvasStrategy,
  findElementPathUnderInteractionPoint,
  getWrapperWithGeneratedUid,
  getWrappingCommands,
  pickCanvasStateFromEditorState,
  pickCanvasStateFromEditorStateWithMetadata,
  RegisteredCanvasStrategies,
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
import { boundingArea } from '../interaction-state'
import { getApplicableReparentFactories } from './reparent-metastrategy'
import type { ReparentStrategy } from './reparent-helpers/reparent-strategy-helpers'
import { styleStringInArray } from '../../../../utils/common-constants'
import type { ValueAtPath } from '../../../../core/shared/jsx-attributes'
import { setJSXValuesAtPaths } from '../../../../core/shared/jsx-attributes'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { MaxContent } from '../../../inspector/inspector-common'
import { wrapInContainerCommand } from '../../commands/wrap-in-container-command'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import type { InsertionPath } from '../../../editor/store/insertion-path'
import { childInsertionPath } from '../../../editor/store/insertion-path'
import { gridDrawToInsertStrategy } from './grid-draw-to-insert-strategy'
import { assertNever } from '../../../../core/shared/utils'

/**
 *
 * NOTE: https://github.com/concrete-utopia/utopia/pull/5819 deleted a bunch of
 * tests for this strategy that relied on the legacy insert menu.
 *
 * ! IF YOU WORK ON THIS, PLEASE RESURRECT THE TESTS AND MAKE THEM USE THE 'F' SHORTCUT !
 *
 */
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

  const targetParent = findElementPathUnderInteractionPoint(canvasState, interactionSession)

  if (
    MetadataUtils.isGridLayoutedContainer(
      MetadataUtils.findElementByElementPath(canvasState.startingMetadata, targetParent),
    )
  ) {
    return stripNulls([
      gridDrawToInsertStrategy(canvasState, interactionSession, customStrategyState),
    ])
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
    true, // cmd is necessary to allow reparenting
    true,
    'allow-smaller-parent',
    customStrategyState,
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
  parentDisplayType: 'flex' | 'flow' | 'grid',
): string {
  switch (strategyType) {
    case 'REPARENT_AS_ABSOLUTE':
      return 'Draw to Insert (Abs)'
    case 'REPARENT_AS_STATIC':
      if (parentDisplayType === 'flex') {
        return 'Draw to Insert (Flex)'
      } else if (parentDisplayType === 'grid') {
        return 'Draw to Insert (Grid)'
      } else {
        return 'Draw to Insert (Flow)'
      }
    case 'REPARENT_INTO_GRID':
      return 'Draw to Insert (Grid)'
    default:
      assertNever(strategyType)
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
  targetParent: InsertionPath,
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
    descriptiveLabel: 'Drawing To Insert',
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
    ], // Uses existing hooks in select-mode-hooks.tsx
    fitness: !insertionSubject.textEdit && drawToInsertFitness(interactionSession) ? fitness : 0,
    apply: (strategyLifecycle) => {
      const rootPath = getRootPath(canvasState.startingMetadata)
      if (interactionSession != null) {
        if (interactionSession.interactionData.type === 'DRAG') {
          const maybeWrapperWithUid = getWrapperWithGeneratedUid(
            customStrategyState,
            canvasState,
            insertionSubjects,
          )
          if (interactionSession.interactionData.drag != null) {
            if (rootPath == null) {
              throw new Error('Missing root path for draw interaction')
            }
            const insertionCommand = getInsertionCommands(
              rootPath,
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

              const newPath = EP.appendToPath(targetParent.intendedParentPath, insertionSubject.uid)

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
                  insertionCommand.command,
                  reparentCommand,
                  resizeCommand,
                  ...optionalWrappingCommand,
                ],
                {
                  strategyGeneratedUidsCache: {
                    [insertionSubject.uid]: maybeWrapperWithUid?.uid,
                  },
                },
              )
            }
          } else if (strategyLifecycle === 'end-interaction') {
            const defaultSizeType = insertionSubject.textEdit ? 'hug' : 'default-size'
            if (rootPath == null) {
              throw new Error('missing root path')
            }
            const insertionCommand = getInsertionCommands(
              rootPath,
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

              const newPath = EP.appendToPath(targetParent.intendedParentPath, insertionSubject.uid)

              const optionalWrappingCommand =
                maybeWrapperWithUid != null ? getWrappingCommands(newPath, maybeWrapperWithUid) : []

              return strategyApplicationResult(
                [insertionCommand.command, reparentCommand, ...optionalWrappingCommand],
                {
                  strategyGeneratedUidsCache: {
                    [insertionSubject.uid]: maybeWrapperWithUid?.uid,
                  },
                },
              )
            }
          } else {
            // drag is null, the cursor is not moved yet, but the mousedown already happened
            return strategyApplicationResult(
              getHighlightAndReorderIndicatorCommands(targetParent.intendedParentPath, targetIndex),
            )
          }
        } else if (interactionSession.interactionData.type === 'HOVER') {
          return strategyApplicationResult(
            getHighlightAndReorderIndicatorCommands(targetParent.intendedParentPath, targetIndex),
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
  storyboardPath: ElementPath,
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

    const insertionPath = childInsertionPath(storyboardPath)

    return {
      command: insertElementInsertionSubject('always', updatedInsertionSubject, insertionPath),
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

    const insertionPath = childInsertionPath(storyboardPath)

    return {
      command: insertElementInsertionSubject('always', updatedInsertionSubject, insertionPath),
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

    const insertionPath = childInsertionPath(storyboardPath)

    return {
      command: insertElementInsertionSubject('always', updatedInsertionSubject, insertionPath),
      frame: frame,
    }
  }
  return null
}

export function getStyleAttributesForFrameInAbsolutePosition(
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

export function updateInsertionSubjectWithAttributes(
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

  const rootPath = getRootPath(startingMetadata)
  if (rootPath == null) {
    throw new Error('Missing root path when running draw strategy')
  }

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

  // IMPORTANT! This canvas state is using an elementPathTree that does not include the newly inserted
  // element as the canvas state's startingElementPathTree. As it happens, this is fine right now,
  // because the resize strategies aren't picked based on the target element's index amongst its siblings,
  // and the updated latestElementPathTree (which will contain this newly inserted element) is available
  // when actually applying the strategies. If we ever need to pick a resize strategy based on the target
  // element's index, we will need to update the elementPathTree with the new element and pass it in here.
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
