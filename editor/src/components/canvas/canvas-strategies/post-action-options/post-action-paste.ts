import type { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { getAllUniqueUids } from '../../../../core/model/get-unique-ids'
import { getStoryboardElementPath } from '../../../../core/model/scene-utils'
import { mapArrayToDictionary, stripNulls } from '../../../../core/shared/array-utils'
import type { Either } from '../../../../core/shared/either'
import { isLeft, left, right } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import type {
  ElementInstanceMetadataMap,
  JSXElementChild,
} from '../../../../core/shared/element-template'
import {
  zeroCanvasPoint,
  canvasRectangle,
  canvasPoint,
  offsetPoint,
} from '../../../../core/shared/math-utils'
import type { CanvasPoint } from '../../../../core/shared/math-utils'
import type { ElementPath, NodeModules } from '../../../../core/shared/project-file-types'
import { fixUtopiaElement } from '../../../../core/shared/uid-utils'
import { assertNever } from '../../../../core/shared/utils'
import { getTargetParentForPaste } from '../../../../utils/clipboard'
import type { ElementPasteWithMetadata, ReparentTargetForPaste } from '../../../../utils/clipboard'
import type { IndexPosition } from '../../../../utils/utils'
import { absolute, front } from '../../../../utils/utils'
import type { ProjectContentTreeRoot } from '../../../assets'
import type { ElementPaste } from '../../../editor/action-types'
import type {
  AllElementProps,
  DerivedState,
  EditorState,
  PasteHerePostActionMenuData,
  PastePostActionMenuData,
  PasteToReplacePostActionMenuData,
} from '../../../editor/store/editor-state'
import { childInsertionPath } from '../../../editor/store/insertion-path'
import type { CanvasCommand } from '../../commands/commands'
import { foldAndApplyCommandsInner } from '../../commands/commands'
import { deleteElement } from '../../commands/delete-element-command'
import { queueGroupTrueUp } from '../../commands/queue-group-true-up-command'
import { showToastCommand } from '../../commands/show-toast-command'
import { updateFunctionCommand } from '../../commands/update-function-command'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import {
  absolutePositionForPaste,
  offsetPositionInPasteBoundingBox,
} from '../strategies/reparent-helpers/reparent-helpers'
import type { ElementPathLookup } from '../strategies/reparent-helpers/reparent-property-changes'
import {
  getReparentPropertyChanges,
  positionElementToCoordinatesCommands,
} from '../strategies/reparent-helpers/reparent-property-changes'
import { reparentStrategyForPaste } from '../strategies/reparent-helpers/reparent-strategy-helpers'
import type { ReparentStrategy } from '../strategies/reparent-helpers/reparent-strategy-helpers'
import type { ElementToReparent, PathToReparent } from '../strategies/reparent-utils'
import { elementToReparent, getReparentOutcomeMultiselect } from '../strategies/reparent-utils'
import { collectGroupTrueUp } from './navigator-reparent'
import type { PostActionChoice } from './post-action-options'

interface EditorStateContext {
  projectContents: ProjectContentTreeRoot
  nodeModules: NodeModules
  openFile: string | null
  pasteTargetsToIgnore: Array<ElementPath>
  builtInDependencies: BuiltInDependencies
  startingMetadata: ElementInstanceMetadataMap
  startingElementPathTrees: ElementPathTrees
  startingAllElementProps: AllElementProps
}

interface PasteContext {
  selectedViews: ElementPath[]
  elementPasteWithMetadata: ElementPasteWithMetadata
  targetOriginalPathTrees: ElementPathTrees
  originalAllElementProps: AllElementProps
  canvasViewportCenter: CanvasPoint
  reparentStrategy: ReparentStrategy | null
  insertionPosition: CanvasPoint | null
  keepSelectedViews?: boolean // optionally selected views can be cleared outside of pasteChoiceCommon
}

export interface ElementOrPathToInsert {
  elementPath: ElementPath
  pathToReparent: ElementToReparent | PathToReparent
  intendedCoordinates: CanvasPoint
  newUID: string
}

function pasteChoiceCommon(
  target: ReparentTargetForPaste,
  editorStateContext: EditorStateContext,
  pasteContext: PasteContext,
): Array<CanvasCommand> | null {
  const indexPosition =
    target.type === 'sibling'
      ? absolute(
          MetadataUtils.getIndexInParent(
            editorStateContext.startingMetadata,
            editorStateContext.startingElementPathTrees,
            target.siblingPath,
          ) + 1,
        )
      : front()

  let oldUIDsToNewUidsLookup: { [uid: string]: { oldUIDs: string[]; newUIDs: string[] } } = {}
  let fixedUIDMappingNewUIDS: Array<string> = []
  const elementsToInsert: Array<ElementOrPathToInsert> =
    pasteContext.elementPasteWithMetadata.elements.map((elementPaste) => {
      const existingIDs = [
        ...getAllUniqueUids(editorStateContext.projectContents).allIDs,
        ...fixedUIDMappingNewUIDS,
      ]
      const elementWithUID = fixUtopiaElement(elementPaste.element, new Set(existingIDs))
      fixedUIDMappingNewUIDS.push(...elementWithUID.mappings.map((value) => value.newUID))

      oldUIDsToNewUidsLookup = {
        ...oldUIDsToNewUidsLookup,
        [elementWithUID.value.uid]: {
          oldUIDs: getUidsFromJSXElementChild(elementPaste.element),
          newUIDs: getUidsFromJSXElementChild(elementWithUID.value),
        },
      }

      const intendedCoordinates = (() => {
        if (pasteContext.insertionPosition != null) {
          const pointRelativeToNewParent = MetadataUtils.getFrameRelativeToTargetContainingBlock(
            target.parentPath.intendedParentPath,
            editorStateContext.startingMetadata,
            canvasRectangle({
              x: pasteContext.insertionPosition.x,
              y: pasteContext.insertionPosition.y,
              width: 0,
              height: 0,
            }),
          )

          return offsetPoint(
            pointRelativeToNewParent != null
              ? canvasPoint({ x: pointRelativeToNewParent.x, y: pointRelativeToNewParent.y })
              : pasteContext.insertionPosition,
            offsetPositionInPasteBoundingBox(
              elementPaste.originalElementPath,
              pasteContext.elementPasteWithMetadata.elements.map(
                (element) => element.originalElementPath,
              ),
              pasteContext.elementPasteWithMetadata.targetOriginalContextMetadata,
            ),
          )
        } else {
          return absolutePositionForPaste(
            target,
            elementPaste.originalElementPath,
            pasteContext.elementPasteWithMetadata.elements.map(
              (element) => element.originalElementPath,
            ),
            {
              originalTargetMetadata:
                pasteContext.elementPasteWithMetadata.targetOriginalContextMetadata,
              originalPathTrees: pasteContext.targetOriginalPathTrees,
              currentMetadata: editorStateContext.startingMetadata,
              currentPathTrees: editorStateContext.startingElementPathTrees,
            },
            editorStateContext.startingAllElementProps,
            editorStateContext.startingElementPathTrees,
            pasteContext.canvasViewportCenter,
          )
        }
      })()

      return {
        elementPath: elementPaste.originalElementPath,
        pathToReparent: elementToReparent(elementWithUID.value, elementPaste.importsToAdd),
        intendedCoordinates: intendedCoordinates,
        newUID: elementWithUID.value.uid,
      }
    })
  return staticReparentAndUpdatePosition(
    target,
    editorStateContext,
    pasteContext,
    elementsToInsert,
    indexPosition,
    oldUIDsToNewUidsLookup,
    null,
  )
}

export function staticReparentAndUpdatePosition(
  target: ReparentTargetForPaste,
  editorStateContext: EditorStateContext,
  pasteContext: PasteContext,
  elementsToInsert: Array<ElementOrPathToInsert>,
  indexPosition: IndexPosition,
  oldUIDsToNewUidsLookup: { [uid: string]: { oldUIDs: string[]; newUIDs: string[] } } | null,
  newPathsAfterReparent: Array<ElementPath> | null,
): Array<CanvasCommand> | null {
  const reparentCommands = getReparentOutcomeMultiselect(
    editorStateContext.builtInDependencies,
    editorStateContext.projectContents,
    editorStateContext.nodeModules,
    editorStateContext.openFile,
    elementsToInsert.map((e) => e.pathToReparent),
    target.parentPath,
    'always',
    indexPosition,
  )

  if (reparentCommands == null) {
    return null
  }

  const strategy =
    pasteContext.reparentStrategy != null
      ? pasteContext.reparentStrategy
      : reparentStrategyForPaste(
          editorStateContext.startingMetadata,
          editorStateContext.startingAllElementProps,
          editorStateContext.startingElementPathTrees,
          target.parentPath.intendedParentPath,
        )

  const commands = elementsToInsert.flatMap((elementToInsert) => {
    return [
      updateFunctionCommand('always', (editor, commandLifecycle) => {
        const newPath =
          editor.canvas.controls.reparentedToPaths.find(
            (path) => EP.toUid(path) === elementToInsert.newUID,
          ) ?? newPathsAfterReparent?.find((path) => EP.toUid(path) === elementToInsert.newUID)

        const childPathLookup: ElementPathLookup = (() => {
          if (oldUIDsToNewUidsLookup != null) {
            const { oldUIDs, newUIDs } = oldUIDsToNewUidsLookup[elementToInsert.newUID]
            return zip(oldUIDs, newUIDs, (a, b) => ({
              oldUid: a,
              newUid: b,
            })).reduce((acc: ElementPathLookup, { oldUid, newUid }) => {
              const newPathForUid = editor.canvas.controls.reparentedToPaths.find(
                (path) => EP.toUid(path) === newUid,
              )
              return { ...acc, [oldUid]: newPathForUid }
            }, {})
          } else if (newPathsAfterReparent != null) {
            return mapArrayToDictionary(
              newPathsAfterReparent,
              (k) => EP.toUid(k),
              (v) => v,
            )
          }
          return {}
        })()

        if (newPath == null) {
          return []
        }

        const pastedElementMetadata = MetadataUtils.findElementByElementPath(
          pasteContext.elementPasteWithMetadata.targetOriginalContextMetadata,
          elementToInsert.elementPath,
        )

        const propertyChangeCommands = getReparentPropertyChanges(
          strategy,
          elementToInsert.elementPath,
          newPath,
          target.parentPath.intendedParentPath,
          pasteContext.elementPasteWithMetadata.targetOriginalContextMetadata,
          pasteContext.targetOriginalPathTrees,
          editor.jsxMetadata,
          editor.elementPathTree,
          editor.projectContents,
          editor.canvas.openFile?.filename ?? null,
          pastedElementMetadata?.specialSizeMeasurements.position ?? null,
          pastedElementMetadata?.specialSizeMeasurements.display ?? null,
          pasteContext.originalAllElementProps,
          childPathLookup,
        )

        const absolutePositioningCommands =
          strategy === 'REPARENT_AS_STATIC'
            ? []
            : positionElementToCoordinatesCommands(
                { oldPath: elementToInsert.elementPath, newPath: newPath },
                pasteContext.originalAllElementProps,
                {
                  originalTargetMetadata:
                    pasteContext.elementPasteWithMetadata.targetOriginalContextMetadata,
                  originalPathTrees: pasteContext.targetOriginalPathTrees,
                  currentMetadata: editor.jsxMetadata,
                  currentPathTrees: editor.elementPathTree,
                },
                elementToInsert.intendedCoordinates,
                childPathLookup,
              )

        const propertyCommands = [...propertyChangeCommands, ...absolutePositioningCommands]

        return foldAndApplyCommandsInner(
          editor,
          [],
          [...propertyCommands, updateSelectedViews('always', [...editor.selectedViews, newPath])],
          commandLifecycle,
        ).statePatches
      }),
    ]
  })

  const groupTrueUpPaths = elementsToInsert.flatMap((element) =>
    collectGroupTrueUp(
      editorStateContext.projectContents,
      editorStateContext.startingMetadata,
      editorStateContext.startingElementPathTrees,
      editorStateContext.startingAllElementProps,
      target.parentPath.intendedParentPath,
      EP.appendToPath(target.parentPath.intendedParentPath, element.newUID),
      element.elementPath,
    ),
  )

  return stripNulls([
    pasteContext.keepSelectedViews ? null : updateSelectedViews('always', []),
    ...reparentCommands,
    ...commands,
    wildcardPatch('always', {
      canvas: {
        controls: {
          reparentedToPaths: {
            $set: [],
          },
        },
      },
    }),
    ...groupTrueUpPaths.map((path) => queueGroupTrueUp(path)),
  ])
}

export const PropsPreservedPastePostActionChoiceId = 'post-action-choice-props-preserved'

export const PropsPreservedPastePostActionChoice = (
  postActionMenuData: PastePostActionMenuData,
): PostActionChoice => ({
  name: 'Paste with variables preserved',
  id: PropsPreservedPastePostActionChoiceId,
  run: (store, derived, builtInDependencies) =>
    pasteChoiceCommon(
      postActionMenuData.target,
      {
        builtInDependencies: builtInDependencies,
        nodeModules: store.nodeModules.files,
        openFile: store.canvas.openFile?.filename ?? null,
        pasteTargetsToIgnore: postActionMenuData.pasteTargetsToIgnore,
        projectContents: store.projectContents,
        startingMetadata: store.jsxMetadata,
        startingElementPathTrees: store.elementPathTree,
        startingAllElementProps: store.allElementProps,
      },
      {
        selectedViews: store.selectedViews,
        elementPasteWithMetadata: postActionMenuData.dataWithPropsPreserved,
        targetOriginalPathTrees: postActionMenuData.targetOriginalPathTrees,
        canvasViewportCenter: postActionMenuData.canvasViewportCenter,
        originalAllElementProps: postActionMenuData.originalAllElementProps,
        reparentStrategy: null,
        insertionPosition: null,
      },
    ),
})

export const PropsReplacedPastePostActionChoiceId = 'post-action-choice-props-replaced'

export const PropsReplacedPastePostActionChoice = (
  postActionMenuData: PastePostActionMenuData,
): PostActionChoice | null => {
  if (postActionMenuData.dataWithPropsReplaced == null) {
    return null
  }

  // to placate the typechecker
  const dataWithPropsReplaces = postActionMenuData.dataWithPropsReplaced

  return {
    name: 'Paste with variables replaced',
    id: PropsReplacedPastePostActionChoiceId,
    run: (store, derived, builtInDependencies) =>
      pasteChoiceCommon(
        postActionMenuData.target,
        {
          builtInDependencies: builtInDependencies,
          nodeModules: store.nodeModules.files,
          openFile: store.canvas.openFile?.filename ?? null,
          pasteTargetsToIgnore: postActionMenuData.pasteTargetsToIgnore,
          projectContents: store.projectContents,
          startingMetadata: store.jsxMetadata,
          startingElementPathTrees: store.elementPathTree,
          startingAllElementProps: store.allElementProps,
        },
        {
          selectedViews: store.selectedViews,
          elementPasteWithMetadata: dataWithPropsReplaces,
          targetOriginalPathTrees: postActionMenuData.targetOriginalPathTrees,
          canvasViewportCenter: postActionMenuData.canvasViewportCenter,
          originalAllElementProps: postActionMenuData.originalAllElementProps,
          reparentStrategy: null,
          insertionPosition: null,
        },
      ),
  }
}

export function getUidsFromJSXElementChild(element: JSXElementChild): string[] {
  switch (element.type) {
    case 'JSX_ELEMENT':
    case 'JSX_FRAGMENT':
      return [element.uid, ...element.children.flatMap(getUidsFromJSXElementChild)]
    case 'JSX_CONDITIONAL_EXPRESSION':
      return [
        element.uid,
        ...getUidsFromJSXElementChild(element.whenTrue),
        ...getUidsFromJSXElementChild(element.whenFalse),
      ]
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'ATTRIBUTE_VALUE':
    case 'JSX_TEXT_BLOCK':
      return [element.uid]
    default:
      assertNever(element)
  }
}

function zip<A, B, C>(one: A[], other: B[], make: (a: A, b: B) => C): C[] {
  const doZip = (oneInner: A[], otherInner: B[]) =>
    oneInner.map((elem, idx) => make(elem, otherInner[idx]))

  return one.length < other.length ? doZip(one, other) : doZip(one.slice(0, other.length), other)
}

export const PropsPreservedPasteHerePostActionChoiceId = 'props-preserved-paste-here-action-choice'

export const PropsPreservedPasteHerePostActionChoice = (
  data: PasteHerePostActionMenuData,
): PostActionChoice => ({
  name: 'Paste here with variables preserved',
  id: PropsPreservedPasteHerePostActionChoiceId,
  run: (editor, derived, builtInDependencies) => {
    if (
      editor.internalClipboard.elements.length !== 1 ||
      editor.internalClipboard.elements[0].copyDataWithPropsPreserved == null
    ) {
      return []
    }

    const elementToPaste = editor.internalClipboard.elements[0].copyDataWithPropsPreserved.elements
    const targetParent = getTargetParentForPasteHere(editor, derived, elementToPaste)
    if (isLeft(targetParent)) {
      return [
        showToastCommand(
          'Paste element cannot find parent',
          'WARNING',
          'paste-elements-cannot-find-parent',
        ),
      ]
    }

    const originalMetadata =
      editor.internalClipboard.elements[0].copyDataWithPropsPreserved.targetOriginalContextMetadata
    const originalPathTree =
      editor.internalClipboard.elements[0].targetOriginalContextElementPathTrees

    return pasteChoiceCommon(
      targetParent.value,
      {
        builtInDependencies: builtInDependencies,
        nodeModules: editor.nodeModules.files,
        openFile: editor.canvas.openFile?.filename ?? null,
        pasteTargetsToIgnore: [],
        projectContents: editor.projectContents,
        startingMetadata: editor.jsxMetadata,
        startingElementPathTrees: editor.elementPathTree,
        startingAllElementProps: editor.allElementProps,
      },
      {
        selectedViews: editor.selectedViews,
        elementPasteWithMetadata: {
          elements: elementToPaste,
          targetOriginalContextMetadata: originalMetadata,
        },
        targetOriginalPathTrees: originalPathTree,
        canvasViewportCenter: zeroCanvasPoint,
        reparentStrategy: 'REPARENT_AS_ABSOLUTE',
        insertionPosition: data.position,
        originalAllElementProps: data.internalClipboard.elements[0].originalAllElementProps,
      },
    )
  },
})

export const PropsReplacedPasteHerePostActionChoiceId = 'props-replaced-paste-here-action-choice'

export const PropsReplacedPasteHerePostActionChoice = (
  data: PasteHerePostActionMenuData,
): PostActionChoice | null => {
  if (
    data.internalClipboard.elements.length !== 1 ||
    data.internalClipboard.elements[0].copyDataWithPropsReplaced == null
  ) {
    return null
  }
  return {
    name: 'Paste here with variables replaced',
    id: PropsReplacedPasteHerePostActionChoiceId,
    run: (editor, derived, builtInDependencies) => {
      if (
        editor.internalClipboard.elements.length !== 1 ||
        editor.internalClipboard.elements[0].copyDataWithPropsReplaced == null
      ) {
        return []
      }
      const elementToPaste = editor.internalClipboard.elements[0].copyDataWithPropsReplaced.elements
      const targetParent = getTargetParentForPasteHere(editor, derived, elementToPaste)
      if (isLeft(targetParent)) {
        return [
          showToastCommand(
            'Please reload the editor',
            'ERROR',
            'paste-elements-cannot-find-storyboard',
          ),
        ]
      }

      const originalMetadata =
        editor.internalClipboard.elements[0].copyDataWithPropsPreserved
          .targetOriginalContextMetadata
      const originalPathTree =
        editor.internalClipboard.elements[0].targetOriginalContextElementPathTrees

      return pasteChoiceCommon(
        targetParent.value,
        {
          builtInDependencies: builtInDependencies,
          nodeModules: editor.nodeModules.files,
          openFile: editor.canvas.openFile?.filename ?? null,
          pasteTargetsToIgnore: [],
          projectContents: editor.projectContents,
          startingMetadata: editor.jsxMetadata,
          startingElementPathTrees: editor.elementPathTree,
          startingAllElementProps: editor.allElementProps,
        },
        {
          selectedViews: editor.selectedViews,
          elementPasteWithMetadata: {
            elements: elementToPaste,
            targetOriginalContextMetadata: originalMetadata,
          },
          targetOriginalPathTrees: originalPathTree,
          canvasViewportCenter: zeroCanvasPoint,
          reparentStrategy: 'REPARENT_AS_ABSOLUTE',
          insertionPosition: data.position,
          originalAllElementProps: data.internalClipboard.elements[0].originalAllElementProps,
        },
      )
    },
  }
}

function getTargetParentForPasteHere(
  editor: EditorState,
  derived: DerivedState,
  elementToPaste: Array<ElementPaste>,
): Either<string, ReparentTargetForPaste> {
  const originalMetadata =
    editor.internalClipboard.elements[0].copyDataWithPropsPreserved.targetOriginalContextMetadata
  const originalPathTree =
    editor.internalClipboard.elements[0].targetOriginalContextElementPathTrees

  const target = getTargetParentForPaste(
    editor.projectContents,
    editor.selectedViews,
    editor.nodeModules.files,
    editor.canvas.openFile?.filename ?? null,
    editor.jsxMetadata,
    editor.pasteTargetsToIgnore,
    {
      elementPaste: elementToPaste,
      originalContextMetadata: originalMetadata,
      originalContextElementPathTrees: originalPathTree,
    },
    editor.elementPathTree,
  )

  const storyboardPath = getStoryboardElementPath(
    editor.projectContents,
    editor.canvas.openFile?.filename ?? null,
  )

  if (storyboardPath == null) {
    return left('No storyboard found')
  }

  if (isLeft(target)) {
    return right({ type: 'parent', parentPath: childInsertionPath(storyboardPath) })
  }

  // parent targets can be the scene components root div, a scene/element directly on the canvas, or the storyboard
  const allPaths = [
    target.value.parentPath.intendedParentPath,
    ...EP.getAncestors(target.value.parentPath.intendedParentPath),
  ]
  const sceneComponentRoot = allPaths.find((path) =>
    derived.autoFocusedPaths.some((autofocused) => EP.pathsEqual(autofocused, EP.parentPath(path))),
  )
  const storyboardChild = allPaths.find((path) =>
    EP.pathsEqual(storyboardPath, EP.parentPath(path)),
  )
  const targetParent = sceneComponentRoot ?? storyboardChild ?? storyboardPath

  return right({ type: 'parent', parentPath: childInsertionPath(targetParent) })
}

export const PropsPreservedPasteToReplacePostActionChoiceId =
  'props-preserved-paste-to-replace-action-choice'

export const PropsPreservedPasteToReplacePostActionChoice = (
  data: PasteToReplacePostActionMenuData,
): PostActionChoice => ({
  name: 'Paste to replace with variables preserved',
  id: PropsPreservedPasteToReplacePostActionChoiceId,
  run: (editor, derived, builtInDependencies) => {
    if (
      editor.internalClipboard.elements.length !== 1 ||
      editor.internalClipboard.elements[0].copyDataWithPropsPreserved == null
    ) {
      return []
    }

    const elementToPaste = editor.internalClipboard.elements[0].copyDataWithPropsPreserved.elements
    const originalMetadata =
      editor.internalClipboard.elements[0].copyDataWithPropsPreserved.targetOriginalContextMetadata
    const originalPathTree =
      editor.internalClipboard.elements[0].targetOriginalContextElementPathTrees

    return pasteToReplaceCommands(
      editor,
      builtInDependencies,
      data.targets,
      elementToPaste,
      originalMetadata,
      originalPathTree,
    )
  },
})

export const PropsReplacedPasteToReplacePostActionChoiceId =
  'props-replaced-paste-to-replace-action-choice'

export const PropsReplacedPasteToReplacePostActionChoice = (
  data: PasteToReplacePostActionMenuData,
): PostActionChoice | null => {
  if (
    data.internalClipboard.elements.length !== 1 ||
    data.internalClipboard.elements[0].copyDataWithPropsReplaced == null
  ) {
    return null
  }
  return {
    name: 'Paste to replace with variables replaced',
    id: PropsReplacedPasteToReplacePostActionChoiceId,
    run: (editor, derived, builtInDependencies) => {
      if (
        editor.internalClipboard.elements.length !== 1 ||
        editor.internalClipboard.elements[0].copyDataWithPropsReplaced == null
      ) {
        return []
      }
      const elementToPaste = editor.internalClipboard.elements[0].copyDataWithPropsReplaced.elements
      const originalMetadata =
        editor.internalClipboard.elements[0].copyDataWithPropsPreserved
          .targetOriginalContextMetadata
      const originalPathTree =
        editor.internalClipboard.elements[0].targetOriginalContextElementPathTrees

      return pasteToReplaceCommands(
        editor,
        builtInDependencies,
        data.targets,
        elementToPaste,
        originalMetadata,
        originalPathTree,
      )
    },
  }
}

function pasteToReplaceCommands(
  editor: EditorState,
  builtInDependencies: BuiltInDependencies,
  unfilteredTargets: Array<ElementPath>,
  elementToPaste: Array<ElementPaste>,
  originalMetadata: ElementInstanceMetadataMap,
  originalPathTree: ElementPathTrees,
): Array<CanvasCommand> {
  const targets = unfilteredTargets.filter((target) => !EP.isRootElementOfInstance(target))

  const pasteCommands = targets.flatMap((target) => {
    return [
      updateFunctionCommand('always', (updatedEditor, commandLifecycle) => {
        const element = MetadataUtils.findElementByElementPath(editor.jsxMetadata, target)
        const position = MetadataUtils.getFrameOrZeroRectInCanvasCoords(target, editor.jsxMetadata)
        const strategy = MetadataUtils.isPositionAbsolute(element)
          ? 'REPARENT_AS_ABSOLUTE'
          : 'REPARENT_AS_STATIC'

        const parentInsertionPath = MetadataUtils.getReparentTargetOfTarget(
          updatedEditor.jsxMetadata,
          target,
        )
        if (parentInsertionPath == null) {
          return []
        }
        const commands = pasteChoiceCommon(
          {
            type: 'sibling',
            siblingPath: target,
            parentPath: parentInsertionPath,
          },
          {
            builtInDependencies: builtInDependencies,
            nodeModules: editor.nodeModules.files,
            openFile: editor.canvas.openFile?.filename ?? null,
            pasteTargetsToIgnore: [],
            projectContents: updatedEditor.projectContents,
            startingMetadata: editor.jsxMetadata,
            startingElementPathTrees: editor.elementPathTree,
            startingAllElementProps: editor.allElementProps,
          },
          {
            selectedViews: editor.selectedViews,
            elementPasteWithMetadata: {
              elements: elementToPaste,
              targetOriginalContextMetadata: originalMetadata,
            },
            targetOriginalPathTrees: originalPathTree,
            canvasViewportCenter: zeroCanvasPoint,
            reparentStrategy: strategy,
            insertionPosition: position,
            keepSelectedViews: true,
            originalAllElementProps: editor.allElementProps,
          },
        )
        if (commands == null) {
          return []
        }
        return foldAndApplyCommandsInner(updatedEditor, [], commands, commandLifecycle).statePatches
      }),
    ]
  }, [] as Array<CanvasCommand>)
  const deleteCommands = targets.map((target) => deleteElement('always', target))

  return stripNulls([
    updateSelectedViews('always', []),
    ...pasteCommands,
    ...deleteCommands,
    targets.length !== unfilteredTargets.length
      ? showToastCommand('Cannot replace root elements', 'WARNING', 'paste-to-replace-on-root')
      : null,
  ])
}
