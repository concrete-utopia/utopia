import type { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { getAllUniqueUids } from '../../../../core/model/get-unique-ids'
import { getStoryboardElementPath } from '../../../../core/model/scene-utils'
import type { Either } from '../../../../core/shared/either'
import { isLeft, left, right } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import {
  zeroCanvasPoint,
  canvasRectangle,
  canvasPoint,
  offsetPoint,
} from '../../../../core/shared/math-utils'
import type { CanvasPoint } from '../../../../core/shared/math-utils'
import type { ElementPath, NodeModules } from '../../../../core/shared/project-file-types'
import { fixUtopiaElement } from '../../../../core/shared/uid-utils'
import { getTargetParentForPaste } from '../../../../utils/clipboard'
import type { ElementPasteWithMetadata, ReparentTargetForPaste } from '../../../../utils/clipboard'
import { absolute, front } from '../../../../utils/utils'
import type { ProjectContentTreeRoot } from '../../../assets'
import type { ElementPaste } from '../../../editor/action-types'
import type {
  AllElementProps,
  DerivedState,
  EditorState,
  PasteHerePostActionMenuData,
  PastePostActionMenuData,
} from '../../../editor/store/editor-state'
import { childInsertionPath } from '../../../editor/store/insertion-path'
import type { CanvasCommand } from '../../commands/commands'
import { foldAndApplyCommandsInner } from '../../commands/commands'
import { showToastCommand } from '../../commands/show-toast-command'
import { updateFunctionCommand } from '../../commands/update-function-command'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import {
  absolutePositionForPaste,
  offsetPositionInPasteBoundingBox,
} from '../strategies/reparent-helpers/reparent-helpers'
import {
  getReparentPropertyChanges,
  positionElementToCoordinatesCommands,
} from '../strategies/reparent-helpers/reparent-property-changes'
import { reparentStrategyForPaste } from '../strategies/reparent-helpers/reparent-strategy-helpers'
import type { ReparentStrategy } from '../strategies/reparent-helpers/reparent-strategy-helpers'
import { elementToReparent, getReparentOutcomeMultiselect } from '../strategies/reparent-utils'
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
  canvasViewportCenter: CanvasPoint
  reparentStrategy: ReparentStrategy | null
  insertionPosition: CanvasPoint | null
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

  let fixedUIDMappingNewUIDS: Array<string> = []
  const elementsToInsert = pasteContext.elementPasteWithMetadata.elements.map((elementPaste) => {
    const existingIDs = [
      ...getAllUniqueUids(editorStateContext.projectContents).allIDs,
      ...fixedUIDMappingNewUIDS,
    ]
    const elementWithUID = fixUtopiaElement(elementPaste.element, new Set(existingIDs))
    fixedUIDMappingNewUIDS.push(...elementWithUID.mappings.map((value) => value.newUID))

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
          pasteContext.canvasViewportCenter,
        )
      }
    })()

    return {
      elementPath: elementPaste.originalElementPath,
      pathToReparent: elementToReparent(elementWithUID.value, elementPaste.importsToAdd),
      intendedCoordinates: intendedCoordinates,
      uid: elementWithUID.value.uid,
    }
  })

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
        const newPath = Object.values(editor.canvas.controls.reparentedToPaths).find(
          // TODO: should become a lookup based on elementToInsert.uid
          (path) => EP.toUid(path) === elementToInsert.uid,
        )

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
        )

        const absolutePositioningCommands =
          strategy === 'REPARENT_AS_STATIC'
            ? []
            : positionElementToCoordinatesCommands(newPath, elementToInsert.intendedCoordinates)

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

  return [
    updateSelectedViews('always', []),
    ...reparentCommands,
    ...commands,
    wildcardPatch('always', {
      canvas: {
        controls: {
          reparentedToPaths: {
            $set: {},
          },
        },
      },
    }),
  ]
}

export const PasteWithPropsPreservedPostActionChoiceId = 'post-action-choice-props-preserved'

export const PasteWithPropsPreservedPostActionChoice = (
  postActionMenuData: PastePostActionMenuData,
): PostActionChoice => ({
  name: 'Paste with variables preserved',
  id: PasteWithPropsPreservedPostActionChoiceId,
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
        reparentStrategy: null,
        insertionPosition: null,
      },
    ),
})

export const PasteWithPropsReplacedPostActionChoiceId = 'post-action-choice-props-replaced'

export const PasteWithPropsReplacedPostActionChoice = (
  postActionMenuData: PastePostActionMenuData,
): PostActionChoice | null => {
  if (postActionMenuData.dataWithPropsReplaced == null) {
    return null
  }

  // to placate the typechecker
  const dataWithPropsReplaces = postActionMenuData.dataWithPropsReplaced

  return {
    name: 'Paste with variables replaced',
    id: PasteWithPropsReplacedPostActionChoiceId,
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
          reparentStrategy: null,
          insertionPosition: null,
        },
      ),
  }
}

export const PasteHereWithPropsPreservedPostActionChoiceId =
  'post-here-action-choice-props-preserved'

export const PasteHereWithPropsPreservedPostActionChoice = (
  data: PasteHerePostActionMenuData,
): PostActionChoice => ({
  name: 'Paste here with variables preserved',
  id: PasteHereWithPropsPreservedPostActionChoiceId,
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
      },
    )
  },
})

export const PasteHereWithPropsReplacedPostActionChoiceId = 'post-here-action-choice-props-replaced'

export const PasteHereWithPropsReplacedPostActionChoice = (
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
    id: PasteHereWithPropsReplacedPostActionChoiceId,
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
