import { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { getAllUniqueUids } from '../../../../core/model/get-unique-ids'
import { getStoryboardElementPath } from '../../../../core/model/scene-utils'
import { isLeft } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import {
  CanvasPoint,
  canvasPoint,
  canvasRectangle,
  offsetPoint,
  zeroCanvasPoint,
} from '../../../../core/shared/math-utils'
import { fixUtopiaElement } from '../../../../core/shared/uid-utils'
import { getTargetParentForPaste } from '../../../../utils/clipboard'
import { front } from '../../../../utils/utils'
import { ElementPaste } from '../../../editor/action-types'
import { PasteElementToInsert } from '../../../editor/actions/actions'
import {
  DerivedState,
  EditorState,
  PasteHerePostActionMenuData,
} from '../../../editor/store/editor-state'
import { childInsertionPath } from '../../../editor/store/insertion-path'
import { CanvasCommand, foldAndApplyCommandsInner } from '../../commands/commands'
import { showToastCommand } from '../../commands/show-toast-command'
import { updateFunctionCommand } from '../../commands/update-function-command'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import { offsetPositionInPasteBoundingBox } from '../strategies/reparent-helpers/reparent-helpers'
import {
  getReparentPropertyChanges,
  positionElementToCoordinatesCommands,
} from '../strategies/reparent-helpers/reparent-property-changes'
import { StaticReparentTarget } from '../strategies/reparent-helpers/reparent-strategy-helpers'
import { elementToReparent, getReparentOutcomeMultiselect } from '../strategies/reparent-utils'
import { PostActionChoice } from './post-action-options'

function pasteChoiceCommon(
  elementToPaste: Array<ElementPaste>,
  position: CanvasPoint,
  editor: EditorState,
  derived: DerivedState,
  builtInDependencies: BuiltInDependencies,
): Array<CanvasCommand> | null {
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
  if (isLeft(target)) {
    return [
      showToastCommand(
        'Paste element cannot find parent',
        'WARNING',
        'paste-elements-cannot-find-parent',
      ),
    ]
  }

  // parent targets can be the scene components root div, a scene/element directly on the canvas, or the storyboard
  const allPaths = [
    target.value.parentPath.intendedParentPath,
    ...EP.getAncestors(target.value.parentPath.intendedParentPath),
  ]
  const sceneComponentRoot = allPaths.find((path) =>
    derived.autoFocusedPaths.some((autofocused) => EP.pathsEqual(autofocused, EP.parentPath(path))),
  )
  const storyboardPath = getStoryboardElementPath(
    editor.projectContents,
    editor.canvas.openFile?.filename ?? null,
  )
  const storyboardChild = allPaths.find((path) =>
    EP.pathsEqual(storyboardPath, EP.parentPath(path)),
  )
  const targetParent = sceneComponentRoot ?? storyboardChild ?? storyboardPath

  if (targetParent == null) {
    return [
      showToastCommand(
        'Paste element cannot find parent, not even storyboard',
        'WARNING',
        'paste-elements-cannot-find-parent',
      ),
    ]
  }

  let fixedUIDMappingNewUIDS: Array<string> = []
  const pasteElementsToInsert: Array<PasteElementToInsert> = elementToPaste.map((elementPaste) => {
    const existingIDs = [
      ...getAllUniqueUids(editor.projectContents).allIDs,
      ...fixedUIDMappingNewUIDS,
    ]
    const elementWithUID = fixUtopiaElement(elementPaste.element, new Set(existingIDs))
    fixedUIDMappingNewUIDS.push(...elementWithUID.mappings.map((value) => value.newUID))

    const pointRelativeToNewParent = MetadataUtils.getFrameRelativeToTargetContainingBlock(
      targetParent,
      editor.jsxMetadata,
      canvasRectangle({ x: position.x, y: position.y, width: 0, height: 0 }),
    )

    const intendedCoordinates = offsetPoint(
      pointRelativeToNewParent != null
        ? canvasPoint({ x: pointRelativeToNewParent.x, y: pointRelativeToNewParent.y })
        : position,
      offsetPositionInPasteBoundingBox(
        elementPaste.originalElementPath,
        elementToPaste.map((element) => element.originalElementPath),
        originalMetadata,
      ),
    )

    return {
      elementPath: elementPaste.originalElementPath,
      pathToReparent: elementToReparent(elementWithUID.value, elementPaste.importsToAdd),
      intendedCoordinates: intendedCoordinates,
      uid: elementWithUID.value.uid,
    }
  })

  const reparentTarget: StaticReparentTarget = {
    type: 'REPARENT_AS_ABSOLUTE',
    insertionPath: childInsertionPath(targetParent),
  }

  const reparentCommands = getReparentOutcomeMultiselect(
    builtInDependencies,
    editor.projectContents,
    editor.nodeModules.files,
    editor.canvas.openFile?.filename,
    pasteElementsToInsert.map((e) => e.pathToReparent),
    reparentTarget.insertionPath,
    'always',
    front(),
  )

  if (reparentCommands == null) {
    return null
  }
  const commands = pasteElementsToInsert.flatMap((elementToInsert) => {
    return [
      updateFunctionCommand('always', (working, commandLifecycle) => {
        const newPath = working.canvas.controls.reparentedToPaths.find(
          (path) => EP.toUid(path) === elementToInsert.uid,
        )

        if (newPath == null) {
          return []
        }

        const pastedElementMetadata = MetadataUtils.findElementByElementPath(
          originalMetadata,
          elementToInsert.elementPath,
        )

        const propertyChangeCommands = getReparentPropertyChanges(
          'REPARENT_AS_ABSOLUTE',
          elementToInsert.elementPath,
          newPath,
          reparentTarget.insertionPath.intendedParentPath,
          originalMetadata,
          originalPathTree,
          working.jsxMetadata,
          working.elementPathTree,
          working.projectContents,
          working.canvas.openFile?.filename ?? null,
          pastedElementMetadata?.specialSizeMeasurements.position ?? null,
          pastedElementMetadata?.specialSizeMeasurements.display ?? null,
        )

        const absolutePositioningCommands = positionElementToCoordinatesCommands(
          newPath,
          elementToInsert.intendedCoordinates,
        )

        const propertyCommands = [...propertyChangeCommands, ...absolutePositioningCommands]

        return foldAndApplyCommandsInner(
          working,
          [],
          [...propertyCommands, updateSelectedViews('always', [...working.selectedViews, newPath])],
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
            $set: [],
          },
        },
      },
    }),
  ]
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
    return pasteChoiceCommon(elementToPaste, data.position, editor, derived, builtInDependencies)
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
      const elementToPaste = editor.internalClipboard.elements[0].copyDataWithPropsReplaced.elements
      return pasteChoiceCommon(elementToPaste, data.position, editor, derived, builtInDependencies)
    },
  }
}
