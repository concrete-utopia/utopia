import { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { getAllUniqueUids } from '../../../../core/model/get-unique-ids'
import * as EP from '../../../../core/shared/element-path'
import { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import {
  ElementInstanceMetadataMap,
  JSXElementChild,
} from '../../../../core/shared/element-template'
import { CanvasPoint } from '../../../../core/shared/math-utils'
import { ElementPath, NodeModules } from '../../../../core/shared/project-file-types'
import { fixUtopiaElement } from '../../../../core/shared/uid-utils'
import { assertNever } from '../../../../core/shared/utils'
import { ElementPasteWithMetadata, ReparentTargetForPaste } from '../../../../utils/clipboard'
import { absolute, front } from '../../../../utils/utils'
import { ProjectContentTreeRoot } from '../../../assets'
import { AllElementProps, PastePostActionMenuData } from '../../../editor/store/editor-state'
import { CanvasCommand, foldAndApplyCommandsInner } from '../../commands/commands'
import { updateFunctionCommand } from '../../commands/update-function-command'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import { absolutePositionForPaste } from '../strategies/reparent-helpers/reparent-helpers'
import {
  ElementPathLookup,
  getReparentPropertyChanges,
  positionElementToCoordinatesCommands,
} from '../strategies/reparent-helpers/reparent-property-changes'
import { reparentStrategyForPaste } from '../strategies/reparent-helpers/reparent-strategy-helpers'
import { elementToReparent, getReparentOutcomeMultiselect } from '../strategies/reparent-utils'
import { PostActionChoice } from './post-action-options'

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
  const elementsToInsert = pasteContext.elementPasteWithMetadata.elements.map((elementPaste) => {
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

    const intendedCoordinates = absolutePositionForPaste(
      target,
      elementPaste.originalElementPath,
      pasteContext.elementPasteWithMetadata.elements.map((element) => element.originalElementPath),
      {
        originalTargetMetadata: pasteContext.elementPasteWithMetadata.targetOriginalContextMetadata,
        originalPathTrees: pasteContext.targetOriginalPathTrees,
        currentMetadata: editorStateContext.startingMetadata,
        currentPathTrees: editorStateContext.startingElementPathTrees,
      },
      pasteContext.canvasViewportCenter,
    )

    return {
      elementPath: elementPaste.originalElementPath,
      pathToReparent: elementToReparent(elementWithUID.value, elementPaste.importsToAdd),
      intendedCoordinates: intendedCoordinates,
      newUID: elementWithUID.value.uid,
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

  const strategy = reparentStrategyForPaste(
    editorStateContext.startingMetadata,
    editorStateContext.startingAllElementProps,
    editorStateContext.startingElementPathTrees,
    target.parentPath.intendedParentPath,
  )

  const commands = elementsToInsert.flatMap((elementToInsert) => {
    return [
      updateFunctionCommand('always', (editor, commandLifecycle) => {
        const newPath = editor.canvas.controls.reparentedToPaths.find(
          (path) => EP.toUid(path) === elementToInsert.newUID,
        )

        const { oldUIDs, newUIDs } = oldUIDsToNewUidsLookup[elementToInsert.newUID]

        const childPathLookup: ElementPathLookup = zip(oldUIDs, newUIDs, (a, b) => ({
          oldUid: a,
          newUid: b,
        })).reduce((acc: ElementPathLookup, { oldUid, newUid }) => {
          const newPathForUid = editor.canvas.controls.reparentedToPaths.find(
            (path) => EP.toUid(path) === newUid,
          )

          if (newPathForUid == null) {
            throw new Error('UID should have a corresponding path in `reparentedPaths`')
          }

          return { ...acc, [oldUid]: newPathForUid }
        }, {})

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

export const PasteWithPropsPreservedPostActionChoiceId = 'post-action-choice-props-preserved'

export const PasteWithPropsPreservedPostActionChoice = (
  postActionMenuData: PastePostActionMenuData,
): PostActionChoice => ({
  name: 'Paste with variables preserved',
  id: PasteWithPropsPreservedPostActionChoiceId,
  run: (store, builtInDependencies) =>
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
    run: (store, builtInDependencies) =>
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
        },
      ),
  }
}

function getUidsFromJSXElementChild(element: JSXElementChild): string[] {
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
