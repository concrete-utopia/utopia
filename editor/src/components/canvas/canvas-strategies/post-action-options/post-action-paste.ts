import { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { getAllUniqueUids } from '../../../../core/model/get-unique-ids'
import { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { CanvasPoint } from '../../../../core/shared/math-utils'
import { ElementPath, NodeModules } from '../../../../core/shared/project-file-types'
import { fixUtopiaElement } from '../../../../core/shared/uid-utils'
import { ElementPasteWithMetadata, ReparentTargetForPaste } from '../../../../utils/clipboard'
import { absolute, front } from '../../../../utils/utils'
import { ProjectContentTreeRoot } from '../../../assets'
import { AllElementProps, PastePostActionMenuData } from '../../../editor/store/editor-state'
import { CanvasCommand, foldAndApplyCommandsInner } from '../../commands/commands'
import { updateFunctionCommand } from '../../commands/update-function-command'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import {
  absolutePositionForPaste,
  insertWithReparentStrategies,
} from '../strategies/reparent-helpers/reparent-helpers'
import {
  reparentStrategyForPaste,
  StaticReparentTarget,
} from '../strategies/reparent-helpers/reparent-strategy-helpers'
import { elementToReparent } from '../strategies/reparent-utils'
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
  canvasViewportCenter: CanvasPoint
}

function pasteChoiceCommon(
  target: ReparentTargetForPaste,
  editorStateContext: EditorStateContext,
  pasteContext: PasteContext,
): Array<CanvasCommand> | null {
  const elements = pasteContext.elementPasteWithMetadata.elements

  const strategy = reparentStrategyForPaste(
    editorStateContext.startingMetadata,
    editorStateContext.startingAllElementProps,
    editorStateContext.startingElementPathTrees,
    target.parentPath.intendedParentPath,
  )

  const commands = elements.flatMap((currentValue) => {
    return [
      updateFunctionCommand('always', (editor, commandLifecycle) => {
        const existingIDs = getAllUniqueUids(editor.projectContents).allIDs
        const elementWithUniqueUID = fixUtopiaElement(
          currentValue.element,
          new Set(existingIDs),
        ).value

        const reparentTarget: StaticReparentTarget =
          strategy === 'REPARENT_AS_ABSOLUTE'
            ? {
                type: strategy,
                insertionPath: target.parentPath,
                intendedCoordinates: absolutePositionForPaste(
                  target,
                  currentValue.originalElementPath,
                  elements.map((e) => e.originalElementPath),
                  {
                    originalTargetMetadata:
                      pasteContext.elementPasteWithMetadata.targetOriginalContextMetadata,
                    currentMetadata: editorStateContext.startingMetadata,
                    originalPathTrees: pasteContext.targetOriginalPathTrees,
                    currentPathTrees: editorStateContext.startingElementPathTrees,
                  },
                  pasteContext.canvasViewportCenter,
                ),
              }
            : { type: strategy, insertionPath: target.parentPath }

        const indexPosition =
          target.type === 'sibling'
            ? absolute(
                MetadataUtils.getIndexInParent(
                  editor.jsxMetadata,
                  editor.elementPathTree,
                  target.siblingPath,
                ) + 1,
              )
            : front()

        const result = insertWithReparentStrategies(
          editor,
          pasteContext.elementPasteWithMetadata.targetOriginalContextMetadata,
          pasteContext.targetOriginalPathTrees,
          reparentTarget,
          {
            elementPath: currentValue.originalElementPath,
            pathToReparent: elementToReparent(elementWithUniqueUID, currentValue.importsToAdd),
          },
          indexPosition,
          editorStateContext.builtInDependencies,
        )

        if (result == null) {
          return []
        }

        return foldAndApplyCommandsInner(
          editor,
          [],
          [
            ...result.commands,
            updateSelectedViews('always', [...editor.selectedViews, result.newPath]),
          ],
          commandLifecycle,
        ).statePatches
      }),
    ]
  })

  return [updateSelectedViews('always', []), ...commands]
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
      },
    ),
})

export const PasteWithPropsReplacedPostActionChoiceId = 'post-action-choice-props-replaced'

export const PasteWithPropsReplacedPostActionChoice = (
  postActionMenuData: PastePostActionMenuData,
): PostActionChoice => ({
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
        elementPasteWithMetadata: postActionMenuData.dataWithPropsReplaced,
        targetOriginalPathTrees: postActionMenuData.targetOriginalPathTrees,
        canvasViewportCenter: postActionMenuData.canvasViewportCenter,
      },
    ),
})
