import { PostActionMenuData } from '../../../editor/store/editor-state'
import { pasteStrategyApply } from '../strategies/paste-metastrategy'
import { PostActionChoice } from './post-action-options'

export const PasteWithPropsPreservedPostActionChoice = (
  postActionMenuData: PostActionMenuData,
): PostActionChoice => ({
  name: 'Paste with variables preserved',
  id: 'post-action-choice-props-preserved',
  run: (store, builtInDependencies) =>
    pasteStrategyApply(
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

export const PasteWithPropsReplacedPostActionChoice = (
  postActionMenuData: PostActionMenuData,
): PostActionChoice => ({
  name: 'Paste with variables replaced',
  id: 'post-action-choice-props-reserved',
  run: (store, builtInDependencies) =>
    pasteStrategyApply(
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
