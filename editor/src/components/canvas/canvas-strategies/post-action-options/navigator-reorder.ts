import * as EP from '../../../../core/shared/element-path'
import { zeroCanvasPoint } from '../../../../core/shared/math-utils'
import type { NavigatorReorderPostActionMenuData } from '../../../editor/store/editor-state'
import { getInsertionPathWithWrapWithFragmentBehavior } from '../../../editor/store/insertion-path'
import { showToastCommand } from '../../commands/show-toast-command'
import { absolutePositionForReparent } from '../strategies/reparent-helpers/reparent-helpers'
import { pathToReparent } from '../strategies/reparent-utils'
import type { PostActionChoice } from './post-action-options'
import type { ElementOrPathToInsert } from './post-action-paste'
import { staticReparentAndUpdatePosition } from './post-action-paste'

export const NavigatorReorderPropsPreservedPostActionChoiceId =
  'navigator-reorder-post-action-props-preserved'

export const NavigatorReorderPropsPreservedPostActionChoice = (
  data: NavigatorReorderPostActionMenuData,
): PostActionChoice => ({
  name: 'Reparent with variables preserved',
  id: NavigatorReorderPropsPreservedPostActionChoiceId,
  run: (editor, derived, builtInDependencies) => {
    const newParentPath = getInsertionPathWithWrapWithFragmentBehavior(
      data.targetParent,
      editor.projectContents,
      editor.nodeModules.files,
      editor.canvas.openFile?.filename,
      editor.jsxMetadata,
      editor.elementPathTree,
    )

    if (newParentPath == null) {
      return [
        showToastCommand(
          'Cannot drop element here',
          'WARNING',
          'navigator-reoreder-cannot-reorder-under',
        ),
      ]
    }

    const elementsToReparent: Array<ElementOrPathToInsert> = data.dragSources.map((path) => {
      return {
        elementPath: path,
        pathToReparent: pathToReparent(path),
        intendedCoordinates: absolutePositionForReparent(
          path,
          data.dragSources,
          newParentPath.intendedParentPath,
          {
            originalTargetMetadata: editor.jsxMetadata,
            currentMetadata: editor.jsxMetadata,
            originalPathTrees: editor.elementPathTree,
            currentPathTrees: editor.elementPathTree,
          },
          editor.allElementProps,
          editor.elementPathTree,
          zeroCanvasPoint,
        ),
        uid: EP.toUid(path),
      }
    })

    return staticReparentAndUpdatePosition(
      { type: 'parent', parentPath: newParentPath },
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
          elements: [],
          targetOriginalContextMetadata: editor.jsxMetadata,
        },
        targetOriginalPathTrees: editor.elementPathTree,
        canvasViewportCenter: zeroCanvasPoint,
        reparentStrategy: null,
        insertionPosition: null,
      },
      elementsToReparent,
      data.indexPosition,
    )
  },
})

export const NavigatorReorderPropsReplacedPostActionChoiceId =
  'navigator-reorder-post-action-props-replaced'

export const NavigatorReorderPropsReplacedPostActionChoice = (
  data: NavigatorReorderPostActionMenuData,
): PostActionChoice | null => {
  // TODO REPLACE PROPS USING replaceJSXElementCopyData
  // IF NO REPLACE IS NEEDED RETURN NULL
  return {
    name: 'Reparent with variables replaced',
    id: NavigatorReorderPropsReplacedPostActionChoiceId,
    run: (editor, derived, builtInDependencies) => {
      return []
    },
  }
}
