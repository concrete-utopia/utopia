import type { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { foldEither } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../../core/shared/element-template'
import type { CanvasPoint } from '../../../../core/shared/math-utils'
import {
  canvasPoint,
  isFiniteRectangle,
  isNotNullFiniteRectangle,
  offsetPoint,
} from '../../../../core/shared/math-utils'
import type { ElementPath, id } from '../../../../core/shared/project-file-types'
import { filterMetadataForCopy } from '../../../../utils/clipboard'
import type { ElementPaste } from '../../../editor/action-types'
import type {
  EditorState,
  NavigatorReparentPostActionMenuData,
} from '../../../editor/store/editor-state'
import { getInsertionPathWithWrapWithFragmentBehavior } from '../../../editor/store/insertion-path'
import type { CanvasCommand } from '../../commands/commands'
import { showToastCommand } from '../../commands/show-toast-command'
import { treatElementAsGroupLike } from '../strategies/group-helpers'
import {
  absolutePositionForReparent,
  replaceJSXElementCopyData,
} from '../strategies/reparent-helpers/reparent-helpers'
import { pathToReparent } from '../strategies/reparent-utils'
import type { PostActionChoice } from './post-action-options'
import type { ElementOrPathToInsert } from './post-action-paste'
import { staticReparentAndUpdatePosition } from './post-action-paste'

function getNavigatorReparentCommands(
  data: NavigatorReparentPostActionMenuData,
  editor: EditorState,
  builtInDependencies: BuiltInDependencies,
): Array<CanvasCommand> | null {
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
        'navigator-reparent-cannot-reparent-under',
      ),
    ]
  }

  const elementsToReparent: Array<ElementOrPathToInsert> = data.dragSources.map((path) => {
    const intendedCoordinatesWithoutGroups = absolutePositionForReparent(
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
      data.canvasViewportCenter,
    )
    const intendedCoordinates = adjustIntendedCoordinatesForGroups(
      editor.jsxMetadata,
      editor.elementPathTree,
      data.targetParent,
      intendedCoordinatesWithoutGroups,
      MetadataUtils.findElementByElementPath(editor.jsxMetadata, path),
    )
    return {
      elementPath: path,
      pathToReparent: pathToReparent(path),
      intendedCoordinates: intendedCoordinates,
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
      canvasViewportCenter: data.canvasViewportCenter,
      reparentStrategy: null,
      insertionPosition: null,
    },
    elementsToReparent,
    data.indexPosition,
  )
}

export const NavigatorReparentPropsPreservedPostActionChoiceId =
  'navigator-reparent-post-action-props-preserved'

export const NavigatorReparentPropsPreservedPostActionChoice = (
  data: NavigatorReparentPostActionMenuData,
): PostActionChoice => ({
  name: 'Reparent with variables preserved',
  id: NavigatorReparentPropsPreservedPostActionChoiceId,
  run: (editor, derived, builtInDependencies) =>
    getNavigatorReparentCommands(data, editor, builtInDependencies),
})

export const NavigatorReparentPropsReplacedPostActionChoiceId =
  'navigator-reparent-post-action-props-replaced'

export const NavigatorReparentPropsReplacedPostActionChoice = (
  data: NavigatorReparentPostActionMenuData,
): PostActionChoice | null => {
  const elements: Array<ElementPaste> = mapDropNulls((target) => {
    const metadata = MetadataUtils.findElementByElementPath(data.jsxMetadata, target)
    if (metadata != null) {
      return foldEither(
        (_) => null,
        (element) => ({ element: element, originalElementPath: target, importsToAdd: {} }),
        metadata.element,
      )
    }
    return null
  }, data.dragSources)
  const replacePropsCommands = replaceJSXElementCopyData(
    {
      elements: elements,
      targetOriginalContextMetadata: filterMetadataForCopy(data.dragSources, data.jsxMetadata),
    },
    data.allElementProps,
  )?.replacePropCommands

  if (replacePropsCommands == null) {
    return null
  }
  return {
    name: 'Reparent with variables replaced',
    id: NavigatorReparentPropsReplacedPostActionChoiceId,
    run: (editor, derived, builtInDependencies) => {
      const reparentCommands = getNavigatorReparentCommands(data, editor, builtInDependencies)
      if (reparentCommands == null) {
        return null
      }
      return [...replacePropsCommands, ...reparentCommands]
    },
  }
}

function adjustIntendedCoordinatesForGroups(
  jsxMetadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  reparentTargetPath: ElementPath,
  intendedCoordinates: CanvasPoint,
  element: ElementInstanceMetadata | null,
): CanvasPoint {
  const reparentTargetParentIsGroup = treatElementAsGroupLike(
    jsxMetadata,
    pathTrees,
    reparentTargetPath,
  )
  const elementToInsertFrame = element?.globalFrame ?? null
  if (
    elementToInsertFrame != null &&
    isFiniteRectangle(elementToInsertFrame) &&
    reparentTargetParentIsGroup
  ) {
    const groupFrame =
      MetadataUtils.findElementByElementPath(jsxMetadata, reparentTargetPath)
        ?.specialSizeMeasurements.globalContentBoxForChildren ?? null
    if (isNotNullFiniteRectangle(groupFrame) && isNotNullFiniteRectangle(elementToInsertFrame)) {
      // adjust the position by removing any skew caused by the group boundaries
      return offsetPoint(elementToInsertFrame, canvasPoint({ x: -groupFrame.x, y: -groupFrame.y }))
    }
  }
  return intendedCoordinates
}

export function collectGroupTrueUp(
  jsxMetadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  reparentTargetPath: ElementPath,
  elementToInsert: ElementPath,
): Array<ElementPath> {
  const reparentTargetParentIsGroup = treatElementAsGroupLike(
    jsxMetadata,
    pathTrees,
    reparentTargetPath,
  )
  const maybeElementAncestorGroup =
    EP.getAncestors(elementToInsert).find((path) => {
      return treatElementAsGroupLike(jsxMetadata, pathTrees, path)
    }) ?? null

  let paths: Array<ElementPath> = []
  if (reparentTargetParentIsGroup) {
    // the reparent target is a group, so true up using the new path of the reparented element
    paths.push(EP.appendToPath(reparentTargetPath, EP.toUid(elementToInsert)))
  }

  if (maybeElementAncestorGroup != null) {
    // the reparented element comes out of a group, so true up the group by its elements
    const groupChildren = MetadataUtils.getChildrenPathsOrdered(
      jsxMetadata,
      pathTrees,
      maybeElementAncestorGroup,
    )
    paths.push(...groupChildren.filter((child) => !EP.pathsEqual(elementToInsert, child)))
  }
  return paths
}
