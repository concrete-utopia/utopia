import type { ProjectContentTreeRoot } from '../../../../components/assets'
import type { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { elementPathFromInsertionPath } from '../../../../core/model/element-template-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { foldEither, isRight } from '../../../../core/shared/either'
import { generateUidWithExistingComponents } from '../../../../core/model/element-template-utils'
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
  AllElementProps,
  DerivedState,
  EditorState,
  NavigatorReparentPostActionMenuData,
} from '../../../editor/store/editor-state'
import { getInsertionPath } from '../../../editor/store/insertion-path'
import type { CanvasCommand } from '../../commands/commands'
import { showToastCommand } from '../../commands/show-toast-command'
import { allowGroupTrueUp, treatElementAsGroupLike } from '../strategies/group-helpers'
import {
  absolutePositionForReparent,
  replaceJSXElementCopyData,
} from '../strategies/reparent-helpers/reparent-helpers'
import { pathToReparent } from '../strategies/reparent-utils'
import type { PostActionChoice } from './post-action-options'
import type { ElementOrPathToInsert, OldPathToNewPathMapping } from './post-action-paste'
import { staticReparentAndUpdatePosition } from './post-action-paste'
import {
  emptyImports,
  emptyImportsMergeResolution,
} from '../../../../core/workers/common/project-file-utils'

function getNavigatorReparentCommands(
  data: NavigatorReparentPostActionMenuData,
  editor: EditorState,
  builtInDependencies: BuiltInDependencies,
): Array<CanvasCommand> | null {
  const wrapperUID = generateUidWithExistingComponents(editor.projectContents)

  const newParentPath = getInsertionPath(
    data.targetParent,
    editor.projectContents,
    editor.jsxMetadata,
    editor.elementPathTree,
    wrapperUID,
    data.dragSources.length,
    editor.propertyControlsInfo,
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
      'keep-visible-position',
    )
    const intendedCoordinates = adjustIntendedCoordinatesForGroups(
      editor.jsxMetadata,
      data.targetParent,
      intendedCoordinatesWithoutGroups,
      MetadataUtils.findElementByElementPath(editor.jsxMetadata, path),
    )
    return {
      elementPath: path,
      pathToReparent: pathToReparent(path),
      intendedCoordinates: intendedCoordinates,
      newUID: EP.toUid(path),
    }
  })

  const oldPathToNewPathMapping: OldPathToNewPathMapping = {}

  for (const value of data.dragSources) {
    const childrenPaths = MetadataUtils.getChildrenPathsOrdered(editor.elementPathTree, value)

    const pathAfterReparent = elementPathFromInsertionPath(newParentPath, EP.toUid(value))

    for (const path of [value, ...childrenPaths]) {
      oldPathToNewPathMapping[EP.toString(path)] =
        EP.replaceIfAncestor(path, value, pathAfterReparent) ?? undefined
    }
  }

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
      originalAllElementProps: editor.allElementProps,
    },
    elementsToReparent,
    data.indexPosition,
    oldPathToNewPathMapping,
  )
}

export const PropsPreservedNavigatorReparentPostActionChoiceId =
  'props-preserved-navigator-reparent-post-action'

export const PropsPreservedNavigatorReparentPostActionChoice = (
  data: NavigatorReparentPostActionMenuData,
): PostActionChoice => ({
  name: 'Reparent with variables preserved',
  id: PropsPreservedNavigatorReparentPostActionChoiceId,
  run: (editor, derived, builtInDependencies) =>
    getNavigatorReparentCommands(data, editor, builtInDependencies),
})

export const PropsReplacedNavigatorReparentPostActionChoiceId =
  'props-replaced-navigator-reparent-post-action'

export const PropsReplacedNavigatorReparentPostActionChoice = (
  data: NavigatorReparentPostActionMenuData,
): PostActionChoice | null => {
  const elements: Array<ElementPaste> = mapDropNulls((target) => {
    const metadata = MetadataUtils.findElementByElementPath(data.jsxMetadata, target)
    if (metadata != null) {
      return foldEither(
        (_) => null,
        (element) => ({
          element: element,
          originalElementPath: target,
          importsToAdd: emptyImports(),
        }),
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
    id: PropsReplacedNavigatorReparentPostActionChoiceId,
    run: (editor, derived, builtInDependencies) => {
      const reparentCommands = getNavigatorReparentCommands(data, editor, builtInDependencies)
      if (reparentCommands == null) {
        return null
      }
      return [...replacePropsCommands, ...reparentCommands]
    },
  }
}

export function adjustIntendedCoordinatesForGroups(
  jsxMetadata: ElementInstanceMetadataMap,
  reparentTargetPath: ElementPath,
  intendedCoordinates: CanvasPoint,
  element: ElementInstanceMetadata | null,
): CanvasPoint {
  const reparentTargetParentIsGroup = treatElementAsGroupLike(jsxMetadata, reparentTargetPath)
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
  projectContents: ProjectContentTreeRoot,
  jsxMetadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  reparentTargetPath: ElementPath,
  newPath: ElementPath,
  oldPath: ElementPath,
): Array<ElementPath> {
  const reparentTargetParentIsGroup = allowGroupTrueUp(
    projectContents,
    jsxMetadata,
    pathTrees,
    allElementProps,
    reparentTargetPath,
  )

  let paths: Array<ElementPath> = []
  if (reparentTargetParentIsGroup) {
    // the reparent target is a group, so true up using the new path of the reparented element
    paths.push(EP.appendToPath(reparentTargetPath, EP.toUid(newPath)))
  }

  const maybeElementAncestorGroup =
    EP.getAncestors(oldPath).find((path) => {
      return allowGroupTrueUp(projectContents, jsxMetadata, pathTrees, allElementProps, path)
    }) ?? null
  if (maybeElementAncestorGroup != null) {
    // the reparented element comes out of a group, so true up the group by its elements
    const groupChildren = MetadataUtils.getChildrenPathsOrdered(
      pathTrees,
      maybeElementAncestorGroup,
    )
    paths.push(...groupChildren.filter((child) => !EP.pathsEqual(oldPath, child)))
  }
  return paths
}
