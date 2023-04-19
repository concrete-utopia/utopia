import { ElementPath } from '../../core/shared/project-file-types'
import * as EP from '../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  isJSXConditionalExpression,
  JSXConditionalExpression,
} from '../../core/shared/element-template'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { foldEither, isLeft, isRight } from '../../core/shared/either'
import {
  ConditionalClauseNavigatorEntry,
  conditionalClauseNavigatorEntry,
  isConditionalClauseNavigatorEntry,
  NavigatorEntry,
  regularNavigatorEntry,
  syntheticNavigatorEntry,
} from '../editor/store/editor-state'
import {
  buildTree,
  ElementPathTree,
  ElementPathTreeRoot,
  getSubTree,
  reorderTree,
} from '../../core/shared/element-path-tree'
import { objectValues } from '../../core/shared/object-utils'
import { fastForEach } from '../../core/shared/utils'
import { ConditionalCase, getConditionalClausePath } from '../../core/model/conditionals'
import { UtopiaTheme } from '../../uuiui'

function baseNavigatorDepth(path: ElementPath): number {
  // The storyboard means that this starts at -1,
  // so that the scenes are the left most entity.
  return EP.fullDepth(path) - 1
}

export function navigatorDepth(
  navigatorEntry: NavigatorEntry,
  metadata: ElementInstanceMetadataMap,
): number {
  const path = navigatorEntry.elementPath
  let result: number = baseNavigatorDepth(path)
  for (const ancestorPath of EP.getAncestors(path)) {
    const elementMetadata = MetadataUtils.findElementByElementPath(metadata, ancestorPath)
    if (elementMetadata != null) {
      const isConditional = foldEither(
        () => false,
        (e) => isJSXConditionalExpression(e),
        elementMetadata.element,
      )
      if (isConditional) {
        result = result + 1
      }
    }
  }

  // For the clause entry itself, this needs to step back by 1.
  if (isConditionalClauseNavigatorEntry(navigatorEntry)) {
    result = result + 1
  }

  return result
}

interface GetNavigatorTargetsResults {
  navigatorTargets: Array<NavigatorEntry>
  visibleNavigatorTargets: Array<NavigatorEntry>
}

export function getNavigatorTargets(
  metadata: ElementInstanceMetadataMap,
  collapsedViews: Array<ElementPath>,
  hiddenInNavigator: Array<ElementPath>,
): GetNavigatorTargetsResults {
  // Note: This value will not necessarily be representative of the structured ordering in
  // the code that produced these elements, between siblings, as a result of it
  // relying on `metadata`, which has insertion ordering.
  const projectTree = buildTree(objectValues(metadata).map((m) => m.elementPath)).map((subTree) => {
    return reorderTree(subTree, metadata)
  })

  // This function exists separately from getAllPaths because the Navigator handles collapsed views
  let navigatorTargets: Array<NavigatorEntry> = []
  let visibleNavigatorTargets: Array<NavigatorEntry> = []

  function walkAndAddKeys(subTree: ElementPathTree | null, collapsedAncestor: boolean): void {
    if (subTree != null) {
      const path = subTree.path
      const isHiddenInNavigator = EP.containsPath(path, hiddenInNavigator)
      const isConditional = MetadataUtils.isElementPathConditionalFromMetadata(metadata, path)
      navigatorTargets.push(regularNavigatorEntry(path))
      if (
        !collapsedAncestor &&
        !isHiddenInNavigator &&
        !MetadataUtils.isElementTypeHiddenInNavigator(path, metadata)
      ) {
        visibleNavigatorTargets.push(regularNavigatorEntry(path))
      }

      const isCollapsed = EP.containsPath(path, collapsedViews)
      const newCollapsedAncestor = collapsedAncestor || isCollapsed || isHiddenInNavigator

      function walkSubTree(subTreeChildren: ElementPathTreeRoot): void {
        let unfurledComponents: Array<ElementPathTree> = []

        fastForEach(subTreeChildren, (child) => {
          if (EP.isRootElementOfInstance(child.path)) {
            unfurledComponents.push(child)
          } else {
            walkAndAddKeys(child, newCollapsedAncestor)
          }
        })

        fastForEach(unfurledComponents, (unfurledComponent) => {
          walkAndAddKeys(unfurledComponent, newCollapsedAncestor)
        })
      }

      function walkConditionalClause(
        conditionalSubTree: ElementPathTree,
        conditional: JSXConditionalExpression,
        conditionalCase: ConditionalCase,
      ): void {
        const clauseValue =
          conditionalCase === 'true-case' ? conditional.whenTrue : conditional.whenFalse

        function addNavigatorTargetUnlessCollapsed(entry: NavigatorEntry) {
          if (newCollapsedAncestor) {
            return
          }
          navigatorTargets.push(entry)
          visibleNavigatorTargets.push(entry)
        }

        // Get the clause path.
        const clausePath = getConditionalClausePath(path, clauseValue)

        // Create the entry for the name of the clause.
        const clauseTitleEntry = conditionalClauseNavigatorEntry(
          conditionalSubTree.path,
          conditionalCase,
        )
        addNavigatorTargetUnlessCollapsed(clauseTitleEntry)

        // Create the entry for the value of the clause.
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, clausePath)
        if (elementMetadata == null) {
          const clauseValueEntry = syntheticNavigatorEntry(clausePath, clauseValue)
          addNavigatorTargetUnlessCollapsed(clauseValueEntry)
        }

        // Walk the clause of the conditional.
        const clausePathTree = conditionalSubTree.children.find((childPath) => {
          return EP.pathsEqual(childPath.path, clausePath)
        })
        if (clausePathTree != null) {
          walkAndAddKeys(clausePathTree, newCollapsedAncestor)
        }
      }

      if (isConditional) {
        // Add in the additional elements for a conditional.
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
        if (
          elementMetadata != null &&
          isRight(elementMetadata.element) &&
          isJSXConditionalExpression(elementMetadata.element.value)
        ) {
          const jsxConditionalElement: JSXConditionalExpression = elementMetadata.element.value

          walkConditionalClause(subTree, jsxConditionalElement, 'true-case')
          walkConditionalClause(subTree, jsxConditionalElement, 'false-case')
        } else {
          throw new Error(`Unexpected non-conditional expression retrieved at ${EP.toString(path)}`)
        }
      } else {
        walkSubTree(subTree.children)
      }
    }
  }

  function getCanvasRoots(trees: ElementPathTree[]): ElementPath[] {
    if (projectTree.length <= 0) {
      return []
    }

    const storyboardTree = trees.find((e) => EP.isStoryboardPath(e.path))
    if (storyboardTree == null) {
      return []
    }

    return storyboardTree.children.map((c) => c.path)
  }

  const canvasRoots = getCanvasRoots(projectTree)
  fastForEach(canvasRoots, (childElement) => {
    const subTree = getSubTree(projectTree, childElement)

    walkAndAddKeys(subTree, false)
  })

  return {
    navigatorTargets: navigatorTargets,
    visibleNavigatorTargets: visibleNavigatorTargets,
  }
}

export function getConditionalClausePathForNavigatorEntry(
  navigatorEntry: ConditionalClauseNavigatorEntry,
  elementMetadata: ElementInstanceMetadata,
): ElementPath | null {
  if (elementMetadata == null) {
    return null
  }
  const element = elementMetadata.element
  if (isLeft(element)) {
    return null
  }
  const jsxElement = element.value
  if (!isJSXConditionalExpression(jsxElement)) {
    return null
  }
  const clauseElement =
    navigatorEntry.clause === 'true-case' ? jsxElement.whenTrue : jsxElement.whenFalse
  return getConditionalClausePath(navigatorEntry.elementPath, clauseElement)
}
