import type { ElementPath } from '../../core/shared/project-file-types'
import * as EP from '../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSXConditionalExpression,
} from '../../core/shared/element-template'
import {
  getJSXAttribute,
  hasElementsWithin,
  isJSXAttributesEntry,
  isJSXConditionalExpression,
  isJSXElement,
  isJSXMapExpression,
} from '../../core/shared/element-template'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { foldEither, isLeft, isRight } from '../../core/shared/either'
import type { ConditionalClauseNavigatorEntry, NavigatorEntry } from '../editor/store/editor-state'
import {
  conditionalClauseNavigatorEntry,
  invalidOverrideNavigatorEntry,
  isConditionalClauseNavigatorEntry,
  regularNavigatorEntry,
  renderPropNavigatorEntry,
  syntheticNavigatorEntry,
} from '../editor/store/editor-state'
import type { ElementPathTree, ElementPathTrees } from '../../core/shared/element-path-tree'
import { getCanvasRoots, getSubTree } from '../../core/shared/element-path-tree'
import { fastForEach } from '../../core/shared/utils'
import type { ConditionalCase } from '../../core/model/conditionals'
import { getConditionalClausePath } from '../../core/model/conditionals'
import { findUtopiaCommentFlag, isUtopiaCommentFlagMapCount } from '../../core/shared/comment-flags'
import { getPropertyControlsForTarget } from '../../core/property-controls/property-controls-utils'
import type { PropertyControlsInfo } from '../custom-code/code-file'
import type { ProjectContentTreeRoot } from '../assets'
import type { PropertyControls } from 'utopia-api/core'
import { isFeatureEnabled } from '../../utils/feature-switches'

export function baseNavigatorDepth(path: ElementPath): number {
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
  elementPathTree: ElementPathTrees,
  collapsedViews: Array<ElementPath>,
  hiddenInNavigator: Array<ElementPath>,
  propertyControlsInfo: PropertyControlsInfo,
  openFilePath: string | null,
  projectContents: ProjectContentTreeRoot,
): GetNavigatorTargetsResults {
  // Note: This value will not necessarily be representative of the structured ordering in
  // the code that produced these elements, between siblings, as a result of it
  // relying on `metadata`, which has insertion ordering.
  const projectTree = elementPathTree

  // This function exists separately from getAllPaths because the Navigator handles collapsed views
  let navigatorTargets: Array<NavigatorEntry> = []
  let visibleNavigatorTargets: Array<NavigatorEntry> = []

  function walkAndAddKeys(subTree: ElementPathTree | null, collapsedAncestor: boolean): void {
    if (subTree != null) {
      const path = subTree.path
      const isHiddenInNavigator = EP.containsPath(path, hiddenInNavigator)
      const isConditional = MetadataUtils.isElementPathConditionalFromMetadata(metadata, path)
      const isMap = MetadataUtils.isJSXMapExpression(path, metadata)
      // const isComponent = MetadataUtils.isComponentInstance(path, metadata)
      navigatorTargets.push(regularNavigatorEntry(path))
      if (
        !collapsedAncestor &&
        !isHiddenInNavigator &&
        !MetadataUtils.isElementTypeHiddenInNavigator(path, metadata, elementPathTree)
      ) {
        visibleNavigatorTargets.push(regularNavigatorEntry(path))
      }
      const processedAsRenderProp = new Set<string>()

      function walkPropertyControls(propControls: PropertyControls): void {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
        if (
          elementMetadata == null ||
          isLeft(elementMetadata.element) ||
          !isJSXElement(elementMetadata.element.value)
        ) {
          return
        }

        const jsxElement = elementMetadata.element.value
        let idx = 0
        const addedProps = new Set<string>()
        Object.values(jsxElement.props).forEach((attr) => {
          if (!isJSXAttributesEntry(attr)) {
            return
          }
          const prop = attr.key.toString()
          if (prop in propControls && propControls[prop].control === 'jsx') {
            const propValue = getJSXAttribute(jsxElement.props, prop)

            if (propValue?.type === 'ATTRIBUTE_OTHER_JAVASCRIPT') {
              addedProps.add(prop)
              const elementWithin = Object.values(propValue.elementsWithin)[0]
              if (elementWithin == null) {
                const entry = renderPropNavigatorEntry(
                  EP.appendToPath(path, propValue.uid),
                  prop,
                  propValue,
                  false,
                )
                navigatorTargets.push(entry)
                visibleNavigatorTargets.push(entry)
                return
              }
              const childPath = EP.appendToPath(path, EP.createIndexedUid(elementWithin.uid, ++idx))
              const navigatorEntry = renderPropNavigatorEntry(childPath, prop, propValue, true)
              navigatorTargets.push(navigatorEntry)
              visibleNavigatorTargets.push(navigatorEntry)

              const subTreeChild = subTree?.children.find((child) =>
                EP.pathsEqual(child.path, childPath),
              )
              if (subTreeChild != null) {
                processedAsRenderProp.add(EP.toString(subTreeChild.path))
                walkAndAddKeys(subTreeChild, collapsedAncestor)
              } else {
                const entry = syntheticNavigatorEntry(childPath, elementWithin)
                navigatorTargets.push(entry)
                visibleNavigatorTargets.push(entry)
              }
            }
          }
        })
        Object.entries(propControls).forEach(([prop, control]) => {
          if (addedProps.has(prop)) {
            return
          }
          if (control.control !== 'jsx') {
            return
          }
          const propValue = getJSXAttribute(jsxElement.props, prop)

          const navigatorEntry = renderPropNavigatorEntry(
            EP.appendToPath(path, propValue?.uid ?? 'fake-uid'),
            prop,
            propValue,
            false,
          )
          navigatorTargets.push(navigatorEntry)
          visibleNavigatorTargets.push(navigatorEntry)
        })
      }

      const propertyControls = getPropertyControlsForTarget(
        path,
        propertyControlsInfo,
        openFilePath,
        projectContents,
      )
      if (isFeatureEnabled('Render Props in Navigator') && propertyControls != null) {
        walkPropertyControls(propertyControls)
      }

      const isCollapsed = EP.containsPath(path, collapsedViews)
      const newCollapsedAncestor = collapsedAncestor || isCollapsed || isHiddenInNavigator

      function addNavigatorTargetUnlessCollapsed(entry: NavigatorEntry) {
        if (newCollapsedAncestor) {
          return
        }
        navigatorTargets.push(entry)
        visibleNavigatorTargets.push(entry)
      }

      function walkConditionalClause(
        conditionalSubTree: ElementPathTree,
        conditional: JSXConditionalExpression,
        conditionalCase: ConditionalCase,
      ): void {
        const clauseValue =
          conditionalCase === 'true-case' ? conditional.whenTrue : conditional.whenFalse

        // Get the clause path.
        const clausePath = getConditionalClausePath(path, clauseValue)

        // Create the entry for the name of the clause.
        const clauseTitleEntry = conditionalClauseNavigatorEntry(
          conditionalSubTree.path,
          conditionalCase,
        )
        addNavigatorTargetUnlessCollapsed(clauseTitleEntry)

        const isDynamic = (elementPath: ElementPath) => {
          return (
            MetadataUtils.isElementGenerated(metadata, elementPath) ||
            MetadataUtils.isGeneratedTextFromMetadata(elementPath, elementPathTree, metadata)
          )
        }

        const branch =
          conditionalCase === 'true-case' ? conditional.whenTrue : conditional.whenFalse

        // Walk the clause of the conditional.
        const clausePathTrees = Object.values(conditionalSubTree.children).filter((childPath) => {
          if (isDynamic(childPath.path) && hasElementsWithin(branch)) {
            for (const element of Object.values(branch.elementsWithin)) {
              const firstChildPath = EP.appendToPath(EP.parentPath(clausePath), element.uid)
              const containedElement = Object.values(metadata).find(({ elementPath }) => {
                return EP.pathsEqual(EP.dynamicPathToStaticPath(elementPath), firstChildPath)
              })
              if (containedElement != null) {
                return true
              }
            }
          }
          return EP.pathsEqual(childPath.path, clausePath)
        })
        if (clausePathTrees.length > 0) {
          clausePathTrees.map((t) => walkAndAddKeys(t, newCollapsedAncestor))
        }

        // Create the entry for the value of the clause.
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, clausePath)
        if (
          elementMetadata == null &&
          (clausePathTrees.length === 0 || !clausePathTrees.some((t) => isDynamic(t.path)))
        ) {
          const clauseValueEntry = syntheticNavigatorEntry(clausePath, clauseValue)
          addNavigatorTargetUnlessCollapsed(clauseValueEntry)
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
      } else if (isMap) {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
        if (
          elementMetadata != null &&
          isRight(elementMetadata.element) &&
          isJSXMapExpression(elementMetadata.element.value)
        ) {
          const element = elementMetadata.element.value
          const commentFlag = findUtopiaCommentFlag(element.comments, 'map-count')

          const mapCountOverride = isUtopiaCommentFlagMapCount(commentFlag)
            ? commentFlag.value
            : null
          fastForEach(Object.values(subTree.children), (child) => {
            walkAndAddKeys(child, newCollapsedAncestor)
          })
          if (mapCountOverride != null) {
            for (let i = Object.values(subTree.children).length; i < mapCountOverride; i++) {
              const entry = invalidOverrideNavigatorEntry(
                EP.appendToPath(path, `invalid-override-${i + 1}`),
                'data source not found',
              )
              addNavigatorTargetUnlessCollapsed(entry)
            }
          }
        }
      } else {
        fastForEach(Object.values(subTree.children), (child) => {
          if (!processedAsRenderProp.has(child.pathString)) {
            walkAndAddKeys(child, newCollapsedAncestor)
          }
        })
      }
    }
  }

  const canvasRoots = getCanvasRoots(projectTree)
  fastForEach(canvasRoots, (childElement) => {
    const subTree = getSubTree(projectTree, childElement.path)

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
