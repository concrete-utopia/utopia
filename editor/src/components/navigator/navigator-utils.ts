import type { ElementPath } from '../../core/shared/project-file-types'
import * as EP from '../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSExpression,
  JSXConditionalExpression,
  JSXElement,
  JSXFragment,
  JSXMapExpression,
} from '../../core/shared/element-template'
import {
  getJSXAttribute,
  hasElementsWithin,
  isJSExpressionValue,
  isJSXConditionalExpression,
  isJSXElement,
  isJSXElementLike,
  isJSXMapExpression,
} from '../../core/shared/element-template'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { foldEither, isLeft, isRight } from '../../core/shared/either'
import type {
  ConditionalClauseNavigatorEntry,
  DataReferenceNavigatorEntry,
  InvalidOverrideNavigatorEntry,
  NavigatorEntry,
  SlotNavigatorEntry,
  SyntheticNavigatorEntry,
} from '../editor/store/editor-state'
import {
  conditionalClauseNavigatorEntry,
  dataReferenceNavigatorEntry,
  getElementFromProjectContents,
  invalidOverrideNavigatorEntry,
  isConditionalClauseNavigatorEntry,
  regularNavigatorEntry,
  renderPropNavigatorEntry,
  renderPropValueNavigatorEntry,
  renderedAtChildNode,
  renderedAtPropertyPath,
  slotNavigatorEntry,
  syntheticNavigatorEntry,
} from '../editor/store/editor-state'
import type { ElementPathTree, ElementPathTrees } from '../../core/shared/element-path-tree'
import { getCanvasRoots, getSubTree } from '../../core/shared/element-path-tree'
import { assertNever, fastForEach } from '../../core/shared/utils'
import type { ConditionalCase } from '../../core/model/conditionals'
import { getConditionalClausePath } from '../../core/model/conditionals'
import { findUtopiaCommentFlag, isUtopiaCommentFlagMapCount } from '../../core/shared/comment-flags'
import { getPropertyControlsForTarget } from '../../core/property-controls/property-controls-utils'
import type { PropertyControlsInfo } from '../custom-code/code-file'
import type { ProjectContentTreeRoot } from '../assets'
import type { PropertyControls } from '../custom-code/internal-property-controls'
import { isFeatureEnabled } from '../../utils/feature-switches'
import * as PP from '../../core/shared/property-path'
import * as EPP from '../template-property-path'
import {
  getEntriesForRow,
  type CondensedNavigatorRow,
  type NavigatorRow,
  type RegularNavigatorRow,
} from './navigator-row'
import { mapDropNulls } from '../../core/shared/array-utils'
import invariant from '../../third-party/remix/invariant'
import { getUtopiaID } from '../../core/shared/uid-utils'
import { create } from 'tar'
import { emptySet } from '../../core/shared/set-utils'

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

export type NavigatorTree =
  | {
      type: 'regular-entry'
      subtreeHidden: boolean
      navigatorEntry: NavigatorEntry
      renderProps: { [propName: string]: NavigatorTree }
      children: Array<NavigatorTree>
    }
  | {
      // TODO not sure if leaf has to be a separate type, it could be a regular entry with no children, but it feels like it'll be useful
      type: 'leaf-entry'
      navigatorEntry:
        | DataReferenceNavigatorEntry
        | SyntheticNavigatorEntry
        | InvalidOverrideNavigatorEntry
        | SlotNavigatorEntry
    }
  | {
      type: 'map-entry'
      subtreeHidden: boolean
      navigatorEntry: NavigatorEntry
      mappedEntries: Array<NavigatorTree>
    }
  | {
      type: 'conditional-entry'
      subtreeHidden: boolean
      navigatorEntry: NavigatorEntry
      trueCase: Array<NavigatorTree>
      falseCase: Array<NavigatorTree>
    }

interface GetNavigatorTargetsResults {
  navigatorRows: Array<NavigatorRow>
  navigatorTargets: Array<NavigatorEntry>
  visibleNavigatorTargets: Array<NavigatorEntry>
}

export function getNavigatorTrees(
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  collapsedViews: Array<ElementPath>,
  hiddenInNavigator: Array<ElementPath>,
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
): Array<NavigatorTree> {
  // Note: This value will not necessarily be representative of the structured ordering in
  // the code that produced these elements, between siblings, as a result of it
  // relying on `metadata`, which has insertion ordering.
  const projectTree = elementPathTree

  // This function exists separately from getAllPaths because the Navigator handles collapsed views
  let navigatorTargets: Array<NavigatorEntry> = []
  let visibleNavigatorTargets: Array<NavigatorEntry> = []

  function walkAndAddKeys(
    subTree: ElementPathTree | null,
    collapsedAncestor: boolean,
    propName: string | null,
  ): void {
    if (subTree != null) {
      const path = subTree.path
      const isHiddenInNavigator = EP.containsPath(path, hiddenInNavigator)
      const isConditional = MetadataUtils.isElementPathConditionalFromMetadata(metadata, path)
      const isMap = MetadataUtils.isJSXMapExpression(path, metadata)
      const elementFromProjectContents = getElementFromProjectContents(path, projectContents)
      const elementIsDataReferenceFromProjectContents = MetadataUtils.isElementDataReference(
        elementFromProjectContents,
      )

      const isCollapsed = EP.containsPath(path, collapsedViews)
      const newCollapsedAncestor = collapsedAncestor || isCollapsed || isHiddenInNavigator

      function addNavigatorTargetsUnlessCollapsed(...entries: Array<NavigatorEntry>) {
        if (newCollapsedAncestor) {
          return
        }
        navigatorTargets.push(...entries)
        visibleNavigatorTargets.push(...entries)
      }

      if (
        elementIsDataReferenceFromProjectContents &&
        isFeatureEnabled('Data Entries in the Navigator')
      ) {
        if (elementFromProjectContents != null) {
          // add synthetic entry
          const dataRefEntry = dataReferenceNavigatorEntry(
            path,
            renderedAtChildNode(EP.parentPath(path), EP.toUid(path)),
            EP.parentPath(path),
            elementFromProjectContents,
          )
          addNavigatorTargetsUnlessCollapsed(dataRefEntry)
        } else {
          throw new Error(`internal error: Unexpected non-element found at ${EP.toString(path)}`)
        }
        return // early return!!
      }

      // const isComponent = MetadataUtils.isComponentInstance(path, metadata)
      const navigatorTarget =
        propName == null
          ? regularNavigatorEntry(path)
          : renderPropValueNavigatorEntry(path, propName)
      navigatorTargets.push(navigatorTarget)

      const elementTypeHidden = MetadataUtils.isElementTypeHiddenInNavigator(
        path,
        metadata,
        elementPathTree,
      )

      if (!collapsedAncestor && !isHiddenInNavigator && !elementTypeHidden) {
        visibleNavigatorTargets.push(navigatorTarget)
      }
      // We collect the paths which are shown in render props, so we can filter them out from regular
      // children to avoid duplications.
      const processedPathsAsRenderProp = new Set<string>()
      let renderPropFound: boolean = false

      function getPropValueChildPath(propValue: JSExpression) {
        return EP.appendToPath(path, propValue.uid)
      }

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

        Object.entries(propControls).forEach(([prop, control]) => {
          if (control.control !== 'jsx' || prop === 'children') {
            return
          }
          const propValue = getJSXAttribute(jsxElement.props, prop)
          renderPropFound = true
          const fakeRenderPropPath = EP.appendToPath(path, renderPropId(prop))

          if (propValue == null || (isJSExpressionValue(propValue) && propValue.value == null)) {
            const maybeChildPath = propValue != null ? getPropValueChildPath(propValue) : null
            const entries = [
              renderPropNavigatorEntry(fakeRenderPropPath, prop, maybeChildPath),
              slotNavigatorEntry(fakeRenderPropPath, prop),
            ]
            addNavigatorTargetsUnlessCollapsed(...entries)
            return
          }

          const childPath = getPropValueChildPath(propValue)
          const entry = renderPropNavigatorEntry(fakeRenderPropPath, prop, childPath)
          addNavigatorTargetsUnlessCollapsed(entry)

          const subTreeChild = subTree?.children.find((child) =>
            EP.pathsEqual(child.path, childPath),
          )
          if (subTreeChild != null) {
            processedPathsAsRenderProp.add(EP.toString(subTreeChild.path))
            walkAndAddKeys(subTreeChild, collapsedAncestor, prop)
          } else {
            const synthEntry = isFeatureEnabled('Data Entries in the Navigator')
              ? dataReferenceNavigatorEntry(
                  childPath,
                  renderedAtPropertyPath(EPP.create(path, PP.create(prop))),
                  path,
                  propValue,
                )
              : syntheticNavigatorEntry(childPath, propValue)
            addNavigatorTargetsUnlessCollapsed(synthEntry)
          }
        })
      }

      const propertyControls = getPropertyControlsForTarget(
        path,
        propertyControlsInfo,
        projectContents,
      )

      if (propertyControls != null) {
        walkPropertyControls(propertyControls)
      }

      function walkConditionalClause_(
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
        addNavigatorTargetsUnlessCollapsed(clauseTitleEntry)

        const isDynamic = (elementPath: ElementPath) => {
          return (
            MetadataUtils.isElementOrAncestorGenerated(elementPath) ||
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
          clausePathTrees.map((t) => walkAndAddKeys(t, newCollapsedAncestor, null))
        }

        // Create the entry for the value of the clause.
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, clausePath)
        if (
          elementMetadata == null &&
          (clausePathTrees.length === 0 || !clausePathTrees.some((t) => isDynamic(t.path)))
        ) {
          const clauseValueEntry = syntheticNavigatorEntry(clausePath, clauseValue)
          addNavigatorTargetsUnlessCollapsed(clauseValueEntry)
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

          walkConditionalClause_(subTree, jsxConditionalElement, 'true-case')
          walkConditionalClause_(subTree, jsxConditionalElement, 'false-case')
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
            walkAndAddKeys(child, newCollapsedAncestor, null)
          })
          if (mapCountOverride != null) {
            for (let i = Object.values(subTree.children).length; i < mapCountOverride; i++) {
              const entry = invalidOverrideNavigatorEntry(
                EP.appendToPath(path, `invalid-override-${i + 1}`),
                'data source not found',
              )
              addNavigatorTargetsUnlessCollapsed(entry)
            }
          }
        }
      } else if (
        // if the metadata doesn't contain children elements, we still want to look through the in-code children, maybe we find some data entries to show
        // ideally this should be done in the metadata, in the future this part should be removed
        subTree.children.length === 0 &&
        isFeatureEnabled('Data Entries in the Navigator')
      ) {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
        if (
          elementMetadata != null &&
          isRight(elementMetadata.element) &&
          isJSXElement(elementMetadata.element.value) &&
          elementMetadata.element.value.children.length > 0
        ) {
          const jsxElement = elementMetadata.element.value
          const children = jsxElement.children
          children.forEach((child) => {
            const childPath = EP.appendToPath(path, child.uid)
            const dataRefEntry = dataReferenceNavigatorEntry(
              childPath,
              renderedAtChildNode(path, child.uid),
              path,
              child,
            )
            addNavigatorTargetsUnlessCollapsed(dataRefEntry)
          })
        }
      } else {
        const notRenderPropChildren = Object.values(subTree.children).filter(
          (c) => !processedPathsAsRenderProp.has(c.pathString),
        )
        if (
          notRenderPropChildren.length > 0 &&
          renderPropFound // only show a dedicated label for the children prop if the component has render props too
        ) {
          const entry = renderPropNavigatorEntry(
            EP.appendToPath(path, renderPropId('children')),
            'children',
            notRenderPropChildren[0].path, // pick the first element path
          )
          addNavigatorTargetsUnlessCollapsed(entry)
        }
        fastForEach(notRenderPropChildren, (child) => {
          walkAndAddKeys(child, newCollapsedAncestor, null)
        })
      }
    }
  }

  const canvasRoots = getCanvasRoots(projectTree)
  const navigatorTrees: Array<NavigatorTree> = mapDropNulls((canvasRoot) => {
    const subTree = getSubTree(projectTree, canvasRoot.path)
    if (subTree == null) {
      return null
    }
    return createNavigatorSubtree(
      metadata,
      projectTree,
      projectContents,
      propertyControlsInfo,
      collapsedViews,
      hiddenInNavigator,
      subTree,
    )
  }, canvasRoots)

  return navigatorTrees
}

function createNavigatorSubtree(
  metadata: ElementInstanceMetadataMap,
  elementPathTrees: ElementPathTrees,
  projectContents: ProjectContentTreeRoot,
  propertyControlsInfo: PropertyControlsInfo,
  collapsedViews: Array<ElementPath>, // TODO turn this into a single array!!
  hiddenInNavigator: Array<ElementPath>,
  subTree: ElementPathTree,
): NavigatorTree {
  const elementPath = subTree.path
  const jsxElementChild = getElementFromProjectContents(elementPath, projectContents)
  if (jsxElementChild == null) {
    return {
      type: 'leaf-entry',
      navigatorEntry: invalidOverrideNavigatorEntry(
        subTree.path,
        'Element was not found in project contents.',
      ),
    }
  }

  const elementIsDataReferenceFromProjectContents =
    MetadataUtils.isElementDataReference(jsxElementChild)

  const isCollapsed = EP.containsPath(elementPath, collapsedViews)
  const isHiddenInNavigator = EP.containsPath(elementPath, hiddenInNavigator)

  const elementTypeHidden = MetadataUtils.isElementTypeHiddenInNavigator(
    elementPath,
    metadata,
    elementPathTrees,
  )

  const hidden = isCollapsed || isHiddenInNavigator || elementTypeHidden

  if (
    elementIsDataReferenceFromProjectContents &&
    isFeatureEnabled('Data Entries in the Navigator')
  ) {
    // add synthetic entry
    const dataRefEntry = dataReferenceNavigatorEntry(
      elementPath,
      renderedAtChildNode(EP.parentPath(elementPath), EP.toUid(elementPath)),
      EP.parentPath(elementPath),
      jsxElementChild,
    )
    return { type: 'leaf-entry', navigatorEntry: dataRefEntry }
  }

  if (isJSXElementLike(jsxElementChild)) {
    return walkRegularNavigatorEntry(
      metadata,
      elementPathTrees,
      projectContents,
      propertyControlsInfo,
      collapsedViews,
      hiddenInNavigator,
      subTree,
      jsxElementChild,
      getPropertyControlsForTarget(elementPath, propertyControlsInfo, projectContents),
      elementPath,
      hidden,
    )
  }

  if (isJSXConditionalExpression(jsxElementChild)) {
    return walkConditionalNavigatorEntry(
      metadata,
      elementPathTrees,
      projectContents,
      propertyControlsInfo,
      collapsedViews,
      hiddenInNavigator,
      subTree,
      jsxElementChild,
      elementPath,
      hidden,
    )
  }

  if (isJSXMapExpression(jsxElementChild)) {
    return walkMapExpression(
      metadata,
      elementPathTrees,
      projectContents,
      propertyControlsInfo,
      collapsedViews,
      hiddenInNavigator,
      subTree,
      jsxElementChild,
      hidden,
    )
  }

  if (!isFeatureEnabled('Data Entries in the Navigator')) {
    // fallback case for the FS off
    return {
      type: 'leaf-entry',
      navigatorEntry: syntheticNavigatorEntry(elementPath, jsxElementChild),
    }
  }

  throw new Error(`Unexpected element encountered in the Navigator: ${subTree.pathString}`)
}

function walkRegularNavigatorEntry(
  metadata: ElementInstanceMetadataMap,
  elementPathTrees: ElementPathTrees,
  projectContents: ProjectContentTreeRoot,
  propertyControlsInfo: PropertyControlsInfo,
  collapsedViews: Array<ElementPath>,
  hiddenInNavigator: Array<ElementPath>,
  subTree: ElementPathTree,
  jsxElement: JSXElement | JSXFragment,
  propControls: PropertyControls | null, // TODO this is redundant, we should be able to get this from propertyControlsInfo: PropertyControlsInfo,
  elementPath: ElementPath,
  hidden: boolean,
): NavigatorTree {
  let renderPropChildrenAccumulator: { [propName: string]: NavigatorTree } = {}
  let processedAccumulator: Set<string> = emptySet()

  if (isJSXElement(jsxElement)) {
    Object.entries(propControls ?? {}).forEach(([prop, control]) => {
      if (control.control !== 'jsx' || prop === 'children') {
        return
      }
      const propValue = getJSXAttribute(jsxElement.props, prop)
      const fakeRenderPropPath = EP.appendToPath(elementPath, renderPropId(prop))

      if (propValue == null || (isJSExpressionValue(propValue) && propValue.value == null)) {
        renderPropChildrenAccumulator[prop] = {
          type: 'leaf-entry',
          navigatorEntry: slotNavigatorEntry(
            fakeRenderPropPath, // TODO fakeRenderPropPath must be deleted
            prop,
          ),
        }
        return
      }

      const childPath = EP.appendToPath(elementPath, getUtopiaID(propValue))

      const subTreeChild = subTree?.children.find((child) => EP.pathsEqual(child.path, childPath))
      if (subTreeChild != null) {
        const childTree = createNavigatorSubtree(
          metadata,
          elementPathTrees,
          projectContents,
          propertyControlsInfo,
          collapsedViews,
          hiddenInNavigator,
          subTreeChild,
        )
        processedAccumulator.add(EP.toString(subTreeChild.path))
        renderPropChildrenAccumulator[prop] = childTree
      } else {
        const synthEntry = isFeatureEnabled('Data Entries in the Navigator')
          ? dataReferenceNavigatorEntry(
              childPath,
              renderedAtPropertyPath(EPP.create(elementPath, PP.create(prop))),
              elementPath,
              propValue,
            )
          : syntheticNavigatorEntry(childPath, propValue)

        processedAccumulator.add(EP.toString(childPath))
        renderPropChildrenAccumulator[prop] = {
          type: 'leaf-entry',
          navigatorEntry: synthEntry,
        }
      }
    })
  }

  const childrenPaths = subTree.children.filter(
    (child) => !processedAccumulator.has(EP.toString(child.path)),
  )
  const children: Array<NavigatorTree> = childrenPaths.map((child) =>
    createNavigatorSubtree(
      metadata,
      elementPathTrees,
      projectContents,
      propertyControlsInfo,
      collapsedViews,
      hiddenInNavigator,
      child,
    ),
  )

  return {
    type: 'regular-entry',
    navigatorEntry: regularNavigatorEntry(elementPath),
    renderProps: renderPropChildrenAccumulator,
    children: children,
    subtreeHidden: hidden,
  }
}

function walkConditionalNavigatorEntry(
  metadata: ElementInstanceMetadataMap,
  elementPathTrees: ElementPathTrees,
  projectContents: ProjectContentTreeRoot,
  propertyControlsInfo: PropertyControlsInfo,
  collapsedViews: Array<ElementPath>,
  hiddenInNavigator: Array<ElementPath>,
  subTree: ElementPathTree,
  jsxElement: JSXConditionalExpression,
  elementPath: ElementPath,
  hidden: boolean,
): NavigatorTree {
  const trueCase = walkConditionalClause(
    metadata,
    elementPathTrees,
    projectContents,
    propertyControlsInfo,
    collapsedViews,
    hiddenInNavigator,
    subTree,
    jsxElement,
    'true-case',
  )
  const falseCase = walkConditionalClause(
    metadata,
    elementPathTrees,
    projectContents,
    propertyControlsInfo,
    collapsedViews,
    hiddenInNavigator,
    subTree,
    jsxElement,
    'false-case',
  )

  return {
    type: 'conditional-entry',
    navigatorEntry: regularNavigatorEntry(elementPath),
    trueCase: trueCase,
    falseCase: falseCase,
    subtreeHidden: hidden,
  }
}

function walkConditionalClause(
  metadata: ElementInstanceMetadataMap,
  elementPathTrees: ElementPathTrees,
  projectContents: ProjectContentTreeRoot,
  propertyControlsInfo: PropertyControlsInfo,
  collapsedViews: Array<ElementPath>,
  hiddenInNavigator: Array<ElementPath>,
  conditionalSubTree: ElementPathTree,
  conditional: JSXConditionalExpression,
  conditionalCase: ConditionalCase,
): Array<NavigatorTree> {
  const isDynamic = (elementPath: ElementPath) => {
    return (
      MetadataUtils.isElementOrAncestorGenerated(elementPath) ||
      MetadataUtils.isGeneratedTextFromMetadata(elementPath, elementPathTrees, metadata)
    )
  }

  const path = conditionalSubTree.path
  const clauseValue = conditionalCase === 'true-case' ? conditional.whenTrue : conditional.whenFalse

  // Get the clause path.
  const clausePath = getConditionalClausePath(path, clauseValue)

  const branch = conditionalCase === 'true-case' ? conditional.whenTrue : conditional.whenFalse

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

  // if we find regular tree entries for the clause, it means the branch has proper JSXElements, so we recurse into the tree building
  if (clausePathTrees.length > 0) {
    const children = clausePathTrees.map((child) =>
      createNavigatorSubtree(
        metadata,
        elementPathTrees,
        projectContents,
        propertyControlsInfo,
        collapsedViews,
        hiddenInNavigator,
        child,
      ),
    )
    return children
  }

  // No children were found in the ElementPathTrees, so we create a synthetic entry for the value of the clause.
  return [{ type: 'leaf-entry', navigatorEntry: syntheticNavigatorEntry(clausePath, clauseValue) }]
}

function walkMapExpression(
  metadata: ElementInstanceMetadataMap,
  elementPathTrees: ElementPathTrees,
  projectContents: ProjectContentTreeRoot,
  propertyControlsInfo: PropertyControlsInfo,
  collapsedViews: Array<ElementPath>,
  hiddenInNavigator: Array<ElementPath>,
  subTree: ElementPathTree,
  element: JSXMapExpression,
  hidden: boolean,
): NavigatorTree {
  const commentFlag = findUtopiaCommentFlag(element.comments, 'map-count')

  const mapCountOverride = isUtopiaCommentFlagMapCount(commentFlag) ? commentFlag.value : null
  const mappedChildren = Object.values(subTree.children).map((child) =>
    createNavigatorSubtree(
      metadata,
      elementPathTrees,
      projectContents,
      propertyControlsInfo,
      collapsedViews,
      hiddenInNavigator,
      child,
    ),
  )

  const invaldiOverrideEntries = (() => {
    if (mapCountOverride == null) {
      return []
    }

    let invalidEntries: Array<NavigatorTree> = []
    for (let i = Object.values(subTree.children).length; i < mapCountOverride; i++) {
      const entry = invalidOverrideNavigatorEntry(
        EP.appendToPath(subTree.path, `invalid-override-${i + 1}`),
        'data source not found',
      )
      invalidEntries.push({ type: 'leaf-entry', navigatorEntry: entry })
    }
    return invalidEntries
  })()

  return {
    type: 'map-entry',
    navigatorEntry: regularNavigatorEntry(subTree.path),
    mappedEntries: [...mappedChildren, ...invaldiOverrideEntries],
    subtreeHidden: hidden,
  }
}

// TODO!! be able to filter to only visible
export function getMappedNavigatorRows(
  navigatorTree: Array<NavigatorTree>,
  filterVisible: 'all-navigator-targets' | 'visible-navigator-targets',
): Array<NavigatorRow> {
  function getNavigatorEntriesForMapEntry(entry: NavigatorTree): Array<NavigatorEntry> {
    if (
      filterVisible === 'visible-navigator-targets' &&
      'subtreeHidden' in entry &&
      entry.subtreeHidden
    ) {
      return [entry.navigatorEntry]
    }

    switch (entry.type) {
      case 'regular-entry': {
        const path = entry.navigatorEntry.elementPath
        return [
          entry.navigatorEntry,
          ...Object.entries(entry.renderProps).flatMap(([propName, renderPropChild]) => {
            const fakeRenderPropPath = EP.appendToPath(path, renderPropId(propName))
            return [
              renderPropNavigatorEntry(
                fakeRenderPropPath,
                propName,
                renderPropChild.navigatorEntry.elementPath,
              ),
              ...getNavigatorEntriesForMapEntry(renderPropChild),
            ]
          }),
          ...(Object.values(entry.renderProps).length > 0 && entry.children.length > 0
            ? // we only show the children label if there are render props
              [slotNavigatorEntry(EP.appendToPath(path, renderPropId('children')), 'children')]
            : []),
          ...entry.children.flatMap(getNavigatorEntriesForMapEntry),
        ]
      }
      case 'leaf-entry':
        return [entry.navigatorEntry]
      case 'map-entry':
        return [
          entry.navigatorEntry,
          ...entry.mappedEntries.flatMap(getNavigatorEntriesForMapEntry),
        ]
      case 'conditional-entry':
        return [
          entry.navigatorEntry,
          conditionalClauseNavigatorEntry(entry.navigatorEntry.elementPath, 'true-case'),
          ...entry.trueCase.flatMap(getNavigatorEntriesForMapEntry),
          conditionalClauseNavigatorEntry(entry.navigatorEntry.elementPath, 'false-case'),
          ...entry.falseCase.flatMap(getNavigatorEntriesForMapEntry),
        ]
      default:
        assertNever(entry)
    }
  }
  const navigatorTargets: Array<NavigatorEntry> = navigatorTree.flatMap(
    getNavigatorEntriesForMapEntry,
  )

  const regularRows = navigatorTargets.map(
    (target): NavigatorRow => ({
      type: 'regular-row',
      entry: target,
    }),
  )

  return regularRows
}

export function getNavigatorTargets(
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  collapsedViews: Array<ElementPath>,
  hiddenInNavigator: Array<ElementPath>,
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
): GetNavigatorTargetsResults {
  const navigatorTrees = getNavigatorTrees(
    metadata,
    elementPathTree,
    collapsedViews,
    hiddenInNavigator,
    propertyControlsInfo,
    projectContents,
  )

  const navigatorRows = getMappedNavigatorRows(navigatorTrees, 'all-navigator-targets')
  const visibleNavigatorRows = getMappedNavigatorRows(navigatorTrees, 'visible-navigator-targets')
  const navigatorTargets = navigatorRows.flatMap((row) => {
    return getEntriesForRow(row)
  })
  const visibleNavigatorTargets = visibleNavigatorRows.flatMap((row) => {
    return getEntriesForRow(row)
  })

  return {
    navigatorRows: visibleNavigatorRows,
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

function renderPropId(propName: string): string {
  return `prop-label-${propName}`
}
