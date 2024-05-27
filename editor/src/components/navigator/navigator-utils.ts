import type { ElementPath } from '../../core/shared/project-file-types'
import * as EP from '../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSExpression,
  JSXConditionalExpression,
  JSXElement,
  JSXElementChild,
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
  RegularNavigatorEntry,
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
  condensedNavigatorRow,
  getEntriesForRow,
  regularNavigatorRow,
  type CondensedNavigatorRow,
  type NavigatorRow,
  type RegularNavigatorRow,
} from './navigator-row'
import { dropNulls, mapDropNulls } from '../../core/shared/array-utils'
import invariant from '../../third-party/remix/invariant'
import { getUtopiaID } from '../../core/shared/uid-utils'
import { create } from 'tar'
import { emptySet } from '../../core/shared/set-utils'
import { objectMap } from '../../core/shared/object-utils'

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

type RegularNavigatorTree = {
  type: 'regular-entry'
  elementHidden: boolean
  subtreeHidden: boolean
  navigatorEntry: NavigatorEntry
  renderProps: { [propName: string]: NavigatorTree }
  children: Array<NavigatorTree>
}

// maybe the leaf is not actually useful and we can remove it
type LeafNavigatorTree = {
  type: 'leaf-entry'
  elementHidden: boolean
  navigatorEntry: NavigatorEntry
}

type MapNavigatorTree = {
  type: 'map-entry'
  elementHidden: boolean
  subtreeHidden: boolean
  navigatorEntry: NavigatorEntry
  mappedEntries: Array<NavigatorTree>
}

type ConditionalNavigatorTree = {
  type: 'conditional-entry'
  elementHidden: boolean
  subtreeHidden: boolean
  navigatorEntry: NavigatorEntry
  trueCase: Array<NavigatorTree>
  falseCase: Array<NavigatorTree>
}

type CondensedTrunkNavigatorTree = {
  type: 'condensed-trunk'
  elementHidden: boolean
  subtreeHidden: boolean
  navigatorEntry: NavigatorEntry
  child: NavigatorTree
}

type CondensedLeafNavigatorTree = {
  type: 'condensed-leaf'
  navigatorEntry: NavigatorEntry
  children: Array<NavigatorEntry>
}

export type NavigatorTree =
  | RegularNavigatorTree
  | LeafNavigatorTree
  | MapNavigatorTree
  | ConditionalNavigatorTree
  | CondensedTrunkNavigatorTree
  | CondensedLeafNavigatorTree

function isSubtreeHidden(navigatorTree: NavigatorTree): boolean {
  return 'subtreeHidden' in navigatorTree && navigatorTree.subtreeHidden
}

function isElementHidden(navigatorTree: NavigatorTree): boolean {
  return 'elementHidden' in navigatorTree && navigatorTree.elementHidden
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
  const canvasRoots = getCanvasRoots(elementPathTree)
  const navigatorTrees: Array<NavigatorTree> = mapDropNulls((canvasRoot) => {
    const subTree = getSubTree(elementPathTree, canvasRoot.path)
    if (subTree == null) {
      return null
    }
    return createNavigatorSubtree(
      metadata,
      elementPathTree,
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
      elementHidden: false,
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

  const subtreeHidden = isCollapsed
  const elementHidden = isHiddenInNavigator || elementTypeHidden

  if (
    elementIsDataReferenceFromProjectContents &&
    isFeatureEnabled('Condensed Navigator Entries')
  ) {
    // add synthetic entry
    const dataRefEntry = dataReferenceNavigatorEntry(
      elementPath,
      renderedAtChildNode(EP.parentPath(elementPath), EP.toUid(elementPath)),
      EP.parentPath(elementPath),
      jsxElementChild,
    )
    return { type: 'leaf-entry', elementHidden: elementHidden, navigatorEntry: dataRefEntry }
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
      elementHidden,
      subtreeHidden,
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
      elementHidden,
      subtreeHidden,
    )
  }

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
    elementHidden,
    subtreeHidden,
  )
}

function walkRegularNavigatorEntry(
  metadata: ElementInstanceMetadataMap,
  elementPathTrees: ElementPathTrees,
  projectContents: ProjectContentTreeRoot,
  propertyControlsInfo: PropertyControlsInfo,
  collapsedViews: Array<ElementPath>,
  hiddenInNavigator: Array<ElementPath>,
  subTree: ElementPathTree,
  jsxElement: JSXElementChild,
  propControls: PropertyControls | null, // TODO this is redundant, we should be able to get this from propertyControlsInfo: PropertyControlsInfo,
  elementPath: ElementPath,
  elementHidden: boolean,
  subtreeHidden: boolean,
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
          elementHidden: false,
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
        const childTreeEntry = createNavigatorSubtree(
          metadata,
          elementPathTrees,
          projectContents,
          propertyControlsInfo,
          collapsedViews,
          hiddenInNavigator,
          subTreeChild,
        )
        const childTree: NavigatorTree = {
          ...childTreeEntry,
          navigatorEntry: renderPropValueNavigatorEntry(childPath, prop),
        }
        processedAccumulator.add(EP.toString(subTreeChild.path))
        renderPropChildrenAccumulator[prop] = childTree
      } else {
        const synthEntry = isFeatureEnabled('Condensed Navigator Entries')
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
          elementHidden: false,
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
    elementHidden: elementHidden,
    subtreeHidden: subtreeHidden,
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
  elementHidden: boolean,
  subtreeHidden: boolean,
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
    elementHidden: elementHidden,
    subtreeHidden: subtreeHidden,
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
  return [
    {
      type: 'leaf-entry',
      elementHidden: false,
      navigatorEntry: syntheticNavigatorEntry(clausePath, clauseValue),
    },
  ]
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
  elementHidden: boolean,
  subtreeHidden: boolean,
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
      invalidEntries.push({ type: 'leaf-entry', elementHidden: false, navigatorEntry: entry })
    }
    return invalidEntries
  })()

  return {
    type: 'map-entry',
    navigatorEntry: regularNavigatorEntry(subTree.path),
    mappedEntries: [...mappedChildren, ...invaldiOverrideEntries],
    elementHidden: elementHidden,
    subtreeHidden: subtreeHidden,
  }
}

function isCondensableLeafEntry(entry: NavigatorTree): boolean {
  // for now filter only for Data Entries
  return (
    entry.navigatorEntry.type === 'DATA_REFERENCE' &&
    // the entry is either a dedicated leaf entry
    (entry.type === 'leaf-entry' ||
      // or regular entry but has no children and no render props
      (entry.type === 'regular-entry' &&
        entry.children.length === 0 &&
        Object.values(entry.renderProps).length === 0))
  )
}

function condenseNavigatorTree(navigatorTree: Array<NavigatorTree>): Array<NavigatorTree> {
  if (!isFeatureEnabled('Condensed Navigator Entries')) {
    return navigatorTree
  }

  function walkSubtreeMaybeCondense(entry: NavigatorTree): NavigatorTree {
    // if the entry only has leaf children, we can turn it into a condensed leaf
    if (
      entry.type === 'regular-entry' &&
      entry.children.length > 0 &&
      entry.children.every(isCondensableLeafEntry)
    ) {
      return {
        type: 'condensed-leaf',
        navigatorEntry: entry.navigatorEntry,
        children: entry.children.map((c) => c.navigatorEntry),
      }
    }

    // if the entry only has a single child, we can condense it
    if (entry.type === 'regular-entry' && entry.children.length === 1) {
      return {
        type: 'condensed-trunk',
        navigatorEntry: entry.navigatorEntry,
        elementHidden: entry.elementHidden,
        subtreeHidden: entry.subtreeHidden,
        child: walkSubtreeMaybeCondense(entry.children[0]),
      }
    }

    //  we need to recurse into the subtrees here!
    switch (entry.type) {
      case 'regular-entry': {
        return {
          type: 'regular-entry',
          navigatorEntry: entry.navigatorEntry,
          renderProps: objectMap(
            (renderPropChild) => walkSubtreeMaybeCondense(renderPropChild),
            entry.renderProps,
          ),
          children: entry.children.map(walkSubtreeMaybeCondense),
          elementHidden: entry.elementHidden,
          subtreeHidden: entry.subtreeHidden,
        }
      }
      case 'leaf-entry':
        return entry
      case 'map-entry': {
        return {
          type: 'map-entry',
          navigatorEntry: entry.navigatorEntry,
          mappedEntries: entry.mappedEntries.map(walkSubtreeMaybeCondense),
          elementHidden: entry.elementHidden,
          subtreeHidden: entry.subtreeHidden,
        }
      }
      case 'conditional-entry': {
        return {
          type: 'conditional-entry',
          navigatorEntry: entry.navigatorEntry,
          trueCase: entry.trueCase.map(walkSubtreeMaybeCondense),
          falseCase: entry.falseCase.map(walkSubtreeMaybeCondense),
          elementHidden: entry.elementHidden,
          subtreeHidden: entry.subtreeHidden,
        }
      }
      case 'condensed-trunk': {
        return {
          type: 'condensed-trunk',
          elementHidden: entry.elementHidden,
          subtreeHidden: entry.subtreeHidden,
          navigatorEntry: entry.navigatorEntry,
          child: walkSubtreeMaybeCondense(entry.child),
        }
      }
      case 'condensed-leaf':
        return entry
      default:
        assertNever(entry)
    }
  }

  return navigatorTree.map(walkSubtreeMaybeCondense)
}

function flattenCondensedTrunk(entry: CondensedTrunkNavigatorTree): {
  singleRow: Array<NavigatorEntry>
  child: NavigatorTree
} {
  const singleRow = [entry.navigatorEntry]
  let currentChild: NavigatorTree = entry.child
  while (currentChild.type === 'condensed-trunk') {
    singleRow.push(currentChild.navigatorEntry)
    currentChild = currentChild.child
  }
  return { singleRow: singleRow, child: currentChild }
}

function getNavigatorRowsForTree(
  navigatorTree: Array<NavigatorTree>,
  filterVisible: 'all-navigator-targets' | 'visible-navigator-targets',
): Array<NavigatorRow> {
  const condensedTree = condenseNavigatorTree(navigatorTree)

  function walkTree(entry: NavigatorTree, indentation: number): Array<NavigatorRow> {
    function walkIfSubtreeVisible(e: NavigatorTree, i: number): Array<NavigatorRow> {
      if (isSubtreeHidden(entry) && filterVisible === 'visible-navigator-targets') {
        return []
      }
      return walkTree(e, i)
    }

    if (isElementHidden(entry) && filterVisible === 'visible-navigator-targets') {
      return []
    }

    switch (entry.type) {
      case 'condensed-trunk': {
        const { singleRow, child } = flattenCondensedTrunk(entry)
        if (singleRow.length === 1) {
          return [
            regularNavigatorRow(singleRow[0], indentation),
            ...walkIfSubtreeVisible(child, indentation + 1),
          ]
        }
        return [
          condensedNavigatorRow(singleRow, 'trunk', indentation),
          ...walkIfSubtreeVisible(child, indentation + 1),
        ]
      }
      case 'condensed-leaf':
        return [
          condensedNavigatorRow(
            [
              entry.navigatorEntry, // maybe we want to separate this to two rows, one for the label and one for the children
              ...entry.children,
            ],
            'leaf',
            indentation,
          ),
        ]
      case 'regular-entry': {
        const path = entry.navigatorEntry.elementPath
        const showChildrenLabel =
          Object.values(entry.renderProps).length > 0 && entry.children.length > 0
        return [
          regularNavigatorRow(entry.navigatorEntry, indentation),
          ...Object.entries(entry.renderProps).flatMap(([propName, renderPropChild]) => {
            const fakeRenderPropPath = EP.appendToPath(path, renderPropId(propName))
            return [
              regularNavigatorRow(
                renderPropNavigatorEntry(
                  fakeRenderPropPath,
                  propName,
                  renderPropChild.navigatorEntry.elementPath,
                ),
                indentation + 1,
              ),
              ...walkIfSubtreeVisible(renderPropChild, indentation + 2),
            ]
          }),
          ...(showChildrenLabel
            ? // we only show the children label if there are render props
              [
                regularNavigatorRow(
                  renderPropNavigatorEntry(
                    EP.appendToPath(path, renderPropId('children')),
                    'children',
                    entry.children[0].navigatorEntry.elementPath, // pick the first child path
                  ),
                  indentation + 1,
                ),
              ]
            : []),
          ...entry.children.flatMap((t) =>
            walkIfSubtreeVisible(t, showChildrenLabel ? indentation + 2 : indentation + 1),
          ),
        ]
      }
      case 'leaf-entry':
        return [regularNavigatorRow(entry.navigatorEntry, indentation)]
      case 'map-entry':
        return [
          regularNavigatorRow(entry.navigatorEntry, indentation),
          ...entry.mappedEntries.flatMap((t) => walkIfSubtreeVisible(t, indentation + 1)),
        ]
      case 'conditional-entry':
        return dropNulls([
          regularNavigatorRow(entry.navigatorEntry, indentation),
          entry.subtreeHidden
            ? null
            : regularNavigatorRow(
                conditionalClauseNavigatorEntry(entry.navigatorEntry.elementPath, 'true-case'),
                indentation + 1,
              ),
          ...entry.trueCase.flatMap((t) => walkIfSubtreeVisible(t, indentation + 2)),
          entry.subtreeHidden
            ? null
            : regularNavigatorRow(
                conditionalClauseNavigatorEntry(entry.navigatorEntry.elementPath, 'false-case'),
                indentation + 1,
              ),
          ...entry.falseCase.flatMap((t) => walkIfSubtreeVisible(t, indentation + 2)),
        ])
      default:
        assertNever(entry)
    }
  }

  return condensedTree.flatMap((t) => walkTree(t, 0))
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

  const navigatorRows = getNavigatorRowsForTree(navigatorTrees, 'all-navigator-targets')
  const navigatorTargets = navigatorRows.flatMap(getEntriesForRow)

  const visibleNavigatorRows = getNavigatorRowsForTree(navigatorTrees, 'visible-navigator-targets')
  const filteredVisibleNavigatorRows = visibleNavigatorRows
  const visibleNavigatorTargets = filteredVisibleNavigatorRows.flatMap(getEntriesForRow)

  return {
    navigatorRows: filteredVisibleNavigatorRows,
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
