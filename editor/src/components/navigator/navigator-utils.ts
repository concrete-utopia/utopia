import type { ElementPath } from '../../core/shared/project-file-types'
import * as EP from '../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSExpression,
  JSXConditionalExpression,
  JSXElement,
} from '../../core/shared/element-template'
import {
  getJSXAttribute,
  hasElementsWithin,
  isJSExpressionValue,
  isJSXConditionalExpression,
  isJSXElement,
  isJSXMapExpression,
} from '../../core/shared/element-template'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { foldEither, isLeft, isRight } from '../../core/shared/either'
import type {
  ConditionalClauseNavigatorEntry,
  DataReferenceNavigatorEntry,
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
      navigatorEntry: NavigatorEntry
      renderProps: { [propName: string]: NavigatorTree }
      children: Array<NavigatorTree>
      subtreeHidden: boolean
    }
  | {
      type: 'data-entry'
      navigatorEntry: DataReferenceNavigatorEntry | SyntheticNavigatorEntry // TODO remove SyntheticNavigatorEntry as an option from here
    }
  | {
      type: 'map-entry'
      mappedEntries: Array<NavigatorTree>
    }
  | {
      type: 'conditional-entry'
      target: NavigatorEntry
      trueCase: NavigatorTree
      falseCase: NavigatorTree
    }
  | {
      type: 'slot-entry'
      target: SlotNavigatorEntry
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
    return createNavigatorSubtree(metadata, projectTree, subTree)
  }, canvasRoots)

  return navigatorTrees
}

function createNavigatorSubtree(
  metadata: ElementInstanceMetadataMap,
  elementPathTrees: ElementPathTrees,
  subTree: ElementPathTree,
): NavigatorTree {
  // first, only be able to create regular entries
  const elementPath = subTree.path
  const elementMetadata = MetadataUtils.findElementByElementPath(metadata, elementPath)
  invariant(elementMetadata != null, 'Element metadata should not be null')
  const element = elementMetadata.element
  invariant(
    isRight(element),
    'Found an outdated string element in the metadata, we should remove those',
  )
  const jsxElementChild = element.value

  const elementIsDataReferenceFromProjectContents =
    MetadataUtils.isElementDataReference(jsxElementChild)

  // if element is data reference, return leaf
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
    return { type: 'data-entry', navigatorEntry: dataRefEntry }
  }

  if (isJSXElement(jsxElementChild)) {
    return walkRegularNavigatorEntry(
      metadata,
      elementPathTrees,
      subTree,
      jsxElementChild,
      getPropertyControlsForTarget(elementPath, {}, {}),
      elementPath,
      MetadataUtils.isElementTypeHiddenInNavigator(elementPath, metadata, elementPathTrees),
    )
  }
}

function walkRegularNavigatorEntry(
  metadata: ElementInstanceMetadataMap,
  elementPathTrees: ElementPathTrees,
  subTree: ElementPathTree,
  jsxElement: JSXElement,
  propControls: PropertyControls | null,
  elementPath: ElementPath,
  hidden: boolean, // maybe figure it out internally?
): NavigatorTree {
  let renderPropChildrenAccumulator: { [propName: string]: NavigatorTree } = {}
  let processedAccumulator: Set<string> = emptySet()

  Object.entries(propControls ?? {}).forEach(([prop, control]) => {
    if (control.control !== 'jsx' || prop === 'children') {
      return
    }
    const propValue = getJSXAttribute(jsxElement.props, prop)
    const fakeRenderPropPath = EP.appendToPath(elementPath, renderPropId(prop))

    if (propValue == null || (isJSExpressionValue(propValue) && propValue.value == null)) {
      renderPropChildrenAccumulator[prop] = {
        type: 'slot-entry',
        target: slotNavigatorEntry(
          fakeRenderPropPath, // TODO fakeRenderPropPath must be deleted
          prop,
        ),
      }
      return
    }

    const childPath = EP.appendToPath(elementPath, getUtopiaID(propValue))

    const subTreeChild = subTree?.children.find((child) => EP.pathsEqual(child.path, childPath))
    if (subTreeChild != null) {
      const childTree = createNavigatorSubtree(metadata, elementPathTrees, subTreeChild)
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
        type: 'data-entry',
        navigatorEntry: synthEntry,
      }
    }
  })

  const childrenPaths = subTree.children.filter(
    (child) => !processedAccumulator.has(EP.toString(child.path)),
  )
  const children: Array<NavigatorTree> = childrenPaths.map((child) =>
    createNavigatorSubtree(metadata, elementPathTrees, child),
  )

  return {
    type: 'regular-entry',
    navigatorEntry: regularNavigatorEntry(elementPath),
    renderProps: renderPropChildrenAccumulator,
    children: children,
    subtreeHidden: hidden,
  }
}

// TODO!! be able to filter to only visible
export function getMappedNavigatorRows(navigatorTree: Array<NavigatorTree>): Array<NavigatorRow> {
  let toCondense: Array<NavigatorEntry> = []
  let regularRows: Array<RegularNavigatorRow> = []
  // put the first 6 items in condensed rows, the rest as regular rows

  function getNavigatorEntriesForMapEntry(entry: NavigatorTree): Array<NavigatorEntry> {
    switch (entry.type) {
      case 'regular-entry':
        return [entry.navigatorEntry, ...entry.children.flatMap(getNavigatorEntriesForMapEntry)]
      case 'data-entry':
        return [entry.navigatorEntry]
      case 'map-entry':
        return entry.mappedEntries.flatMap(getNavigatorEntriesForMapEntry)
      case 'conditional-entry':
        return [
          entry.target,
          ...getNavigatorEntriesForMapEntry(entry.trueCase),
          ...getNavigatorEntriesForMapEntry(entry.falseCase),
        ]
      case 'slot-entry':
        return [entry.target]
      default:
        assertNever(entry)
    }
  }
  const visibleNavigatorTargets: Array<NavigatorEntry> = navigatorTree.flatMap(
    getNavigatorEntriesForMapEntry,
  )

  for (let i = 0; i < visibleNavigatorTargets.length; i++) {
    if (i < 6) {
      toCondense.push(visibleNavigatorTargets[i])
    } else {
      regularRows.push({
        type: 'regular-row',
        entry: visibleNavigatorTargets[i],
      })
    }
  }
  const condensedRow: Array<CondensedNavigatorRow> = [
    {
      type: 'condensed-row',
      entries: toCondense,
    },
  ]

  return [...condensedRow, ...regularRows]
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

  const navigatorRows = getMappedNavigatorRows(navigatorTrees)
  const navigatorTargets = navigatorRows.flatMap((row) => {
    return getEntriesForRow(row)
  })

  return {
    navigatorRows: navigatorRows,
    navigatorTargets: navigatorTargets,
    visibleNavigatorTargets: navigatorTargets, // TODO: filter out hidden
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
