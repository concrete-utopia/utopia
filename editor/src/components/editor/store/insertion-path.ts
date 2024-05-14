import type { ElementPath, StaticElementPath } from '../../../core/shared/project-file-types'
import * as EP from '../../../core/shared/element-path'
import type { ConditionalCase } from '../../../core/model/conditionals'
import {
  getConditionalCaseCorrespondingToBranchPath,
  isEmptyConditionalBranch,
} from '../../../core/model/conditionals'
import { drop } from '../../../core/shared/array-utils'
import { assertNever } from '../../../core/shared/utils'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { isJSXConditionalExpression } from '../../../core/shared/element-template'
import { isRight } from '../../../core/shared/either'
import type { ProjectContentTreeRoot } from '../../assets'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import type { PropertyControlsInfo } from '../../custom-code/code-file'

export type InsertionPath = ChildInsertionPath | ConditionalClauseInsertionPath

interface ReplaceWithSingleElementBehaviour {
  type: 'replace-with-single-element'
}

interface WrapInFragmentAndAppendElements {
  type: 'wrap-in-fragment-and-append-elements'
  fragmentUID: string
}

interface ReplaceWithElementsWrappedInFragmentBehaviour {
  type: 'replace-with-elements-wrapped-in-fragment'
  fragmentUID: string
}

export type ConditionalClauseInsertBehavior =
  | ReplaceWithSingleElementBehaviour
  | ReplaceWithElementsWrappedInFragmentBehaviour
  | WrapInFragmentAndAppendElements

export const replaceWithSingleElement = (): ReplaceWithSingleElementBehaviour => ({
  type: 'replace-with-single-element',
})

export const wrapInFragmentAndAppendElements = (uid: string): WrapInFragmentAndAppendElements => ({
  type: 'wrap-in-fragment-and-append-elements',
  fragmentUID: uid,
})

export const replaceWithElementsWrappedInFragmentBehaviour = (
  uid: string,
): ReplaceWithElementsWrappedInFragmentBehaviour => ({
  type: 'replace-with-elements-wrapped-in-fragment',
  fragmentUID: uid,
})

export interface ChildInsertionPath {
  type: 'CHILD_INSERTION'
  intendedParentPath: StaticElementPath
}

// Insert element into the intended parent's props.children array.
export function childInsertionPath(
  elementPath: StaticElementPath | ElementPath,
): ChildInsertionPath {
  return {
    type: 'CHILD_INSERTION',
    intendedParentPath: EP.dynamicPathToStaticPath(elementPath),
  }
}

export interface ConditionalClauseInsertionPath {
  type: 'CONDITIONAL_CLAUSE_INSERTION'
  intendedParentPath: StaticElementPath
  clause: ConditionalCase
  insertBehavior: ConditionalClauseInsertBehavior
}

// Insert element into the intended parent's true or false branch expression
export function conditionalClauseInsertionPath(
  elementPath: ElementPath,
  clause: ConditionalCase,
  insertBehavior: ConditionalClauseInsertBehavior,
): ConditionalClauseInsertionPath {
  return {
    type: 'CONDITIONAL_CLAUSE_INSERTION',
    intendedParentPath: EP.dynamicPathToStaticPath(elementPath),
    clause: clause,
    insertBehavior: insertBehavior,
  }
}

export function isConditionalClauseInsertionPath(
  insertionPath: InsertionPath,
): insertionPath is ConditionalClauseInsertionPath {
  return insertionPath.type === 'CONDITIONAL_CLAUSE_INSERTION'
}

export function isChildInsertionPath(
  insertionPath: InsertionPath,
): insertionPath is ChildInsertionPath {
  return insertionPath.type === 'CHILD_INSERTION'
}

export function getElementPathFromInsertionPath(insertionPath: InsertionPath): StaticElementPath {
  return insertionPath.intendedParentPath
}

export function insertionPathToString(insertionPath: InsertionPath): string {
  if (isConditionalClauseInsertionPath(insertionPath)) {
    return `${insertionPath.clause} of ${EP.toString(insertionPath.intendedParentPath)}`
  } else if (isChildInsertionPath(insertionPath)) {
    return EP.toString(insertionPath.intendedParentPath)
  } else {
    assertNever(insertionPath)
  }
}
export function getFragmentUidFromInsertionBehaviour(
  insertionBehaviour: ConditionalClauseInsertBehavior,
): string | null {
  switch (insertionBehaviour.type) {
    case 'replace-with-elements-wrapped-in-fragment':
    case 'wrap-in-fragment-and-append-elements':
      return insertionBehaviour.fragmentUID
    case 'replace-with-single-element':
      return null
    default:
      assertNever(insertionBehaviour)
  }
}

export function getFragmentUidFromInsertionPath(insertionPath: InsertionPath): string | null {
  switch (insertionPath.type) {
    case 'CHILD_INSERTION':
      return null
    case 'CONDITIONAL_CLAUSE_INSERTION':
      return getFragmentUidFromInsertionBehaviour(insertionPath.insertBehavior)
    default:
      assertNever(insertionPath)
  }
}

export function commonInsertionPath(
  metadata: ElementInstanceMetadataMap,
  first: InsertionPath,
  second: InsertionPath,
  insertBehavior: ConditionalClauseInsertBehavior,
): InsertionPath | null {
  const closestSharedAncestor = EP.dynamicPathToStaticPath(
    forceNotNull(
      `Invariant: the common element path is null, it should be pointing to Storyboard`,
      EP.closestSharedAncestor(first.intendedParentPath, second.intendedParentPath, true),
    ),
  )

  if (EP.pathsEqual(closestSharedAncestor, first.intendedParentPath)) {
    return first
  }
  if (EP.pathsEqual(closestSharedAncestor, second.intendedParentPath)) {
    return second
  }
  const closestSharedAncestorElement = forceNotNull(
    'FIXME found no element at the common path',
    MetadataUtils.findElementByElementPath(metadata, closestSharedAncestor),
  )
  if (
    isRight(closestSharedAncestorElement.element) &&
    isJSXConditionalExpression(closestSharedAncestorElement.element.value)
  ) {
    if (closestSharedAncestorElement.conditionValue === 'not-a-conditional') {
      throw new Error('found a conditional with not-a-conditional as the conditionValue')
    }
    // if the closest shared ancestor is a conditional, return the active branch
    return conditionalClauseInsertionPath(
      closestSharedAncestor,
      closestSharedAncestorElement.conditionValue.active === true ? 'true-case' : 'false-case',
      insertBehavior,
    )
  }

  return childInsertionPath(closestSharedAncestor)
}

export function commonInsertionPathFromArray(
  metadata: ElementInstanceMetadataMap,
  array: Array<InsertionPath | null>,
  insertBehavior: ConditionalClauseInsertBehavior,
): InsertionPath | null {
  let workingArray: Array<InsertionPath> = []
  for (const arrayElem of array) {
    if (arrayElem == null) {
      return null
    } else {
      workingArray.push(arrayElem)
    }
  }
  if (workingArray.length === 0) {
    return null
  }
  return drop(1, workingArray).reduce<InsertionPath | null>((working, target) => {
    if (working == null) {
      return working
    } else {
      return commonInsertionPath(metadata, working, target, insertBehavior)
    }
  }, workingArray[0])
}

export function getInsertionPath(
  target: ElementPath,
  projectContents: ProjectContentTreeRoot,
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  fragmentWrapperUID: string,
  numberOfElementsToInsert: number,
  propertyControlsInfo: PropertyControlsInfo,
): InsertionPath | null {
  const targetSupportsChildren = MetadataUtils.targetSupportsChildren(
    projectContents,
    metadata,
    target,
    elementPathTree,
    propertyControlsInfo,
  )

  if (targetSupportsChildren) {
    return childInsertionPath(target)
  }

  const conditionalClause = getConditionalCaseCorrespondingToBranchPath(target, metadata)
  if (conditionalClause == null) {
    return null
  }

  if (isEmptyConditionalBranch(target, metadata)) {
    return numberOfElementsToInsert > 1
      ? conditionalClauseInsertionPath(EP.parentPath(target), conditionalClause, {
          type: 'replace-with-elements-wrapped-in-fragment',
          fragmentUID: fragmentWrapperUID,
        })
      : conditionalClauseInsertionPath(EP.parentPath(target), conditionalClause, {
          type: 'replace-with-single-element',
        })
  }
  return conditionalClauseInsertionPath(EP.parentPath(target), conditionalClause, {
    type: 'wrap-in-fragment-and-append-elements',
    fragmentUID: fragmentWrapperUID,
  })
}
