import { ConditionalCase } from '../../../core/model/conditionals'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { drop } from '../../../core/shared/array-utils'
import { isRight } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import {
  ElementInstanceMetadataMap,
  isJSXConditionalExpression,
} from '../../../core/shared/element-template'
import { forceNotNull } from '../../../core/shared/optional-utils'
import type { ElementPath, StaticElementPath } from '../../../core/shared/project-file-types'

export interface ConditionalClause<P extends ElementPath> {
  elementPath: P
  clauseElementPath: P
  clause: ConditionalCase
}

export type InsertionPath = ChildInsertionPath | ConditionalClauseInsertionPath

export interface ChildInsertionPath {
  type: 'CHILD_INSERTION'
  intendedParentPath: StaticElementPath
}

export function childInsertionPath(
  intendedParentPath: StaticElementPath | ElementPath,
): ChildInsertionPath {
  return {
    type: 'CHILD_INSERTION',
    intendedParentPath: EP.dynamicPathToStaticPath(intendedParentPath),
  }
}

export interface ConditionalClauseInsertionPath {
  type: 'CONDITIONAL_CLAUSE_INSERTION'
  intendedParentPath: StaticElementPath
  clause: ConditionalCase
}

export function conditionalClauseInsertionPath(
  intendedParentPath: StaticElementPath | ElementPath,
  clause: ConditionalCase,
): ConditionalClauseInsertionPath {
  return {
    type: 'CONDITIONAL_CLAUSE_INSERTION',
    intendedParentPath: EP.dynamicPathToStaticPath(intendedParentPath),
    clause: clause,
  }
}

export function isChildInsertionPath(
  insertionPath: InsertionPath,
): insertionPath is ChildInsertionPath {
  return insertionPath.type === 'CHILD_INSERTION'
}

export function isConditionalClauseInsertionPath(
  insertionPath: InsertionPath,
): insertionPath is ConditionalClauseInsertionPath {
  return insertionPath.type === 'CONDITIONAL_CLAUSE_INSERTION'
}

export function getElementPathFromInsertionPath(insertionPath: InsertionPath): StaticElementPath {
  return insertionPath.intendedParentPath
}

export function insertionPathToString(insertionPath: InsertionPath): string {
  return `children of ${EP.toString(insertionPath.intendedParentPath)}`
}

// TODO: do we need this
export function commonInsertionPath(
  metadata: ElementInstanceMetadataMap,
  first: InsertionPath,
  second: InsertionPath,
): InsertionPath | null {
  const closestSharedAncestor = EP.dynamicPathToStaticPath(
    forceNotNull(
      `FIXME the common element path is null, it should be pointing to Storyboard`,
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
      closestSharedAncestorElement.conditionValue === true ? 'true-case' : 'false-case',
    )
  }

  return childInsertionPath(closestSharedAncestor)
}

export function commonInsertionPathFromArray(
  metadata: ElementInstanceMetadataMap,
  array: Array<InsertionPath | null>,
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
      return commonInsertionPath(metadata, working, target)
    }
  }, workingArray[0])
}
