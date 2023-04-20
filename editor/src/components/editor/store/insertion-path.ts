import type { ElementPath, StaticElementPath } from '../../../core/shared/project-file-types'
import * as EP from '../../../core/shared/element-path'
import { ConditionalCase } from '../../../core/model/conditionals'
import { getUtopiaID } from '../../../core/shared/uid-utils'
import { drop } from '../../../core/shared/array-utils'
import { assertNever } from '../../../core/shared/utils'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  ElementInstanceMetadataMap,
  isJSXConditionalExpression,
} from '../../../core/shared/element-template'
import { isRight } from '../../../core/shared/either'

export type InsertionPath = ChildInsertionPath | ConditionalClauseInsertionPath

export interface ChildInsertionPath {
  type: 'CHILD_INSERTION'
  elementPath: StaticElementPath
}

export function childInsertionPath(
  elementPath: StaticElementPath | ElementPath,
): ChildInsertionPath {
  return {
    type: 'CHILD_INSERTION',
    elementPath: EP.dynamicPathToStaticPath(elementPath),
  }
}

export interface ConditionalClauseInsertionPath {
  type: 'CONDITIONAL_CLAUSE_INSERTION'
  elementPath: StaticElementPath
  clause: ConditionalCase
}

export function conditionalClauseInsertionPath(
  elementPath: ElementPath,
  clause: ConditionalCase,
): ConditionalClauseInsertionPath {
  return {
    type: 'CONDITIONAL_CLAUSE_INSERTION',
    elementPath: EP.dynamicPathToStaticPath(elementPath),
    clause: clause,
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
  return insertionPath.elementPath
}

export function insertionPathToString(insertionPath: InsertionPath): string {
  if (isConditionalClauseInsertionPath(insertionPath)) {
    return `${insertionPath.clause} of ${EP.toString(insertionPath.elementPath)}`
  } else if (isChildInsertionPath(insertionPath)) {
    return EP.toString(insertionPath.elementPath)
  } else {
    assertNever(insertionPath)
  }
}

export function commonInsertionPath(
  metadata: ElementInstanceMetadataMap,
  first: InsertionPath,
  second: InsertionPath,
): InsertionPath | null {
  const closestSharedAncestor = EP.dynamicPathToStaticPath(
    forceNotNull(
      `Invariant: the common element path is null, it should be pointing to Storyboard`,
      EP.closestSharedAncestor(first.elementPath, second.elementPath, true),
    ),
  )

  if (EP.pathsEqual(closestSharedAncestor, first.elementPath)) {
    return first
  }
  if (EP.pathsEqual(closestSharedAncestor, second.elementPath)) {
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
