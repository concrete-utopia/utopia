import type { ElementPath, StaticElementPath } from '../../../core/shared/project-file-types'
import * as EP from '../../../core/shared/element-path'
import { ConditionalCase } from '../../../core/model/conditionals'
import { getUtopiaID } from '../../../core/shared/uid-utils'
import { drop } from '../../../core/shared/array-utils'
import { IndexPosition } from '../../../utils/utils'

export interface ConditionalClause<P extends ElementPath> {
  elementPath: P
  clause: ConditionalCase
}

export function conditionalClause<P extends ElementPath>(
  elementPath: P,
  clause: ConditionalCase,
): ConditionalClause<P> {
  return {
    elementPath: elementPath,
    clause: clause,
  }
}

export type InsertionPath = ArrayInsertionPath | SlotInsertionPath | ConditionalClauseInsertionPath

export interface ArrayInsertionPath {
  type: 'ARRAY_INSERTION'
  propName: string
  elementPath: StaticElementPath
  indexPosition: IndexPosition | null
}

export interface SlotInsertionPath {
  type: 'SLOT_INSERTION'
  propName: string
  elementPath: StaticElementPath
  nullable: boolean
}

export interface ConditionalClauseInsertionPath {
  type: 'CONDITIONAL_CLAUSE_INSERTION'
  propName: ConditionalCase
  elementPath: StaticElementPath
}

export function insertionPathIsSlot(
  reparentTargetParent: InsertionPath,
): reparentTargetParent is SlotInsertionPath {
  return reparentTargetParent.type === 'SLOT_INSERTION'
}

export function insertionPathIsArray(
  reparentTargetParent: InsertionPath,
): reparentTargetParent is ArrayInsertionPath {
  return reparentTargetParent.type === 'ARRAY_INSERTION'
}

export function insertionPathIsConditionalClause(
  reparentTargetParent: InsertionPath,
): reparentTargetParent is ArrayInsertionPath {
  return reparentTargetParent.type === 'CONDITIONAL_CLAUSE_INSERTION'
}

export function getElementPathFromInsertionPath(
  reparentTargetParent: InsertionPath,
): StaticElementPath {
  return reparentTargetParent.elementPath
}

// export function dynamicReparentTargetParentToStaticReparentTargetParent(
//   reparentTargetParent: InsertionPath<ElementPath>,
// ): InsertionPath<StaticElementPath> {
//   if (insertionPathIsSlot(reparentTargetParent)) {
//     return conditionalClause(
//       EP.dynamicPathToStaticPath(reparentTargetParent.elementPath),
//       reparentTargetParent.clause,
//     )
//   } else {
//     return EP.dynamicPathToStaticPath(reparentTargetParent)
//   }
// }

export function insertionPathToString(reparentTargetParent: InsertionPath): string {
  return `${reparentTargetParent.propName} of ${EP.toString(reparentTargetParent.elementPath)}`
}

// TODO: do we need this
export function commonReparentTarget(
  first: InsertionPath,
  second: InsertionPath,
): InsertionPath | null {
  const ancestor = EP.closestSharedAncestor(first.elementPath, second.elementPath, true)
  if (ancestor == null) {
    return null
  }
  return {
    type: 'ARRAY_INSERTION',
    elementPath: EP.dynamicPathToStaticPath(ancestor),
    propName: 'children',
    indexPosition: null,
  }
}

export function commonReparentTargetFromArray(
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
  return drop(1, workingArray).reduce<InsertionPath> | null>((working, target) => {
    if (working == null) {
      return working
    } else {
      return commonReparentTarget(working, target)
    }
  }, workingArray[0])
}
