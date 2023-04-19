import type { ElementPath, StaticElementPath } from '../../../core/shared/project-file-types'
import * as EP from '../../../core/shared/element-path'
import { ConditionalCase } from '../../../core/model/conditionals'
import { getUtopiaID } from '../../../core/shared/uid-utils'
import { drop } from '../../../core/shared/array-utils'

export interface ConditionalClause<P extends ElementPath> {
  elementPath: P
  clause: ConditionalCase
}

export function conditionalClauseInsertionPath<P extends ElementPath>(
  elementPath: P,
  clause: ConditionalCase,
): ConditionalClause<P> {
  return {
    elementPath: elementPath,
    clause: clause,
  }
}

export type InsertionPath<P extends ElementPath> = P | ConditionalClause<P>

export function isConditionalClauseInsertionPath<P extends ElementPath>(
  reparentTargetParent: InsertionPath<P>,
): reparentTargetParent is ConditionalClause<P> {
  return 'elementPath' in reparentTargetParent && 'clause' in reparentTargetParent
}

export function isChildInsertionPath<P extends ElementPath>(
  reparentTargetParent: InsertionPath<P>,
): reparentTargetParent is P {
  return !isConditionalClauseInsertionPath(reparentTargetParent)
}

export function getElementPathFromReparentTargetParent<P extends ElementPath>(
  reparentTargetParent: InsertionPath<P>,
): P {
  if (isConditionalClauseInsertionPath(reparentTargetParent)) {
    return reparentTargetParent.elementPath
  } else {
    return reparentTargetParent
  }
}

export function dynamicReparentTargetParentToStaticReparentTargetParent(
  reparentTargetParent: InsertionPath<ElementPath>,
): InsertionPath<StaticElementPath> {
  if (isConditionalClauseInsertionPath(reparentTargetParent)) {
    return conditionalClauseInsertionPath(
      EP.dynamicPathToStaticPath(reparentTargetParent.elementPath),
      reparentTargetParent.clause,
    )
  } else {
    return EP.dynamicPathToStaticPath(reparentTargetParent)
  }
}

export function reparentTargetToString<P extends ElementPath>(
  reparentTargetParent: InsertionPath<P>,
): string {
  if (isConditionalClauseInsertionPath(reparentTargetParent)) {
    return `${reparentTargetParent.clause} of ${EP.toString(reparentTargetParent.elementPath)}`
  } else {
    return EP.toString(reparentTargetParent)
  }
}

export function commonReparentTarget(
  first: InsertionPath<ElementPath>,
  second: InsertionPath<ElementPath>,
): InsertionPath<ElementPath> | null {
  if (isChildInsertionPath(first)) {
    if (isChildInsertionPath(second)) {
      return EP.closestSharedAncestor(first, second, true)
    } else {
      return EP.closestSharedAncestor(first, second.elementPath, true)
    }
  } else {
    if (isChildInsertionPath(second)) {
      return EP.closestSharedAncestor(first.elementPath, second, true)
    } else {
      if (EP.pathsEqual(first.elementPath, second.elementPath)) {
        if (first.clause === second.clause) {
          // If the clauses are the same (both 'true-case' or both 'false-case'), then refer specifically to that case.
          return first
        } else {
          // As the clauses are not the same, but this is the same conditional refer to the conditional instead.
          return first.elementPath
        }
      } else {
        // Best effort result which doesn't handle the clauses.
        return EP.closestSharedAncestor(first.elementPath, second.elementPath, true)
      }
    }
  }
}

export function commonReparentTargetFromArray(
  array: Array<InsertionPath<ElementPath> | null>,
): InsertionPath<ElementPath> | null {
  let workingArray: Array<InsertionPath<ElementPath>> = []
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
  return drop(1, workingArray).reduce<InsertionPath<ElementPath> | null>((working, target) => {
    if (working == null) {
      return working
    } else {
      return commonReparentTarget(working, target)
    }
  }, workingArray[0])
}
