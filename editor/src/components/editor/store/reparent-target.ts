import type { ElementPath, StaticElementPath } from '../../../core/shared/project-file-types'
import * as EP from '../../../core/shared/element-path'
import { ConditionalCase } from '../../../core/model/conditionals'
import { getUtopiaID } from '../../../core/shared/uid-utils'
import { drop } from '../../../core/shared/array-utils'

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

export type ReparentTargetParent<P extends ElementPath> = P | ConditionalClause<P>

export function reparentTargetParentIsConditionalClause<P extends ElementPath>(
  reparentTargetParent: ReparentTargetParent<P>,
): reparentTargetParent is ConditionalClause<P> {
  return 'elementPath' in reparentTargetParent && 'clause' in reparentTargetParent
}

export function reparentTargetParentIsElementPath<P extends ElementPath>(
  reparentTargetParent: ReparentTargetParent<P>,
): reparentTargetParent is P {
  return !reparentTargetParentIsConditionalClause(reparentTargetParent)
}

export function getElementPathFromReparentTargetParent<P extends ElementPath>(
  reparentTargetParent: ReparentTargetParent<P>,
): P {
  if (reparentTargetParentIsConditionalClause(reparentTargetParent)) {
    return reparentTargetParent.elementPath
  } else {
    return reparentTargetParent
  }
}

export function dynamicReparentTargetParentToStaticReparentTargetParent(
  reparentTargetParent: ReparentTargetParent<ElementPath>,
): ReparentTargetParent<StaticElementPath> {
  if (reparentTargetParentIsConditionalClause(reparentTargetParent)) {
    return conditionalClause(
      EP.dynamicPathToStaticPath(reparentTargetParent.elementPath),
      reparentTargetParent.clause,
    )
  } else {
    return EP.dynamicPathToStaticPath(reparentTargetParent)
  }
}

export function reparentTargetToString<P extends ElementPath>(
  reparentTargetParent: ReparentTargetParent<P>,
): string {
  if (reparentTargetParentIsConditionalClause(reparentTargetParent)) {
    return `${reparentTargetParent.clause} of ${EP.toString(reparentTargetParent.elementPath)}`
  } else {
    return EP.toString(reparentTargetParent)
  }
}

export function commonReparentTarget(
  first: ReparentTargetParent<ElementPath>,
  second: ReparentTargetParent<ElementPath>,
): ReparentTargetParent<ElementPath> | null {
  if (reparentTargetParentIsElementPath(first)) {
    if (reparentTargetParentIsElementPath(second)) {
      return EP.closestSharedAncestor(first, second, true)
    } else {
      return EP.closestSharedAncestor(first, second.elementPath, true)
    }
  } else {
    if (reparentTargetParentIsElementPath(second)) {
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
  array: Array<ReparentTargetParent<ElementPath> | null>,
): ReparentTargetParent<ElementPath> | null {
  let workingArray: Array<ReparentTargetParent<ElementPath>> = []
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
  return drop(1, workingArray).reduce<ReparentTargetParent<ElementPath> | null>(
    (working, target) => {
      if (working == null) {
        return working
      } else {
        return commonReparentTarget(working, target)
      }
    },
    workingArray[0],
  )
}
