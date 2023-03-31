import type { ElementPath, StaticElementPath } from '../../../core/shared/project-file-types'
import * as EP from '../../../core/shared/element-path'
import { ConditionalCase } from '../../../core/model/conditionals'

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
