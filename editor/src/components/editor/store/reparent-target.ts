import type { ElementPath, StaticElementPath } from '../../../core/shared/project-file-types'
import * as EP from '../../../core/shared/element-path'
import { ConditionalCase } from '../../../core/model/conditionals'
import { getUtopiaID } from '../../../core/shared/uid-utils'
import { drop } from '../../../core/shared/array-utils'
import { IndexPosition } from '../../../utils/utils'
import { forceNotNull } from '../../../core/shared/optional-utils'
import {
  ElementInstanceMetadataMap,
  isJSXConditionalExpression,
} from '../../../core/shared/element-template'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { isRight } from '../../../core/shared/either'

export interface ConditionalClause<P extends ElementPath> {
  elementPath: P
  clauseElementPath: P
  clause: ConditionalCase
}

export type InsertionPath = ArrayInsertionPath | SlotInsertionPath | ConditionalClauseInsertionPath

export interface ArrayInsertionPath {
  type: 'ARRAY_INSERTION'
  propName: 'children'
  elementPath: StaticElementPath
  indexPosition: IndexPosition | null
}

export function arrayInsertionPath(
  elementPath: StaticElementPath | ElementPath,
  propName: 'children',
  indexPosition: IndexPosition | null,
): ArrayInsertionPath {
  return {
    type: 'ARRAY_INSERTION',
    elementPath: EP.dynamicPathToStaticPath(elementPath),
    propName: propName,
    indexPosition: indexPosition,
  }
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

export function conditionalClauseInsertionPath(
  elementPath: StaticElementPath | ElementPath,
  propName: ConditionalCase,
): ConditionalClauseInsertionPath {
  return {
    type: 'CONDITIONAL_CLAUSE_INSERTION',
    elementPath: EP.dynamicPathToStaticPath(elementPath),
    propName: propName,
  }
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
): reparentTargetParent is ConditionalClauseInsertionPath {
  return reparentTargetParent.type === 'CONDITIONAL_CLAUSE_INSERTION'
}

export function getElementPathFromInsertionPath(
  reparentTargetParent: InsertionPath,
): StaticElementPath {
  return reparentTargetParent.elementPath
}

// export function dynamicReparentTargetParentToStaticReparentTargetParent(
//   reparentTargetParent: InsertionPath,
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
  metadata: ElementInstanceMetadataMap,
  first: InsertionPath,
  second: InsertionPath,
): InsertionPath | null {
  const closestSharedAncestor = EP.dynamicPathToStaticPath(
    forceNotNull(
      `FIXME the common element path is null, it should be pointing to Storyboard`,
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

  return arrayInsertionPath(closestSharedAncestor, 'children', null)
}

export function commonReparentTargetFromArray(
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
      return commonReparentTarget(metadata, working, target)
    }
  }, workingArray[0])
}
