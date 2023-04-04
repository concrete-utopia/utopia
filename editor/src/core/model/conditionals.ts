import { ElementPath } from '../shared/project-file-types'
import * as EP from '../shared/element-path'
import {
  ElementInstanceMetadata,
  isJSXConditionalExpression,
  JSXConditionalExpression,
  JSXElementChild,
} from '../shared/element-template'
import { ElementPathTree } from '../shared/element-path-tree'
import { getUtopiaID } from '../shared/uid-utils'
import { Optic } from '../shared/optics/optics'
import { fromField, fromTypeGuard } from '../shared/optics/optic-creators'
import { findUtopiaCommentFlag, isUtopiaCommentFlagConditional } from '../shared/comment-flags'

export type ConditionalCase = 'true-case' | 'false-case'

// Get the path for the clause (true case or false case) of a conditional.
export function getConditionalClausePath(
  conditionalPath: ElementPath,
  conditionalClause: JSXElementChild,
  _: ConditionalCase, // TODO remove this parameter
): ElementPath {
  return EP.appendToPath(conditionalPath, getUtopiaID(conditionalClause))
}

// Ensure that the children of a conditional are the whenTrue clause followed
// by the whenFalse clause.
export function reorderConditionalChildPathTrees(
  conditional: JSXConditionalExpression,
  conditionalPath: ElementPath,
  childPaths: Array<ElementPathTree>,
): Array<ElementPathTree> {
  if (childPaths.length > 2) {
    throw new Error(`Too many child paths.`)
  } else {
    let result: Array<ElementPathTree> = []

    // The whenTrue clause should be first.
    const trueCasePath = getConditionalClausePath(
      conditionalPath,
      conditional.whenTrue,
      'true-case',
    )
    const trueCasePathTree = childPaths.find((childPath) =>
      EP.pathsEqual(childPath.path, trueCasePath),
    )
    if (trueCasePathTree != null) {
      result.push(trueCasePathTree)
    }

    // The whenFalse clause should be second.
    const falseCasePath = getConditionalClausePath(
      conditionalPath,
      conditional.whenFalse,
      'false-case',
    )
    const falseCasePathTree = childPaths.find((childPath) =>
      EP.pathsEqual(childPath.path, falseCasePath),
    )
    if (falseCasePathTree != null) {
      result.push(falseCasePathTree)
    }

    return result
  }
}

export const jsxConditionalExpressionOptic: Optic<JSXElementChild, JSXConditionalExpression> =
  fromTypeGuard(isJSXConditionalExpression)

export const conditionalWhenTrueOptic: Optic<JSXConditionalExpression, JSXElementChild> =
  fromField('whenTrue')

export const conditionalWhenFalseOptic: Optic<JSXConditionalExpression, JSXElementChild> =
  fromField('whenFalse')

export function getConditionalCase(
  elementPath: ElementPath,
  parent: JSXConditionalExpression,
  spyParentMetadata: ElementInstanceMetadata,
  parentPath: ElementPath,
): ConditionalCase | 'not-a-conditional' {
  if (spyParentMetadata.conditionValue === 'not-a-conditional') {
    return 'not-a-conditional'
  }
  const parentOverride = getConditionalFlag(parent)
  if (parentOverride == null) {
    return spyParentMetadata.conditionValue ? 'true-case' : 'false-case'
  }
  if (
    matchesOverriddenConditionalBranch(elementPath, parentPath, {
      clause: parent.whenTrue,
      branch: 'true-case',
      wantOverride: true,
      parentOverride: parentOverride,
    })
  ) {
    return 'true-case'
  }
  return 'false-case'
}

export function getConditionalFlag(element: JSXConditionalExpression): boolean | null {
  const flag = findUtopiaCommentFlag(element.comments, 'conditional')
  if (!isUtopiaCommentFlagConditional(flag)) {
    return null
  }
  return flag.value
}

export function matchesOverriddenConditionalBranch(
  elementPath: ElementPath,
  parentPath: ElementPath,
  params: {
    clause: JSXElementChild
    branch: ConditionalCase
    wantOverride: boolean
    parentOverride: boolean
  },
): boolean {
  const { clause, branch, wantOverride, parentOverride } = params
  return (
    wantOverride === parentOverride &&
    EP.pathsEqual(elementPath, getConditionalClausePath(parentPath, clause, branch))
  )
}
