import { ElementPath } from '../shared/project-file-types'
import * as EP from '../shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  isJSXConditionalExpression,
  JSXConditionalExpression,
  JSXElementChild,
} from '../shared/element-template'
import { ElementPathTree } from '../shared/element-path-tree'
import { getUtopiaID } from '../shared/uid-utils'
import { Optic } from '../shared/optics/optics'
import { fromField, fromTypeGuard } from '../shared/optics/optic-creators'
import { findUtopiaCommentFlag, isUtopiaCommentFlagConditional } from '../shared/comment-flags'
import { isRight } from '../shared/either'
import { MetadataUtils } from './element-metadata-utils'

export type ConditionalCase = 'true-case' | 'false-case'

// Get the path for the clause (true case or false case) of a conditional.
export function getConditionalClausePath(
  conditionalPath: ElementPath,
  conditionalClause: JSXElementChild,
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
    const trueCasePath = getConditionalClausePath(conditionalPath, conditional.whenTrue)
    const trueCasePathTree = childPaths.find((childPath) =>
      EP.pathsEqual(childPath.path, trueCasePath),
    )
    if (trueCasePathTree != null) {
      result.push(trueCasePathTree)
    }

    // The whenFalse clause should be second.
    const falseCasePath = getConditionalClausePath(conditionalPath, conditional.whenFalse)
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

export function getClauseOptic(
  clause: ConditionalCase,
): Optic<JSXConditionalExpression, JSXElementChild> {
  if (clause === 'true-case') {
    return conditionalWhenTrueOptic
  } else {
    return conditionalWhenFalseOptic
  }
}

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
    wantOverride: boolean
    parentOverride: boolean
  },
): boolean {
  const { clause, wantOverride, parentOverride } = params
  return (
    wantOverride === parentOverride &&
    EP.pathsEqual(elementPath, getConditionalClausePath(parentPath, clause))
  )
}

export function maybeConditionalExpression(
  element: ElementInstanceMetadata | null,
): JSXConditionalExpression | null {
  if (
    element != null &&
    isRight(element.element) &&
    isJSXConditionalExpression(element.element.value)
  ) {
    return element.element.value
  }
  return null
}

export function findMaybeConditionalExpression(
  elementPath: ElementPath | null,
  jsxMetadata: ElementInstanceMetadataMap,
): JSXConditionalExpression | null {
  return maybeConditionalExpression(
    MetadataUtils.findElementByElementPath(jsxMetadata, elementPath),
  )
}

export function maybeBranchConditionalCase(
  conditionalPath: ElementPath,
  conditional: JSXConditionalExpression | null,
  branchPath: ElementPath,
): ConditionalCase | null {
  if (conditional == null) {
    return null
  }
  const truePath = EP.appendToPath(conditionalPath, conditional.whenTrue.uid)
  const falsePath = EP.appendToPath(conditionalPath, conditional.whenFalse.uid)
  if (EP.pathsEqual(truePath, branchPath)) {
    return 'true-case'
  } else if (EP.pathsEqual(falsePath, branchPath)) {
    return 'false-case'
  } else {
    return null
  }
}

export function getConditionalActiveCase(
  path: ElementPath,
  conditional: JSXConditionalExpression,
  spyMetadata: ElementInstanceMetadataMap,
): ConditionalCase | null {
  const override = getConditionalFlag(conditional)
  if (override != null) {
    return override ? 'true-case' : 'false-case'
  }
  const spy = spyMetadata[EP.toString(path)] ?? true
  if (spy.conditionValue === 'not-a-conditional') {
    return null
  }
  return spy.conditionValue ? 'true-case' : 'false-case'
}
