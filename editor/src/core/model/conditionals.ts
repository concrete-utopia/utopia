import { ElementPath } from '../shared/project-file-types'
import * as EP from '../shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  isJSXConditionalExpression,
  isNullJSXAttributeValue,
  JSXConditionalExpression,
  JSXElementChild,
} from '../shared/element-template'
import { ElementPathTree } from '../shared/element-path-tree'
import { getUtopiaID } from '../shared/uid-utils'
import { Optic } from '../shared/optics/optics'
import { fromField, fromTypeGuard } from '../shared/optics/optic-creators'
import { findUtopiaCommentFlag, isUtopiaCommentFlagConditional } from '../shared/comment-flags'
import { isLeft, isRight } from '../shared/either'
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

export function getConditionalCaseCorrespondingToBranchPath(
  branchPath: ElementPath,
  metadata: ElementInstanceMetadataMap,
): ConditionalCase | null {
  const conditionalElement = findMaybeConditionalExpression(EP.parentPath(branchPath), metadata)
  if (conditionalElement == null) {
    return null
  }

  return maybeBranchConditionalCase(EP.parentPath(branchPath), conditionalElement, branchPath)
}

export function getConditionalBranch(
  conditional: JSXConditionalExpression,
  clause: ConditionalCase,
): JSXElementChild {
  return clause === 'true-case' ? conditional.whenTrue : conditional.whenFalse
}

export function isNonEmptyConditionalBranch(
  elementPath: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
): boolean {
  const parentPath = EP.parentPath(elementPath)
  const conditionalParent = findMaybeConditionalExpression(parentPath, jsxMetadata)
  if (conditionalParent == null) {
    return false
  }
  const clause = maybeBranchConditionalCase(parentPath, conditionalParent, elementPath)
  if (clause == null) {
    return false
  }
  const branch = getConditionalBranch(conditionalParent, clause)
  return !isNullJSXAttributeValue(branch)
}

export function isEmptyConditionalBranch(
  elementPath: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
): boolean {
  return !isNonEmptyConditionalBranch(elementPath, jsxMetadata)
}

export function getConditionalClausePathFromMetadata(
  conditionalPath: ElementPath,
  metadata: ElementInstanceMetadataMap,
  clause: ConditionalCase,
): ElementPath | null {
  const conditionalElement = findMaybeConditionalExpression(conditionalPath, metadata)
  if (conditionalElement == null) {
    return null
  }

  return getConditionalClausePath(
    conditionalPath,
    clause === 'true-case' ? conditionalElement.whenTrue : conditionalElement.whenFalse,
  )
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

export function isActiveBranchOfOverriddenConditional(
  clause: ConditionalCase,
  metadata: ElementInstanceMetadata | null,
): boolean {
  const conditional = maybeConditionalExpression(metadata)
  if (conditional == null) {
    return false
  } else return isActiveBranchOfOverriddenConditionalFromConditional(clause, conditional)
}

function isActiveBranchOfOverriddenConditionalFromConditional(
  clause: ConditionalCase,
  conditional: JSXConditionalExpression,
): boolean {
  switch (getConditionalFlag(conditional)) {
    case true:
      return clause === 'true-case'
    case false:
      return clause === 'false-case'
    default:
      return false
  }
}

export function conditionalClauseAsBoolean(clause: ConditionalCase): boolean {
  return clause === 'true-case'
}

function booleanAsConditionalClause(bool: boolean): ConditionalCase {
  return bool ? 'true-case' : 'false-case'
}

export function isChildOfActiveBranchOfConditional(
  path: ElementPath,
  clause: ConditionalCase,
  metadata: ElementInstanceMetadataMap,
): boolean {
  const parentPath = EP.parentPath(path)
  const parentMetadata = MetadataUtils.findElementByElementPath(metadata, parentPath)
  const conditionValue = parentMetadata?.conditionValue

  if (conditionValue == null || conditionValue === 'not-a-conditional') {
    return false
  } else {
    return booleanAsConditionalClause(conditionValue) === clause
  }
}
