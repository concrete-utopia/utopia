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
import { ElementPathTreeRoot } from '../shared/element-path-tree'
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
  childPaths: ElementPathTreeRoot,
): ElementPathTreeRoot {
  if (Object.keys(childPaths).length > 2) {
    throw new Error(`Too many child paths.`)
  } else {
    let result: ElementPathTreeRoot = {}

    // The whenTrue clause should be first.
    const trueCasePath = getConditionalClausePath(conditionalPath, conditional.whenTrue)
    const trueCasePathString = EP.toString(trueCasePath)
    const trueCasePathTree = childPaths[trueCasePathString]
    if (trueCasePathTree != null) {
      result[trueCasePathString] = trueCasePathTree
    }

    // The whenFalse clause should be second.
    const falseCasePath = getConditionalClausePath(conditionalPath, conditional.whenFalse)
    const falseCasePathString = EP.toString(falseCasePath)
    const falseCasePathTree = childPaths[falseCasePathString]
    if (falseCasePathTree != null) {
      result[falseCasePathString] = falseCasePathTree
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

export function isConditionalWithEmptyActiveBranch(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
): boolean {
  const conditional = findMaybeConditionalExpression(path, metadata)
  if (conditional == null) {
    return false
  }
  const clause = getConditionalActiveCase(path, conditional, metadata)
  if (clause == null) {
    return false
  }
  const branch = clause === 'true-case' ? conditional.whenTrue : conditional.whenFalse
  return isNullJSXAttributeValue(branch)
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
  const spy = spyMetadata[EP.toString(path)] ?? null
  if (spy == null) {
    return 'true-case'
  }
  if (spy.conditionValue === 'not-a-conditional') {
    return null
  }
  return spy.conditionValue.active ? 'true-case' : 'false-case'
}

export function conditionalClauseAsBoolean(clause: ConditionalCase): boolean {
  return clause === 'true-case'
}

export function isActiveBranchOfConditional(
  clause: ConditionalCase,
  metadata: ElementInstanceMetadata | null,
): boolean {
  const conditionValue = metadata?.conditionValue
  if (conditionValue == null || conditionValue === 'not-a-conditional') {
    return false
  } else {
    const clauseValue = conditionalClauseAsBoolean(clause)
    return clauseValue === conditionValue.active
  }
}

export function isDefaultBranchOfConditional(
  clause: ConditionalCase,
  metadata: ElementInstanceMetadata | null,
): boolean {
  const conditionValue = metadata?.conditionValue
  if (conditionValue == null || conditionValue === 'not-a-conditional') {
    return false
  } else {
    const clauseValue = conditionalClauseAsBoolean(clause)
    return clauseValue === conditionValue.default
  }
}

export function isActiveOrDefaultBranchOfConditional(
  clause: ConditionalCase,
  metadata: ElementInstanceMetadata | null,
): boolean {
  return (
    isActiveBranchOfConditional(clause, metadata) || isDefaultBranchOfConditional(clause, metadata)
  )
}

export function getActualReparentParentForConditional(
  initial: ElementPath,
  metadata: ElementInstanceMetadataMap,
): ElementPath {
  const parent = EP.parentPath(initial)
  if (findMaybeConditionalExpression(parent, metadata) == null) {
    return parent
  }
  return getActualReparentParentForConditional(parent, metadata)
}
