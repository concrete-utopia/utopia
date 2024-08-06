import type { ElementPath } from '../shared/project-file-types'
import * as EP from '../shared/element-path'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSXConditionalExpression,
  JSXElementChild,
} from '../shared/element-template'
import {
  isJSExpression,
  isJSPropertyAccess,
  isJSXConditionalExpression,
  isNullJSXAttributeValue,
} from '../shared/element-template'
import type { ElementPathTree, ElementPathTrees } from '../shared/element-path-tree'
import { getUtopiaID } from '../shared/uid-utils'
import type { Optic } from '../shared/optics/optics'
import { fromField, fromTypeGuard } from '../shared/optics/optic-creators'
import {
  findUtopiaCommentFlag,
  isUtopiaPropOrCommentFlagConditional,
} from '../shared/comment-flags'
import { isLeft, isRight } from '../shared/either'
import { MetadataUtils } from './element-metadata-utils'
import { forceNotNull } from '../shared/optional-utils'
import { Substores, useEditorState } from '../../components/editor/store/store-hook'

export type ConditionalCase = 'true-case' | 'false-case'

// Get the path for the clause (true case or false case) of a conditional.
export function getConditionalClausePath(
  conditionalPath: ElementPath,
  conditionalBranch: JSXElementChild,
): ElementPath {
  return EP.appendToPath(conditionalPath, getUtopiaID(conditionalBranch))
}

// Ensure that the children of a conditional are the whenTrue clause followed
// by the whenFalse clause.
export function reorderConditionalChildPathTrees(
  conditional: JSXConditionalExpression,
  conditionalPath: ElementPath,
  children: Array<ElementPathTree>,
): Array<ElementPathTree> {
  let result: Array<ElementPathTree> = []

  // The whenTrue clause should be first.
  const trueCasePath = getConditionalClausePath(conditionalPath, conditional.whenTrue)
  const trueCasePathString = EP.toString(trueCasePath)
  const trueCasePathTree = children.find((child) => child.pathString === trueCasePathString)
  if (trueCasePathTree != null) {
    result.push(trueCasePathTree)
  }

  // The whenFalse clause should be second.
  const falseCasePath = getConditionalClausePath(conditionalPath, conditional.whenFalse)
  const falseCasePathString = EP.toString(falseCasePath)
  const falseCasePathTree = children.find((child) => child.pathString === falseCasePathString)
  if (falseCasePathTree != null) {
    result.push(falseCasePathTree)
  }

  return result
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
  if (!isUtopiaPropOrCommentFlagConditional(flag)) {
    return null
  }
  return flag.value
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

export function maybeConditionalActiveBranch(
  elementPath: ElementPath | null,
  jsxMetadata: ElementInstanceMetadataMap,
): JSXElementChild | null {
  if (elementPath == null) {
    return null
  }
  const conditional = maybeConditionalExpression(
    MetadataUtils.findElementByElementPath(jsxMetadata, elementPath),
  )
  if (conditional == null) {
    return null
  }

  const activeCase = forceNotNull(
    'conditional should have an active case',
    getConditionalActiveCase(elementPath, conditional, jsxMetadata),
  )
  return getConditionalBranch(conditional, activeCase)
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

export function isConditionalWithEmptyOrTextEditableActiveBranch(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
): {
  element: JSXConditionalExpression
  clause: ConditionalCase | null
  isEmpty: boolean
  textEditable: boolean
} | null {
  const conditional = findMaybeConditionalExpression(path, metadata)
  if (conditional == null) {
    return null
  }
  const clause = getConditionalActiveCase(path, conditional, metadata)
  if (clause == null) {
    return {
      element: conditional,
      isEmpty: false,
      textEditable: false,
      clause: clause,
    }
  }
  const branch = clause === 'true-case' ? conditional.whenTrue : conditional.whenFalse
  return {
    element: conditional,
    isEmpty: isNullJSXAttributeValue(branch),
    textEditable: isTextEditableConditional(path, metadata, elementPathTree),
    clause: clause,
  }
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

export function isOverriddenConditional(element: ElementInstanceMetadata | null): boolean {
  if (
    element == null ||
    isLeft(element.element) ||
    !isJSXConditionalExpression(element.element.value)
  ) {
    return false
  }
  return getConditionalFlag(element.element.value) != null
}

export function getConditionalActiveCase(
  path: ElementPath,
  conditional: JSXConditionalExpression,
  metadataMap: ElementInstanceMetadataMap,
): ConditionalCase | null {
  const override = getConditionalFlag(conditional)
  if (override != null) {
    return override ? 'true-case' : 'false-case'
  }
  const metadata = MetadataUtils.findElementByElementPath(metadataMap, path)
  if (metadata == null) {
    return 'true-case'
  }
  if (metadata.conditionValue === 'not-a-conditional') {
    return null
  }
  return metadata.conditionValue.active ? 'true-case' : 'false-case'
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

export function findFirstNonConditionalAncestor(
  initial: ElementPath,
  metadata: ElementInstanceMetadataMap,
): ElementPath {
  const parent = EP.parentPath(initial)
  if (findMaybeConditionalExpression(parent, metadata) == null) {
    return parent
  }
  return findFirstNonConditionalAncestor(parent, metadata)
}

export function isTextEditableConditional(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
): boolean {
  const element = MetadataUtils.findElementByElementPath(metadata, path)
  if (element == null) {
    // element doesn't exist
    return false
  }
  const conditional = maybeConditionalExpression(element)
  if (conditional == null) {
    // element is not a conditional
    return false
  }
  const nonConditionalAncestor = findFirstNonConditionalAncestor(path, metadata)
  const siblings = MetadataUtils.getChildrenOrdered(
    metadata,
    elementPathTree,
    nonConditionalAncestor,
  )
  if (siblings.length !== 1) {
    // we don't allow text editing of conditional branches when the conditional has siblings
    // (or if the topmost nested conditional has siblings)
    return false
  }

  const childrenInMetadata = MetadataUtils.getNonExpressionDescendants(
    metadata,
    elementPathTree,
    path,
  )
  if (childrenInMetadata.length > 0) {
    // we don't allow text editing of conditional branches when the conditional have children in metadata
    // that would mean there are elefants in the active branch, which should not be text editable
    return false
  }

  // Finally we check if the active conditional branch is a js expression. Maybe this is an overkill,
  // because the earlier checks already prove this is a leaf element.
  const activeConditionalBranch = maybeConditionalActiveBranch(path, metadata)
  return activeConditionalBranch != null && isJSExpression(activeConditionalBranch)
}

export function useConditionalCaseCorrespondingToBranchPath(
  elementPath: ElementPath,
): ConditionalCase | null {
  return useEditorState(
    Substores.metadata,
    (store) => getConditionalCaseCorrespondingToBranchPath(elementPath, store.editor.jsxMetadata),
    'useConditionalCaseCorrespondingToBranchPath conditionalCase',
  )
}
