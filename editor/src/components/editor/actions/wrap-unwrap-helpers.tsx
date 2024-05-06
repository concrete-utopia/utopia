import {
  findMaybeConditionalExpression,
  getClauseOptic,
  getConditionalActiveCase,
  getConditionalBranch,
} from '../../../core/model/conditionals'
import {
  MetadataUtils,
  getSimpleAttributeAtPath,
  propertyHasSimpleValue,
} from '../../../core/model/element-metadata-utils'
import {
  generateUidWithExistingComponents,
  insertJSXElementChildren,
  renameJsxElementChild,
  transformJSXComponentAtPath,
} from '../../../core/model/element-template-utils'
import {
  applyUtopiaJSXComponentsChanges,
  getUtopiaJSXComponentsFromSuccess,
} from '../../../core/model/project-file-utils'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import type {
  ElementInstanceMetadataMap,
  JSXConditionalExpression,
  JSXElement,
  JSXFragment,
} from '../../../core/shared/element-template'
import {
  JSXElementChild,
  emptyComments,
  isJSXAttributeValue,
  isJSXConditionalExpression,
  isJSXElement,
  isJSXElementLike,
  jsExpressionValue,
  jsxFragment,
  jsxTextBlock,
} from '../../../core/shared/element-template'
import { modify, toFirst } from '../../../core/shared/optics/optic-utilities'
import { forceNotNull, optionalMap } from '../../../core/shared/optional-utils'
import type { ElementPath, Imports } from '../../../core/shared/project-file-types'
import type { IndexPosition } from '../../../utils/utils'
import { absolute } from '../../../utils/utils'
import type { EditorDispatch } from '../action-types'
import type { DerivedState, EditorState } from '../store/editor-state'
import {
  modifyUnderlyingTargetElement,
  withUnderlyingTargetFromEditorState,
} from '../store/editor-state'
import type { ConditionalClauseInsertionPath, InsertionPath } from '../store/insertion-path'
import {
  childInsertionPath,
  getElementPathFromInsertionPath,
  getInsertionPath,
  isChildInsertionPath,
} from '../store/insertion-path'
import { deleteView } from './action-creators'
import { UPDATE_FNS } from './actions'
import { foldAndApplyCommandsSimple } from '../../canvas/commands/commands'
import { addElement } from '../../canvas/commands/add-element-command'
import { mergeImports } from '../../../core/workers/common/project-file-utils'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import { fixUtopiaElementGeneric } from '../../../core/shared/uid-utils'
import { getAllUniqueUids } from '../../../core/model/get-unique-ids'
import { foldEither, right } from '../../../core/shared/either'
import { editorStateToElementChildOptic } from '../../../core/model/common-optics'
import { fromField, fromTypeGuard } from '../../../core/shared/optics/optic-creators'
import { setJSXValueAtPath } from '../../../core/shared/jsx-attribute-utils'
import { addToastToState } from './toast-helpers'
import { notice } from '../../../components/common/notice'

export function unwrapConditionalClause(
  editor: EditorState,
  target: ElementPath,
  parentPath: ConditionalClauseInsertionPath,
): EditorState {
  let newSelection: Array<ElementPath> = []
  const withElementMoved = modifyUnderlyingTargetElement(
    parentPath.intendedParentPath,
    editor,
    (element) => element,
    (success) => {
      const components = getUtopiaJSXComponentsFromSuccess(success)
      const updatedComponents = transformJSXComponentAtPath(
        components,
        EP.dynamicPathToStaticPath(parentPath.intendedParentPath),
        (elem) => {
          if (isJSXConditionalExpression(elem)) {
            const clauseOptic = getClauseOptic(parentPath.clause)
            return modify(
              clauseOptic,
              (clauseElement) => {
                if (isJSXElement(clauseElement)) {
                  if (clauseElement.children.length === 0) {
                    return jsExpressionValue(null, emptyComments)
                  } else if (clauseElement.children.length === 1) {
                    const childElement = clauseElement.children[0]
                    newSelection.push(
                      EP.appendToPath(parentPath.intendedParentPath, childElement.uid),
                    )
                    return childElement
                  } else {
                    const newUID = generateUidWithExistingComponents(editor.projectContents)
                    newSelection.push(EP.appendToPath(parentPath.intendedParentPath, newUID))
                    return jsxFragment(newUID, clauseElement.children, false)
                  }
                } else if (isJSXConditionalExpression(clauseElement)) {
                  const activeCase = getConditionalActiveCase(
                    target,
                    clauseElement,
                    editor.spyMetadata,
                  )
                  if (activeCase != null) {
                    return activeCase === 'true-case'
                      ? clauseElement.whenTrue
                      : clauseElement.whenFalse
                  }
                }
                return clauseElement
              },
              elem,
            )
          } else {
            return elem
          }
        },
      )

      const updatedTopLevelElements = applyUtopiaJSXComponentsChanges(
        success.topLevelElements,
        updatedComponents,
      )

      return {
        ...success,
        topLevelElements: updatedTopLevelElements,
      }
    },
  )

  return { ...withElementMoved, selectedViews: newSelection }
}
export function unwrapTextContainingConditional(
  editor: EditorState,
  target: ElementPath,
): EditorState {
  const conditional = findMaybeConditionalExpression(target, editor.jsxMetadata)
  if (conditional == null) {
    return editor
  }
  const activeCase = getConditionalActiveCase(target, conditional, editor.spyMetadata)
  if (activeCase == null) {
    return editor
  }
  const branch = getConditionalBranch(conditional, activeCase)
  const isTextBranch = isJSXAttributeValue(branch) && typeof branch.value === 'string'
  const elementToInsert = isTextBranch ? jsxTextBlock(branch.value) : branch

  const targetParent = EP.parentPath(target)
  const originalIndexPosition = MetadataUtils.getIndexInParent(
    editor.jsxMetadata,
    editor.elementPathTree,
    target,
  )

  const withParentUpdated = modifyUnderlyingTargetElement(
    targetParent,
    editor,
    (element) => element,
    (success) => {
      const components = getUtopiaJSXComponentsFromSuccess(success)

      const wrapperUID = generateUidWithExistingComponents(editor.projectContents)
      const insertionPath = getInsertionPath(
        targetParent,
        editor.projectContents,
        editor.jsxMetadata,
        editor.elementPathTree,
        wrapperUID,
        1,
      )
      if (insertionPath == null) {
        throw new Error('Invalid unwrap insertion path')
      }

      const updatedComponents = insertJSXElementChildren(
        insertionPath,
        [elementToInsert],
        components,
        absolute(originalIndexPosition),
      )

      const updatedTopLevelElements = applyUtopiaJSXComponentsChanges(
        success.topLevelElements,
        updatedComponents.components,
      )

      return {
        ...success,
        topLevelElements: updatedTopLevelElements,
      }
    },
  )

  return UPDATE_FNS.DELETE_VIEW(deleteView(target), withParentUpdated)
}

export function isTextContainingConditional(
  target: ElementPath,
  metadata: ElementInstanceMetadataMap,
): boolean {
  const element = MetadataUtils.findElementByElementPath(metadata, target)
  const conditional = findMaybeConditionalExpression(target, metadata)
  if (
    conditional != null &&
    element != null &&
    MetadataUtils.isConditionalFromMetadata(element) &&
    element.conditionValue !== 'not-a-conditional'
  ) {
    const currentValue = element.conditionValue.active
    if (currentValue === true) {
      return !isJSXElementLike(conditional.whenTrue)
    } else if (currentValue === false) {
      return !isJSXElementLike(conditional.whenFalse)
    }
  }
  return false
}

function elementHasPositionOf(targetElement: JSXElement, positionValue: string): boolean {
  return propertyHasSimpleValue(
    right(targetElement.props),
    PP.create('style', 'position'),
    positionValue,
  )
}

function elementHasContainLayout(targetElement: JSXElement): boolean {
  return propertyHasSimpleValue(right(targetElement.props), PP.create('style', 'contain'), 'layout')
}

export function fixParentContainingBlockSettings(
  editorState: EditorState,
  targetPath: ElementPath,
): EditorState {
  // Determine if the current status of this element means that we potentially need to look
  // into its ancestors.
  const possibleTargetElement = toFirst(
    editorStateToElementChildOptic(targetPath).compose(fromTypeGuard(isJSXElement)),
    editorState,
  )
  const targetNecessitatesAncestorChecks: boolean = foldEither(
    () => {
      return false
    },
    (targetElement) => {
      // Currently any element with `position: 'absolute'` is a likely candidate.
      const targetHasPositionAbsolute = elementHasPositionOf(targetElement, 'absolute')
      return targetHasPositionAbsolute
    },
    possibleTargetElement,
  )

  let shouldUpdateParent: boolean = false
  const parentPath = EP.parentPath(targetPath)
  const parentOptic = editorStateToElementChildOptic(parentPath).compose(
    fromTypeGuard(isJSXElement),
  )
  // Check the status of the parent to see if that needs to be updated.
  // Do not go outside of the current component, at least for now.
  if (targetNecessitatesAncestorChecks && EP.isFromSameInstanceAs(targetPath, parentPath)) {
    // Determine if the parent needs to be fixed up.
    const possibleParentElement = toFirst(parentOptic, editorState)
    shouldUpdateParent = foldEither(
      () => {
        return false
      },
      (parentElement) => {
        // If the parent does not specify `position: 'absolute'`, `position: 'relative'`
        // or it doesn't already have `contain: 'layout'`, then it needs updating.
        const parentDefinesContainingBlock =
          elementHasPositionOf(parentElement, 'absolute') ||
          elementHasPositionOf(parentElement, 'relative') ||
          elementHasContainLayout(parentElement)
        return !parentDefinesContainingBlock
      },
      possibleParentElement,
    )
  }

  // Update the parent if necessary.
  if (shouldUpdateParent) {
    let attributesUpdated: boolean = false
    const updatedEditorState = modify(
      parentOptic.compose(fromField('props')),
      (attributes) => {
        // Try to update the attributes, which may not be possible if they're
        // defined by an expression.
        const maybeUpdatedAttributes = setJSXValueAtPath(
          attributes,
          PP.create('style', 'contain'),
          jsExpressionValue('layout', emptyComments),
        )
        return foldEither(
          () => {
            return attributes
          },
          (updatedAttributes) => {
            attributesUpdated = true
            return updatedAttributes
          },
          maybeUpdatedAttributes,
        )
      },
      editorState,
    )
    if (attributesUpdated) {
      // Add a toast indicating that `contain: 'layout'` has been added.
      const toast = notice(
        "Added `contain: 'layout'` to the parent of the newly added element.",
        'INFO',
      )
      return addToastToState(updatedEditorState, toast)
    }
  }

  return editorState
}

export function wrapElementInsertions(
  editor: EditorState,
  targets: Array<ElementPath>,
  parentPath: InsertionPath,
  rawElementToInsert: JSXElement | JSXFragment | JSXConditionalExpression,
  importsToAdd: Imports,
  anyTargetIsARootElement: boolean,
  targetThatIsRootElementOfCommonParent: ElementPath | undefined,
): { updatedEditor: EditorState; newPath: ElementPath | null } {
  // TODO this entire targetThatIsRootElementOfCommonParent could be simplified by introducing a "rootElementInsertionPath" to InsertionPath
  const staticTarget =
    optionalMap(childInsertionPath, targetThatIsRootElementOfCommonParent) ?? parentPath

  const existingIDsMutable = new Set(getAllUniqueUids(editor.projectContents).allIDs)
  const elementToInsert = fixUtopiaElementGeneric<typeof rawElementToInsert>(
    rawElementToInsert,
    existingIDsMutable,
  ).value

  const newPath = anyTargetIsARootElement
    ? EP.appendNewElementPath(getElementPathFromInsertionPath(parentPath), elementToInsert.uid)
    : EP.appendToPath(getElementPathFromInsertionPath(parentPath), elementToInsert.uid)

  const indexPosition = findIndexPositionInParent(
    targets,
    parentPath,
    editor.jsxMetadata,
    editor.elementPathTree,
  )

  switch (elementToInsert.type) {
    case 'JSX_FRAGMENT': {
      function pathsToBeWrappedInFragment(): ElementPath[] {
        const elements: ElementPath[] = targets.filter((path) => {
          return !targets
            .filter((otherPath) => !EP.pathsEqual(otherPath, path))
            .some((otherPath) => EP.isDescendantOf(path, otherPath))
        })
        const parents = new Set<ElementPath>()
        elements.forEach((e) => parents.add(EP.parentPath(e)))
        if (parents.size !== 1) {
          return []
        }
        return elements
      }

      const children = pathsToBeWrappedInFragment()
      if (children.length === 0) {
        // nothing to do
        return { updatedEditor: editor, newPath: null }
      }

      switch (staticTarget.type) {
        case 'CHILD_INSERTION':
          return {
            updatedEditor: foldAndApplyCommandsSimple(editor, [
              addElement('always', staticTarget, elementToInsert, { importsToAdd, indexPosition }),
            ]),
            newPath: newPath,
          }
        case 'CONDITIONAL_CLAUSE_INSERTION':
          const withTargetAdded = insertElementIntoJSXConditional(
            editor,
            staticTarget,
            elementToInsert,
            importsToAdd,
          )
          return { updatedEditor: withTargetAdded, newPath: newPath }
        default:
          const _exhaustiveCheck: never = staticTarget
          return { updatedEditor: editor, newPath: null }
      }
    }
    case 'JSX_ELEMENT': {
      switch (staticTarget.type) {
        case 'CHILD_INSERTION':
          return {
            updatedEditor: foldAndApplyCommandsSimple(editor, [
              addElement('always', staticTarget, elementToInsert, { importsToAdd, indexPosition }),
            ]),
            newPath: newPath,
          }
        case 'CONDITIONAL_CLAUSE_INSERTION':
          const withTargetAdded = insertElementIntoJSXConditional(
            editor,
            staticTarget,
            elementToInsert,
            importsToAdd,
          )
          return { updatedEditor: withTargetAdded, newPath: newPath }
        default:
          const _exhaustiveCheck: never = staticTarget
          return { updatedEditor: editor, newPath: null }
      }
    }
    case 'JSX_CONDITIONAL_EXPRESSION': {
      switch (staticTarget.type) {
        case 'CHILD_INSERTION':
          return {
            updatedEditor: foldAndApplyCommandsSimple(editor, [
              addElement('always', staticTarget, elementToInsert, { importsToAdd, indexPosition }),
            ]),
            newPath: newPath,
          }
        case 'CONDITIONAL_CLAUSE_INSERTION':
          const withTargetAdded = insertConditionalIntoConditionalClause(
            editor,
            staticTarget,
            elementToInsert,
            importsToAdd,
          )
          return { updatedEditor: withTargetAdded, newPath: newPath }
        default:
          const _exhaustiveCheck: never = staticTarget
          return { updatedEditor: editor, newPath: null }
      }
    }
    default:
      const _exhaustiveCheck: never = elementToInsert
      return { updatedEditor: editor, newPath: null }
  }
}

function findIndexPositionInParent(
  targets: Array<ElementPath>,
  parentPath: InsertionPath,
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
): IndexPosition | undefined {
  let indexInParent: number | null = null
  if (parentPath != null && isChildInsertionPath(parentPath)) {
    indexInParent = optionalMap(
      (firstPathMatchingCommonParent) =>
        MetadataUtils.getIndexInParent(metadata, elementPathTree, firstPathMatchingCommonParent),
      targets.find((target) => EP.pathsEqual(EP.parentPath(target), parentPath.intendedParentPath)),
    )
  }
  return (
    optionalMap(
      (index) =>
        ({
          type: 'before',
          index: index,
        } as IndexPosition),
      indexInParent,
    ) ?? undefined
  )
}

export function insertElementIntoJSXConditional(
  editor: EditorState,
  staticTarget: ConditionalClauseInsertionPath,
  elementToInsert: JSXElement | JSXFragment,
  importsToAdd: Imports,
): EditorState {
  return modifyUnderlyingTargetElement(
    staticTarget.intendedParentPath,
    editor,
    (element) => element,
    (success, _, underlyingFilePath) => {
      const components = getUtopiaJSXComponentsFromSuccess(success)

      const { imports, duplicateNameMapping } = mergeImports(
        underlyingFilePath,
        success.imports,
        importsToAdd,
      )

      let elementToInsertRenamed = renameJsxElementChild(elementToInsert, duplicateNameMapping)

      const updatedComponents = transformJSXComponentAtPath(
        components,
        getElementPathFromInsertionPath(staticTarget),
        (oldRoot) => {
          if (isJSXConditionalExpression(oldRoot)) {
            const clauseOptic = getClauseOptic(staticTarget.clause)
            return modify(
              clauseOptic,
              (clauseElement) => {
                return {
                  ...elementToInsertRenamed,
                  children: [...elementToInsertRenamed.children, clauseElement],
                }
              },
              oldRoot,
            )
          } else {
            return {
              ...elementToInsertRenamed,
              children: [...elementToInsertRenamed.children, oldRoot],
            }
          }
        },
      )

      const updatedTopLevelElements = applyUtopiaJSXComponentsChanges(
        success.topLevelElements,
        updatedComponents,
      )

      return {
        ...success,
        topLevelElements: updatedTopLevelElements,
        imports: imports,
      }
    },
  )
}
function insertConditionalIntoConditionalClause(
  editor: EditorState,
  staticTarget: ConditionalClauseInsertionPath,
  elementToInsert: JSXConditionalExpression,
  importsToAdd: Imports,
): EditorState {
  return modifyUnderlyingTargetElement(
    staticTarget.intendedParentPath,
    editor,
    (element) => element,
    (success, _, underlyingFilePath) => {
      const { imports, duplicateNameMapping } = mergeImports(
        underlyingFilePath,
        success.imports,
        importsToAdd,
      )

      let elementToInsertRenamed = renameJsxElementChild(elementToInsert, duplicateNameMapping)

      const components = getUtopiaJSXComponentsFromSuccess(success)
      const updatedComponents = transformJSXComponentAtPath(
        components,
        getElementPathFromInsertionPath(staticTarget),
        (oldRoot) => {
          if (isJSXConditionalExpression(oldRoot)) {
            const clauseOptic = getClauseOptic(staticTarget.clause)
            return modify(
              clauseOptic,
              (clauseElement) => {
                return { ...elementToInsertRenamed, whenTrue: clauseElement }
              },
              oldRoot,
            )
          } else {
            return { ...oldRoot }
          }
        },
      )

      const updatedTopLevelElements = applyUtopiaJSXComponentsChanges(
        success.topLevelElements,
        updatedComponents,
      )

      return {
        ...success,
        topLevelElements: updatedTopLevelElements,
        imports: imports,
      }
    },
  )
}
