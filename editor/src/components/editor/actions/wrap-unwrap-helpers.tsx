import {
  findMaybeConditionalExpression,
  getClauseOptic,
  getConditionalActiveCase,
  getConditionalBranch,
} from '../../../core/model/conditionals'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  generateUidWithExistingComponents,
  transformJSXComponentAtPath,
} from '../../../core/model/element-template-utils'
import {
  applyUtopiaJSXComponentsChanges,
  getUtopiaJSXComponentsFromSuccess,
} from '../../../core/model/project-file-utils'
import * as EP from '../../../core/shared/element-path'
import {
  ElementInstanceMetadataMap,
  JSXConditionalExpression,
  JSXElement,
  JSXElementChild,
  JSXFragment,
  emptyComments,
  isJSXAttributeValue,
  isJSXConditionalExpression,
  isJSXElement,
  isJSXElementLike,
  jsExpressionValue,
  jsxFragment,
  jsxTextBlock,
} from '../../../core/shared/element-template'
import { modify } from '../../../core/shared/optics/optic-utilities'
import { forceNotNull, optionalMap } from '../../../core/shared/optional-utils'
import { ElementPath, Imports } from '../../../core/shared/project-file-types'
import { IndexPosition, absolute } from '../../../utils/utils'
import { EditorDispatch } from '../action-types'
import {
  EditorState,
  insertElementAtPath,
  modifyUnderlyingTargetElement,
} from '../store/editor-state'
import {
  ConditionalClauseInsertionPath,
  InsertionPath,
  childInsertionPath,
  getDefaultInsertionPathForElementPathSlot,
  getElementPathFromInsertionPath,
  isChildInsertionPath,
} from '../store/insertion-path'
import { deleteView } from './action-creators'
import { UPDATE_FNS } from './actions'
import { foldAndApplyCommandsSimple } from '../../canvas/commands/commands'
import { addElement } from '../../canvas/commands/add-element-command'
import { mergeImports } from '../../../core/workers/common/project-file-utils'

export function unwrapConditionalClause(
  editor: EditorState,
  target: ElementPath,
  parentPath: ConditionalClauseInsertionPath,
): EditorState {
  let newSelection: Array<ElementPath> = []
  const withElementMoved = modifyUnderlyingTargetElement(
    parentPath.intendedParentPath,
    forceNotNull('No storyboard file found', editor.canvas.openFile?.filename),
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
  dispatch: EditorDispatch,
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
  const originalIndexPosition = MetadataUtils.getIndexInParent(editor.jsxMetadata, target)

  const withParentUpdated = modifyUnderlyingTargetElement(
    targetParent,
    forceNotNull('No storyboard file found', editor.canvas.openFile?.filename),
    editor,
    (element) => element,
    (success) => {
      const components = getUtopiaJSXComponentsFromSuccess(success)

      const insertionPath = getDefaultInsertionPathForElementPathSlot(
        targetParent,
        editor.projectContents,
        editor.nodeModules.files,
        editor.canvas.openFile?.filename,
        editor.jsxMetadata,
      )
      if (insertionPath == null) {
        throw new Error('Invalid unwrap insertion path')
      }

      const updatedComponents = insertElementAtPath(
        editor.projectContents,
        insertionPath,
        elementToInsert,
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

  return UPDATE_FNS.DELETE_VIEW(deleteView(target), withParentUpdated, dispatch)
}

export function isTextContainingConditional(
  target: ElementPath,
  metadata: ElementInstanceMetadataMap,
): boolean {
  const element = MetadataUtils.findElementByElementPath(metadata, target)
  const conditional = findMaybeConditionalExpression(target, metadata)
  if (conditional != null && element != null && MetadataUtils.isConditionalFromMetadata(element)) {
    const currentValue = element.conditionValue
    if (currentValue === true) {
      return !isJSXElementLike(conditional.whenTrue)
    } else if (currentValue === false) {
      return !isJSXElementLike(conditional.whenFalse)
    }
  }
  return false
}

export function wrapElementInsertions(
  editor: EditorState,
  targets: Array<ElementPath>,
  parentPath: InsertionPath,
  elementToInsert: JSXElement | JSXFragment | JSXConditionalExpression,
  importsToAdd: Imports,
  anyTargetIsARootElement: boolean,
  targetThatIsRootElementOfCommonParent: ElementPath | undefined,
): { updatedEditor: EditorState; newPath: ElementPath | null } {
  // TODO this entire targetThatIsRootElementOfCommonParent could be simplified by introducing a "rootElementInsertionPath" to InsertionPath
  const staticTarget =
    optionalMap(childInsertionPath, targetThatIsRootElementOfCommonParent) ?? parentPath

  const newPath = anyTargetIsARootElement
    ? EP.appendNewElementPath(getElementPathFromInsertionPath(parentPath), elementToInsert.uid)
    : EP.appendToPath(getElementPathFromInsertionPath(parentPath), elementToInsert.uid)

  const indexPosition = findIndexPositionInParent(targets, parentPath, editor.jsxMetadata)

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
              addElement(
                'always',
                staticTarget,
                elementToInsert,
                { importsToAdd, indexPosition },
                'use-deprecated-insertJSXElementChild',
              ),
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
              addElement(
                'always',
                staticTarget,
                elementToInsert,
                { importsToAdd, indexPosition },
                'use-deprecated-insertJSXElementChild',
              ),
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
              addElement(
                'always',
                staticTarget,
                elementToInsert,
                { importsToAdd, indexPosition },
                'use-deprecated-insertJSXElementChild',
              ),
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
): IndexPosition | undefined {
  let indexInParent: number | null = null
  if (parentPath != null && isChildInsertionPath(parentPath)) {
    indexInParent = optionalMap(
      (firstPathMatchingCommonParent) =>
        MetadataUtils.getIndexInParent(metadata, firstPathMatchingCommonParent),
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

function insertElementIntoJSXConditional(
  editor: EditorState,
  staticTarget: ConditionalClauseInsertionPath,
  elementToInsert: JSXElement | JSXFragment,
  importsToAdd: Imports,
): EditorState {
  return modifyUnderlyingTargetElement(
    staticTarget.intendedParentPath,
    forceNotNull('No storyboard file found', editor.canvas.openFile?.filename),
    editor,
    (element) => element,
    (success, _, underlyingFilePath) => {
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
                return {
                  ...elementToInsert,
                  children: [...elementToInsert.children, clauseElement],
                }
              },
              oldRoot,
            )
          } else {
            return {
              ...elementToInsert,
              children: [...elementToInsert.children, oldRoot],
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
        imports: mergeImports(underlyingFilePath, success.imports, importsToAdd),
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
    forceNotNull('No storyboard file found', editor.canvas.openFile?.filename),
    editor,
    (element) => element,
    (success, _, underlyingFilePath) => {
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
                return { ...elementToInsert, whenTrue: clauseElement }
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
        imports: mergeImports(underlyingFilePath, success.imports, importsToAdd),
      }
    },
  )
}
