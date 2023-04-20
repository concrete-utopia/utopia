import { findMaybeConditionalExpression, getClauseOptic } from '../../../core/model/conditionals'
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
  JSXElementChild,
  emptyComments,
  isJSExpressionValue,
  isJSXConditionalExpression,
  isJSXElement,
  jsExpressionValue,
  jsxFragment,
} from '../../../core/shared/element-template'
import { modify } from '../../../core/shared/optics/optic-utilities'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { absolute } from '../../../utils/utils'
import { EditorDispatch } from '../action-types'
import {
  EditorState,
  insertElementAtPath_DEPRECATED,
  modifyUnderlyingTargetElement,
} from '../store/editor-state'
import { ConditionalClauseInsertionPath, childInsertionPath } from '../store/insertion-path'
import { deleteView } from './action-creators'
import { UPDATE_FNS } from './actions'

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
  const targetParent = EP.parentPath(target)

  const conditional = findMaybeConditionalExpression(target, editor.jsxMetadata)
  const elementMetadata = MetadataUtils.findElementByElementPath(editor.jsxMetadata, target)
  let elementToInsert: JSXElementChild | null = null
  if (
    conditional != null &&
    elementMetadata != null &&
    MetadataUtils.isConditionalFromMetadata(elementMetadata)
  ) {
    const currentValue = elementMetadata.conditionValue
    if (currentValue === true) {
      elementToInsert = conditional.whenTrue
    } else if (currentValue === false) {
      elementToInsert = conditional.whenFalse
    }
  }

  const originalIndexPosition = MetadataUtils.getIndexInParent(editor.jsxMetadata, target)

  const withParentUpdated = modifyUnderlyingTargetElement(
    targetParent,
    forceNotNull('No storyboard file found', editor.canvas.openFile?.filename),
    editor,
    (element) => element,
    (success) => {
      if (elementToInsert != null) {
        const components = getUtopiaJSXComponentsFromSuccess(success)
        const updatedComponents = insertElementAtPath_DEPRECATED(
          editor.projectContents,
          editor.canvas.openFile?.filename ?? null,
          childInsertionPath(targetParent),
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
      }
      return success
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
      return isJSExpressionValue(conditional.whenTrue)
    } else if (currentValue === false) {
      return isJSExpressionValue(conditional.whenFalse)
    }
  }
  return false
}
