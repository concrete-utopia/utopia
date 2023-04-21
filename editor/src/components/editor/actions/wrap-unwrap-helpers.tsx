import { findMaybeConditionalExpression, getClauseOptic } from '../../../core/model/conditionals'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  InsertChildAndDetails,
  generateUidWithExistingComponents,
  insertChildAndDetails,
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
  UtopiaJSXComponent,
  emptyComments,
  isJSExpressionValue,
  isJSXConditionalExpression,
  isJSXElement,
  jsExpressionValue,
  jsxFragment,
} from '../../../core/shared/element-template'
import { modify } from '../../../core/shared/optics/optic-utilities'
import { forceNotNull, optionalMap } from '../../../core/shared/optional-utils'
import { ElementPath, Imports } from '../../../core/shared/project-file-types'
import { absolute } from '../../../utils/utils'
import {
  normalisePathSuccessOrThrowError,
  normalisePathToUnderlyingTarget,
} from '../../custom-code/code-file'
import { EditorDispatch } from '../action-types'
import {
  EditorState,
  SimpleParseSuccess,
  getOpenUIJSFileKey,
  insertElementAtPath,
  modifyParseSuccessAtPath,
  modifyParseSuccessWithSimple,
  modifyUnderlyingTargetElement,
} from '../store/editor-state'
import {
  ConditionalClauseInsertionPath,
  InsertionPath,
  childInsertionPath,
  getElementPathFromInsertionPath,
  isChildInsertionPath,
} from '../store/insertion-path'
import { deleteView } from './action-creators'
import { UPDATE_FNS } from './actions'
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
        const updatedComponents = insertElementAtPath(
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

export function wrapElementInsertions(
  editor: EditorState,
  targets: Array<ElementPath>,
  parentPath: InsertionPath,
  elementToInsert: JSXElement | JSXFragment | JSXConditionalExpression,
  importsToAdd: Imports,
  anyTargetIsARootElement: boolean,
  targetThatIsRootElementOfCommonParent: ElementPath | undefined,
): { updatedEditor: EditorState; newPath: ElementPath | null; detailsOfUpdate: string | null } {
  let viewPath: ElementPath | null = null
  let detailsOfUpdate: string | null = null

  let indexInParent: number | null = null
  if (parentPath != null && isChildInsertionPath(parentPath)) {
    indexInParent = optionalMap(
      (firstPathMatchingCommonParent) =>
        MetadataUtils.getIndexInParent(editor.jsxMetadata, firstPathMatchingCommonParent),
      targets.find((target) => EP.pathsEqual(EP.parentPath(target), parentPath.intendedParentPath)),
    )
  }

  const underlyingTarget = normalisePathToUnderlyingTarget(
    editor.projectContents,
    editor.nodeModules.files,
    forceNotNull('No storyboard file found', getOpenUIJSFileKey(editor)),
    targetThatIsRootElementOfCommonParent ?? getElementPathFromInsertionPath(parentPath),
  )

  const targetSuccess = normalisePathSuccessOrThrowError(underlyingTarget)
  const withWrapperAdded = modifyParseSuccessAtPath(
    targetSuccess.filePath,
    editor,
    (parseSuccess) => {
      const utopiaJSXComponents = getUtopiaJSXComponentsFromSuccess(parseSuccess)
      let withTargetAdded: InsertChildAndDetails = insertChildAndDetails(utopiaJSXComponents)

      function withInsertedJSXElement() {
        return insertElementAtPath(
          editor.projectContents,
          editor.canvas.openFile?.filename ?? null,
          parentPath,
          elementToInsert,
          utopiaJSXComponents,
          optionalMap(
            (index) => ({
              type: 'before',
              index: index,
            }),
            indexInParent,
          ),
        )
      }

      // TODO this entire targetThatIsRootElementOfCommonParent could be simplified by introducing a "rootElementInsertionPath" to InsertionPath
      const staticTarget =
        optionalMap(childInsertionPath, targetThatIsRootElementOfCommonParent) ?? parentPath

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
            return parseSuccess
          }

          switch (staticTarget.type) {
            case 'CHILD_INSERTION':
              withTargetAdded = withInsertedJSXElement()
              break
            case 'CONDITIONAL_CLAUSE_INSERTION':
              withTargetAdded = withElementInsertedIntoJSXConditional(
                utopiaJSXComponents,
                staticTarget,
                elementToInsert,
              )
              break
            default:
              const _exhaustiveCheck: never = staticTarget
              return parseSuccess
          }

          break
        }
        case 'JSX_ELEMENT': {
          switch (staticTarget.type) {
            case 'CHILD_INSERTION':
              withTargetAdded = withInsertedJSXElement()
              break
            case 'CONDITIONAL_CLAUSE_INSERTION':
              withTargetAdded = withElementInsertedIntoJSXConditional(
                utopiaJSXComponents,
                staticTarget,
                elementToInsert,
              )
              break
            default:
              const _exhaustiveCheck: never = staticTarget
              return parseSuccess
          }
          break
        }
        case 'JSX_CONDITIONAL_EXPRESSION': {
          switch (staticTarget.type) {
            case 'CHILD_INSERTION':
              withTargetAdded = withInsertedJSXElement()
              break
            case 'CONDITIONAL_CLAUSE_INSERTION':
              withTargetAdded = insertChildAndDetails(
                transformJSXComponentAtPath(
                  utopiaJSXComponents,
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
                ),
              )
              break
            default:
              const _exhaustiveCheck: never = staticTarget
              return parseSuccess
          }
          break
        }
        default:
          const _exhaustiveCheck: never = elementToInsert
          return parseSuccess
      }

      viewPath = anyTargetIsARootElement
        ? EP.appendNewElementPath(getElementPathFromInsertionPath(parentPath), elementToInsert.uid)
        : EP.appendToPath(getElementPathFromInsertionPath(parentPath), elementToInsert.uid)

      detailsOfUpdate = withTargetAdded.insertionDetails
      return modifyParseSuccessWithSimple((success: SimpleParseSuccess) => {
        return {
          ...success,
          utopiaComponents: withTargetAdded.components,
          imports: mergeImports(targetSuccess.filePath, success.imports, importsToAdd),
        }
      }, parseSuccess)
    },
  )
  return { updatedEditor: withWrapperAdded, newPath: viewPath, detailsOfUpdate: detailsOfUpdate }
}

function withElementInsertedIntoJSXConditional(
  utopiaJSXComponents: Array<UtopiaJSXComponent>,
  staticTarget: ConditionalClauseInsertionPath,
  element: JSXElement | JSXFragment,
) {
  return insertChildAndDetails(
    transformJSXComponentAtPath(
      utopiaJSXComponents,
      getElementPathFromInsertionPath(staticTarget),
      (oldRoot) => {
        if (isJSXConditionalExpression(oldRoot)) {
          const clauseOptic = getClauseOptic(staticTarget.clause)
          return modify(
            clauseOptic,
            (clauseElement) => {
              return {
                ...element,
                children: [...element.children, clauseElement],
              }
            },
            oldRoot,
          )
        } else {
          return {
            ...element,
            children: [...element.children, oldRoot],
          }
        }
      },
    ),
  )
}
