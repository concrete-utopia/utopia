import { IS_TEST_ENVIRONMENT } from '../../common/env-vars'
import { getContentsTreeFromPath, addFileToProjectContents } from '../../components/assets'
import type { EditorState } from '../../components/editor/store/editor-state'
import { StoryboardFilePath } from '../../components/editor/store/editor-state'
import { isLeft } from '../shared/either'
import type { JSXElementChild, ElementsWithin, JSXElement } from '../shared/element-template'
import { getJSXAttribute, jsExpressionValue, emptyComments } from '../shared/element-template'
import { setJSXValueAtPath } from '../shared/jsx-attributes'
import type { TextFile } from '../shared/project-file-types'
import * as PP from '../shared/property-path'
import { assertNever } from '../shared/utils'
import { isSceneAgainstImports, isRemixSceneAgainstImports } from './project-file-utils'

export const SceneCommentIdPropName = 'commentId'

function getIdPropFromJSXElement(element: JSXElement): string | null {
  const maybeIdProp = getJSXAttribute(element.props, SceneCommentIdPropName)
  if (
    maybeIdProp == null ||
    maybeIdProp.type !== 'ATTRIBUTE_VALUE' ||
    typeof maybeIdProp.value !== 'string'
  ) {
    return null
  }
  return maybeIdProp.value
}

function setIdPropOnJSXElement(element: JSXElement, idPropValueToUse: string): JSXElement | null {
  const updatedProps = setJSXValueAtPath(
    element.props,
    PP.create(SceneCommentIdPropName),
    jsExpressionValue(idPropValueToUse, emptyComments),
  )

  if (IS_TEST_ENVIRONMENT || isLeft(updatedProps)) {
    return null
  }
  return { ...element, props: updatedProps.value }
}

function transformJSXElementChildRecursively(
  element: JSXElementChild,
  transform: (_: JSXElementChild) => JSXElementChild,
): JSXElementChild {
  switch (element.type) {
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'ATTRIBUTE_VALUE':
    case 'JSX_TEXT_BLOCK':
      return element
    case 'JSX_CONDITIONAL_EXPRESSION':
      const whenTrue = transform(element.whenTrue)
      const whenFalse = transform(element.whenTrue)
      return transform({ ...element, whenTrue, whenFalse })
    case 'JSX_FRAGMENT': {
      const children = element.children.map((c) => transform(c))
      return transform({ ...element, children })
    }
    case 'JSX_MAP_EXPRESSION':
      let elementsWithin: ElementsWithin = {}
      for (const [key, value] of Object.entries(element.elementsWithin)) {
        elementsWithin[key] = transform(value) as JSXElement
      }
      return transform({ ...element, elementsWithin })
    case 'JSX_ELEMENT':
      const children = element.children.map((c) => transform(c))
      return transform({ ...element, children })
    default:
      assertNever(element)
  }
}

export function ensureSceneIdsExist(editor: EditorState): EditorState {
  const storyboardFile = getContentsTreeFromPath(editor.projectContents, StoryboardFilePath)
  if (
    storyboardFile == null ||
    storyboardFile.type !== 'PROJECT_CONTENT_FILE' ||
    storyboardFile.content.type !== 'TEXT_FILE' ||
    storyboardFile.content.fileContents.parsed.type !== 'PARSE_SUCCESS'
  ) {
    return editor
  }

  let seenIdProps: Set<string> = new Set()
  let anyIdPropUpdated = false
  const imports = storyboardFile.content.fileContents.parsed.imports

  const nextToplevelElements = storyboardFile.content.fileContents.parsed.topLevelElements.map(
    (e) => {
      if (e.type !== 'UTOPIA_JSX_COMPONENT') {
        return e
      }

      const nextRootElement = transformJSXElementChildRecursively(e.rootElement, (child) => {
        const isConsideredScene =
          isSceneAgainstImports(child, imports) || isRemixSceneAgainstImports(child, imports)

        if (child.type !== 'JSX_ELEMENT' || !isConsideredScene) {
          return child
        }

        const idPropValue = getIdPropFromJSXElement(child)

        if (idPropValue != null && !seenIdProps.has(idPropValue)) {
          seenIdProps.add(idPropValue)
          return child
        }

        const idPropValueToUse = child.uid
        const updatedChild = setIdPropOnJSXElement(child, idPropValueToUse)
        if (updatedChild == null) {
          return child
        }

        seenIdProps.add(idPropValueToUse)
        anyIdPropUpdated = true
        return updatedChild
      })

      return { ...e, rootElement: nextRootElement }
    },
  )

  if (!anyIdPropUpdated) {
    return editor
  }

  const nextStoryboardFile: TextFile = {
    ...storyboardFile.content,
    fileContents: {
      ...storyboardFile.content.fileContents,
      parsed: {
        ...storyboardFile.content.fileContents.parsed,
        topLevelElements: nextToplevelElements,
      },
    },
  }

  const nextProjectContents = addFileToProjectContents(
    editor.projectContents,
    StoryboardFilePath,
    nextStoryboardFile,
  )
  return { ...editor, projectContents: nextProjectContents }
}
