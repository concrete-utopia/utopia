import type { ElementPath, JSXElement, PropertyPath } from 'utopia-shared/src/types'
import { foldEither } from '../../../../core/shared/either'
import type { ValueAtPath } from '../../../../core/shared/jsx-attributes'
import { setJSXValuesAtPaths, unsetJSXValuesAtPaths } from '../../../../core/shared/jsx-attributes'
import type { EditorState, EditorStatePatch } from '../../../editor/store/editor-state'
import { modifyUnderlyingElementForOpenFile } from '../../../editor/store/editor-state'
import { patchParseSuccessAtElementPath } from '../patch-utils'
import type { CSSStyleProperty, StyleInfo } from '../../canvas-types'
import { isCSSNumber, type CSSNumber } from '../../../inspector/common/css-utils'

export interface EditorStateWithPatch {
  editorStateWithChanges: EditorState
  editorStatePatch: EditorStatePatch
}

export function applyValuesAtPath(
  editorState: EditorState,
  target: ElementPath,
  jsxValuesAndPathsToSet: ValueAtPath[],
): EditorStateWithPatch {
  const workingEditorState = modifyUnderlyingElementForOpenFile(
    target,
    editorState,
    (element: JSXElement) => {
      return foldEither(
        () => {
          return element
        },
        (updatedProps) => {
          return {
            ...element,
            props: updatedProps,
          }
        },
        setJSXValuesAtPaths(element.props, jsxValuesAndPathsToSet),
      )
    },
  )

  const editorStatePatch = patchParseSuccessAtElementPath(target, workingEditorState, (success) => {
    return {
      topLevelElements: {
        $set: success.topLevelElements,
      },
      imports: {
        $set: success.imports,
      },
    }
  })

  return {
    editorStateWithChanges: workingEditorState,
    editorStatePatch: editorStatePatch,
  }
}

export function deleteValuesAtPath(
  editorState: EditorState,
  target: ElementPath,
  properties: Array<PropertyPath>,
): EditorStateWithPatch {
  const workingEditorState = modifyUnderlyingElementForOpenFile(
    target,
    editorState,
    (element: JSXElement) => {
      return foldEither(
        () => {
          return element
        },
        (updatedProps) => {
          return {
            ...element,
            props: updatedProps,
          }
        },
        unsetJSXValuesAtPaths(element.props, properties),
      )
    },
  )

  const editorStatePatch = patchParseSuccessAtElementPath(target, workingEditorState, (success) => {
    return {
      topLevelElements: {
        $set: success.topLevelElements,
      },
      imports: {
        $set: success.imports,
      },
    }
  })
  return {
    editorStateWithChanges: workingEditorState,
    editorStatePatch: editorStatePatch,
  }
}

export function maybeCssPropertyFromInlineStyle(property: PropertyPath): string | null {
  const [maybeStyle, prop] = property.propertyElements
  if (maybeStyle !== 'style' || prop == null || typeof prop !== 'string') {
    return null
  }
  return prop
}

export type GetCSSNumberFromStyleInfoResult =
  | { type: 'not-found' }
  | { type: 'not-css-number' }
  | { type: 'css-number'; number: CSSNumber }

export function getCSSNumberFromStyleInfo(
  styleInfo: StyleInfo,
  property: string,
): GetCSSNumberFromStyleInfoResult {
  let styleInfoUntyped = styleInfo as any as Record<string, CSSStyleProperty<unknown>>
  const prop = styleInfoUntyped[property]
  if (prop == null || prop.type === 'not-found') {
    return { type: 'not-found' }
  }

  if (prop.type === 'not-editable' || !isCSSNumber(prop.value)) {
    return { type: 'not-css-number' }
  }
  return { type: 'css-number', number: prop.value }
}
