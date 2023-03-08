import {
  EditorState,
  modifyUnderlyingForOpenFile,
  withUnderlyingTargetFromEditorState,
} from '../../components/editor/store/editor-state'
import {
  ChildOrAttribute,
  isJSXConditionalExpression,
  JSXConditionalExpression,
  JSXElementChild,
} from '../shared/element-template'
import { fromField, fromTypeGuard } from '../shared/optics/optic-creators'
import { Optic, traversal } from '../shared/optics/optics'
import { ElementPath } from '../shared/project-file-types'

export function forElementOptic(target: ElementPath): Optic<EditorState, JSXElementChild> {
  function from(editor: EditorState): Array<JSXElementChild> {
    return withUnderlyingTargetFromEditorState(target, editor, [], (_, element) => {
      return [element]
    })
  }
  function update(
    editor: EditorState,
    modify: (child: JSXElementChild) => JSXElementChild,
  ): EditorState {
    return modifyUnderlyingForOpenFile(target, editor, modify)
  }
  return traversal(from, update)
}

export const jsxConditionalExpressionOptic: Optic<JSXElementChild, JSXConditionalExpression> =
  fromTypeGuard(isJSXConditionalExpression)

export const conditionalWhenTrueOptic: Optic<JSXConditionalExpression, ChildOrAttribute> =
  fromField('whenTrue')

export const conditionalWhenFalseOptic: Optic<JSXConditionalExpression, ChildOrAttribute> =
  fromField('whenFalse')
