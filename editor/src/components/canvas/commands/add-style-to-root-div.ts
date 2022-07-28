import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import {
  emptyComments,
  jsxAttributeNestedObject,
  jsxAttributeOtherJavaScript,
  jsxSpreadAssignment,
} from '../../../core/shared/element-template'
import { ValueAtPath } from '../../../core/shared/jsx-attributes'
import { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { jsxAttributeArbitrary } from '../../../core/workers/parser-printer/parser-printer.test-utils'
import { EditorState } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { applyValuesAtPath } from './adjust-number-command'
import { BaseCommand, CommandFunction, TransientOrNot } from './commands'

export interface AddStyleToRootDiv extends BaseCommand {
  type: 'ADD_STYLE_TO_ROOT_DIV'
  target: ElementPath
}

export function addStyleToRootDiv(
  transient: TransientOrNot,
  target: ElementPath,
): AddStyleToRootDiv {
  return {
    type: 'ADD_STYLE_TO_ROOT_DIV',
    transient: transient,
    target: target,
  }
}

export const runAddStyleToRootDiv: CommandFunction<AddStyleToRootDiv> = (
  editorState: EditorState,
  command: AddStyleToRootDiv,
) => {
  const propsToUpdate: Array<ValueAtPath> = [
    {
      path: PP.create(['style']),
      value: jsxAttributeNestedObject(
        [
          jsxSpreadAssignment(
            jsxAttributeOtherJavaScript('props.style', 'return props.style;', ['props'], null, {}),
            emptyComments,
          ),
        ],
        emptyComments,
      ),
    },
  ]

  const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
    editorState,
    command.target,
    propsToUpdate,
  )

  return {
    editorStatePatches: [propertyUpdatePatch],
    commandDescription: 'Add Style to Root Div',
  }
}
