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
  const isComponentOrScene =
    EP.pathsEqual(command.target, editorState.focusedElementPath) ||
    MetadataUtils.isProbablyScene(editorState.jsxMetadata, command.target)
  const targetRoot = isComponentOrScene
    ? Object.values(editorState.jsxMetadata).filter((possibleRoot) =>
        EP.isRootElementOf(possibleRoot.elementPath, command.target),
      )
    : []

  if (targetRoot.length > 0) {
    const propsToUpdate: Array<ValueAtPath> = [
      {
        path: PP.create(['style']),
        value: jsxAttributeNestedObject(
          [
            jsxSpreadAssignment(
              jsxAttributeOtherJavaScript(
                'props.style',
                'return props.style;',
                ['props'],
                null,
                {},
              ),
              emptyComments,
            ),
          ],
          emptyComments,
        ),
      },
    ]

    const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
      editorState,
      targetRoot[0].elementPath,
      propsToUpdate,
    )

    return {
      editorStatePatches: [propertyUpdatePatch],
      commandDescription: 'Add Style to Root Div',
    }
  } else {
    return {
      editorStatePatches: [],
      commandDescription: 'Add Style to Root Div',
    }
  }
}
