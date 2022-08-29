import { emptyComments, jsxAttributeValue } from '../../../core/shared/element-template'
import { ValueAtPath } from '../../../core/shared/jsx-attributes'
import { ElementPath } from '../../../core/shared/project-file-types'
import { EditorState } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { applyValuesAtPath } from './adjust-number-command'
import { BaseCommand, CommandFunction, WhenToRun } from './commands'

export interface ConvertInlineBlock extends BaseCommand {
  type: 'CONVERT_INLINE_BLOCK'
  target: ElementPath
  inlineBlockOrBlock: 'inline-block' | 'block'
}

export function convertInlineBlock(
  whenToRun: WhenToRun,
  target: ElementPath,
  inlineBlockOrBlock: 'inline-block' | 'block',
): ConvertInlineBlock {
  return {
    type: 'CONVERT_INLINE_BLOCK',
    target: target,
    inlineBlockOrBlock: inlineBlockOrBlock,
    whenToRun: whenToRun,
  }
}

export const runConvertInlineBlock: CommandFunction<ConvertInlineBlock> = (
  editorState: EditorState,
  command: ConvertInlineBlock,
) => {
  const propsToUpdate: Array<ValueAtPath> = [
    {
      path: stylePropPathMappingFn('display', ['style']),
      value: jsxAttributeValue(command.inlineBlockOrBlock, emptyComments),
    },
  ]

  const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
    editorState,
    command.target,
    propsToUpdate,
  )

  return {
    editorStatePatches: [propertyUpdatePatch],
    commandDescription: `Switch Display to ${command.inlineBlockOrBlock}`,
  }
}
