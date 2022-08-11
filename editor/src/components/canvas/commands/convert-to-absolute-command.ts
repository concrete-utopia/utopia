import * as EP from '../../../core/shared/element-path'
import { emptyComments, jsxAttributeValue } from '../../../core/shared/element-template'
import { ValueAtPath } from '../../../core/shared/jsx-attributes'
import { ElementPath } from '../../../core/shared/project-file-types'
import { EditorState } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { applyValuesAtPath } from './adjust-number-command'
import { BaseCommand, CommandFunction, WhenToRun } from './commands'

export interface ConvertToAbsolute extends BaseCommand {
  type: 'CONVERT_TO_ABSOLUTE'
  target: ElementPath
}

export function convertToAbsolute(transient: WhenToRun, target: ElementPath): ConvertToAbsolute {
  return {
    type: 'CONVERT_TO_ABSOLUTE',
    whenToRun: transient,
    target: target,
  }
}

export const runConvertToAbsolute: CommandFunction<ConvertToAbsolute> = (
  editorState: EditorState,
  command: ConvertToAbsolute,
) => {
  const propsToUpdate: Array<ValueAtPath> = [
    {
      path: stylePropPathMappingFn('position', ['style']),
      value: jsxAttributeValue('absolute', emptyComments),
    },
  ]

  const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
    editorState,
    command.target,
    propsToUpdate,
  )

  return {
    editorStatePatches: [propertyUpdatePatch],
    commandDescription: 'Switch Position to Absolute',
  }
}
