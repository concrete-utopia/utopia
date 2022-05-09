import { emptyComments, jsxAttributeValue } from '../../../core/shared/element-template'
import { ValueAtPath } from '../../../core/shared/jsx-attributes'
import { LocalRectangle } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { EditorState } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { applyValuesAtPath } from './adjust-number-command'
import { BaseCommand, CommandFunction, TransientOrNot } from './commands'

export interface SetLocalFrameCommand extends BaseCommand {
  type: 'SET_LOCAL_FRAME'
  target: ElementPath
  frame: LocalRectangle
}

export function setLocalFrame(
  transient: TransientOrNot,
  target: ElementPath,
  frame: LocalRectangle,
): SetLocalFrameCommand {
  return {
    type: 'SET_LOCAL_FRAME',
    transient: transient,
    target: target,
    frame: frame,
  }
}

export const runSetLocalFrame: CommandFunction<SetLocalFrameCommand> = (
  editorState: EditorState,
  command: SetLocalFrameCommand,
) => {
  const propsToUpdate: Array<ValueAtPath> = [
    {
      path: stylePropPathMappingFn('position', ['style']),
      value: jsxAttributeValue('absolute', emptyComments),
    },
    {
      path: stylePropPathMappingFn('left', ['style']),
      value: jsxAttributeValue(command.frame.x, emptyComments),
    },
    {
      path: stylePropPathMappingFn('top', ['style']),
      value: jsxAttributeValue(command.frame.y, emptyComments),
    },
    {
      path: stylePropPathMappingFn('width', ['style']),
      value: jsxAttributeValue(command.frame.width, emptyComments),
    },
    {
      path: stylePropPathMappingFn('height', ['style']),
      value: jsxAttributeValue(command.frame.height, emptyComments),
    },
  ]

  const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
    editorState,
    command.target,
    propsToUpdate,
  )

  return {
    editorStatePatches: [propertyUpdatePatch],
    commandDescription: 'the new set frame command',
  }
}
