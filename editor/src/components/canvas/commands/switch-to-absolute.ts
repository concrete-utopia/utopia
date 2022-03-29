import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { emptyComments, jsxAttributeValue } from '../../../core/shared/element-template'
import { ValueAtPath } from '../../../core/shared/jsx-attributes'
import { ElementPath } from '../../../core/shared/project-file-types'
import { convertSelectionToAbsolute } from '../../editor/actions/action-creators'
import { UPDATE_FNS } from '../../editor/actions/actions'

import { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { applyValuesAtPath } from './adjust-number-command'
import type { BaseCommand, CommandFunction, TransientOrNot } from './commands'

export interface SwitchToAbsolute extends BaseCommand {
  type: 'SWITCH_TO_ABSOLUTE'
  value: Array<ElementPath>
}

export function switchToAbsolute(
  transient: TransientOrNot,
  value: Array<ElementPath>,
): SwitchToAbsolute {
  return {
    type: 'SWITCH_TO_ABSOLUTE',
    transient: transient,
    value: value,
  }
}

export const runSwitchToAbsolute: CommandFunction<SwitchToAbsolute> = (
  editor: EditorState,
  command: SwitchToAbsolute,
) => {
  // const updatedProjectContents = UPDATE_FNS.CONVERT_SELECTION_TO_ABSOLUTE(
  //   convertSelectionToAbsolute(),
  //   editor,
  // ).projectContents

  const target = editor.selectedViews[0]
  const frame = MetadataUtils.getFrame(target, editor.jsxMetadata)
  let propsToAdd: Array<ValueAtPath> = [
    {
      path: stylePropPathMappingFn('position', ['style']),
      value: jsxAttributeValue('absolute', emptyComments),
    },
  ]
  if (frame != null) {
    propsToAdd = [
      {
        path: stylePropPathMappingFn('left', ['style']),
        value: jsxAttributeValue(frame.x, emptyComments),
      },
      {
        path: stylePropPathMappingFn('top', ['style']),
        value: jsxAttributeValue(frame.y, emptyComments),
      },
      {
        path: stylePropPathMappingFn('width', ['style']),
        value: jsxAttributeValue(frame.width, emptyComments),
      },
      {
        path: stylePropPathMappingFn('height', ['style']),
        value: jsxAttributeValue(frame.height, emptyComments),
      },
      {
        path: stylePropPathMappingFn('position', ['style']),
        value: jsxAttributeValue('absolute', emptyComments),
      },
    ]
  }

  const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(editor, target, propsToAdd)
  return {
    editorStatePatch: propertyUpdatePatch,
    commandDescription: `Switch to Absolute: ${command.value.map(EP.toString).join(', ')}`,
  }
}
