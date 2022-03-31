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
  const updatedEditor = UPDATE_FNS.CONVERT_SELECTION_TO_ABSOLUTE(
    convertSelectionToAbsolute(),
    editor,
  )

  return {
    editorStatePatch: {
      projectContents: {
        $set: updatedEditor.projectContents,
      },
      canvas: {
        controls: {
          highlightOutlines: {
            $set: updatedEditor.canvas.controls.highlightOutlines,
          },
        },
      },
    },
    commandDescription: `Switch to Absolute: ${command.value.map(EP.toString).join(', ')}`,
  }
}
