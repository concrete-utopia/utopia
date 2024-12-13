import { styleStringInArray } from '../../../utils/common-constants'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'
import type { ValueAtPath } from '../../../core/shared/jsx-attributes'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'
import { applyValuesAtPath } from './utils/property-utils'

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
      path: stylePropPathMappingFn('position', styleStringInArray),
      value: jsExpressionValue('absolute', emptyComments),
    },
  ]

  const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
    editorState,
    command.target,
    propsToUpdate,
  )

  const updatedMetadataPatch = addPositionAbsoluteToMetadata(
    editorState.jsxMetadata,
    command.target,
  )

  return {
    editorStatePatches: [propertyUpdatePatch, updatedMetadataPatch],
    commandDescription: 'Switch Position to Absolute',
  }
}

function addPositionAbsoluteToMetadata(
  metadataMap: ElementInstanceMetadataMap,
  target: ElementPath,
): EditorStatePatch {
  const existingMetadata = MetadataUtils.findElementByElementPath(metadataMap, target)
  if (existingMetadata == null) {
    return {}
  }

  return {
    jsxMetadata: {
      [EP.toString(target)]: {
        specialSizeMeasurements: {
          position: { $set: 'absolute' },
        },
      },
    },
  }
}
