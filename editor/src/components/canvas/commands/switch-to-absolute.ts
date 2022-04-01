import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { emptyComments, jsxAttributeValue } from '../../../core/shared/element-template'
import { ValueAtPath } from '../../../core/shared/jsx-attributes'
import { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { fastForEach } from '../../../core/shared/utils'
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

export interface HighlightConversion extends BaseCommand {
  type: 'HIGHLIGHT_CONVERSION'
}

export function highlightConversion(transient: TransientOrNot): HighlightConversion {
  return {
    type: 'HIGHLIGHT_CONVERSION',
    transient: transient,
  }
}

export const runHighlightConversion: CommandFunction<HighlightConversion> = (
  editor: EditorState,
  command: HighlightConversion,
) => {
  let elementsToUpdate: ElementPath[] = []
  const selectedViewsWithSiblings = editor.selectedViews.flatMap((path) => {
    return [
      path,
      ...MetadataUtils.getSiblings(editor.jsxMetadata, path).map(
        (metadata) => metadata.elementPath,
      ),
    ]
  })
  fastForEach(selectedViewsWithSiblings, (path) => {
    const canvasFrame = MetadataUtils.getFrameInCanvasCoords(path, editor.jsxMetadata)
    if (
      canvasFrame != null &&
      !editor.selectedViews.some((selectedView) => EP.pathsEqual(path, selectedView))
    ) {
      elementsToUpdate.push(path)
    }
  })

  const elementsThatNeedParentRelative = editor.selectedViews.filter((path) => {
    return !MetadataUtils.findElementByElementPath(editor.jsxMetadata, path)
      ?.specialSizeMeasurements.immediateParentProvidesLayout
  })

  fastForEach(elementsThatNeedParentRelative, (path) => {
    const parentPath = EP.parentPath(path)
    const frame = MetadataUtils.getFrameInCanvasCoords(parentPath, editor.jsxMetadata)
    const hasFrameProps =
      MetadataUtils.findElementByElementPath(editor.jsxMetadata, parentPath)?.props.style?.width !=
        null ||
      MetadataUtils.findElementByElementPath(editor.jsxMetadata, parentPath)?.props.style?.height !=
        null
    if (frame != null && !hasFrameProps) {
      elementsToUpdate.push(path)
    }
  })

  const updatedEditor = elementsToUpdate.reduce((working, path) => {
    const propsToAdd = [
      {
        path: PP.create(['className']),
        value: jsxAttributeValue('utopia-highlight-animation', emptyComments),
      },
    ]
    return applyValuesAtPath(working, path, propsToAdd).editorStateWithChanges
  }, editor)
  return {
    editorStatePatch: {
      projectContents: {
        $set: updatedEditor.projectContents,
      },
      canvas: {
        controls: {
          highlightedElements: { $set: elementsToUpdate },
        },
      },
    },
    commandDescription: `Highlight converted elements`,
  }
}
