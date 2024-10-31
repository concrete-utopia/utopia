import type { CanvasRectangle } from '../../../core/shared/math-utils'
import { isFiniteRectangle, sizesEqual } from '../../../core/shared/math-utils'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { cssPixelLength, type FlexDirection } from '../../inspector/common/css-utils'
import {
  flexChildAndBottomRightProps,
  isHuggingParent,
  prunePropsCommands,
} from '../../inspector/inspector-common'
import type { CanvasFrameAndTarget } from '../canvas-types'
import type { BaseCommand, CanvasCommand, CommandFunctionResult } from './commands'
import { foldAndApplyCommandsSimple } from './commands'
import { setCssLengthProperty, setExplicitCssValue } from './set-css-length-command'
import { showToastCommand } from './show-toast-command'
import { isZeroSizedElement } from '../controls/outline-utils'
import type { HuggingElementContentsStatus } from '../hugging-utils'
import { getHuggingElementContentsStatus } from '../hugging-utils'

export interface IntendedBoundsAndChildrenState extends CanvasFrameAndTarget {
  elementFrame: CanvasRectangle
  huggingElementContentsStatus: HuggingElementContentsStatus
}

export interface PushIntendedBoundsAndUpdateHuggingElements extends BaseCommand {
  type: 'PUSH_INTENDED_BOUNDS_AND_UPDATE_HUGGING_ELEMENTS'
  value: Array<IntendedBoundsAndChildrenState>
}

export function pushIntendedBoundsAndUpdateHuggingElements(
  value: Array<IntendedBoundsAndChildrenState>,
): PushIntendedBoundsAndUpdateHuggingElements {
  return {
    type: 'PUSH_INTENDED_BOUNDS_AND_UPDATE_HUGGING_ELEMENTS',
    whenToRun: 'always',
    value: value,
  }
}

export const runPushIntendedBoundsAndUpdateHuggingElements = (
  editor: EditorState,
  command: PushIntendedBoundsAndUpdateHuggingElements,
): CommandFunctionResult => {
  const { updatedEditor: editorAfterHuggingElements } = applyUpdateResizeHuggingElementsCommands(
    editor,
    command,
  )

  let editorStatePatches: Array<EditorStatePatch> = [
    {
      projectContents: {
        $set: editorAfterHuggingElements.projectContents,
      },
      toasts: {
        $set: editorAfterHuggingElements.toasts,
      },
    },
  ]

  return {
    editorStatePatches: editorStatePatches,
    commandDescription: `Set Intended Bounds for hugging elements ${command.value
      .map((c) => EP.toString(c.target))
      .join(', ')}`,
  }
}

function applyUpdateResizeHuggingElementsCommands(
  editor: EditorState,
  command: PushIntendedBoundsAndUpdateHuggingElements,
): {
  updatedEditor: EditorState
} {
  let commands: Array<CanvasCommand> = []
  for (const frameAndTarget of command.value) {
    const metadata = MetadataUtils.findElementByElementPath(
      editor.jsxMetadata,
      frameAndTarget.target,
    )

    // If the element isn't hugging its parent in either direction, skip this case.
    if (
      metadata == null ||
      !(isHuggingParent(metadata, 'width') || isHuggingParent(metadata, 'height'))
    ) {
      continue
    }

    // If the element still has non-absolute children, skip this case.
    const status = getHuggingElementContentsStatus(editor.jsxMetadata, frameAndTarget.target)
    if (status === 'non-empty') {
      continue
    }

    // If the element is now empty, but used to contain only absolute children, and is zero-sized, skip this case.
    if (
      status === 'empty' &&
      frameAndTarget.huggingElementContentsStatus === 'contains-only-absolute' &&
      isZeroSizedElement(frameAndTarget.elementFrame)
    ) {
      continue
    }

    function setCSSDimension(
      flexDirection: FlexDirection | null,
      prop: 'left' | 'top' | 'width' | 'height',
      value: number,
    ) {
      return setCssLengthProperty(
        'always',
        frameAndTarget.target,
        PP.create('style', prop),
        setExplicitCssValue(cssPixelLength(value)),
        flexDirection,
        'create-if-not-existing',
        'warn-about-replacement',
      )
    }
    if (status === 'contains-only-absolute' || status === 'empty') {
      commands.push(
        ...prunePropsCommands(flexChildAndBottomRightProps, frameAndTarget.target),
        setCSSDimension(
          metadata.specialSizeMeasurements.flexDirection,
          'width',
          frameAndTarget.frame.width,
        ),
        setCSSDimension(
          metadata.specialSizeMeasurements.flexDirection,
          'height',
          frameAndTarget.frame.height,
        ),
        showToastCommand('Added fixed width and height', 'NOTICE', 'added-width-height'), // TODO before merge verify this toast shows up
      )
    }
  }
  return {
    updatedEditor: foldAndApplyCommandsSimple(editor, commands),
  }
}
