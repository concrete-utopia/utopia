import type { ElementPath } from 'utopia-shared/src/types'
import type { BaseCommand, CommandFunctionResult } from './commands'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type { DeleteCSSProp, EditorStateWithPatches, UpdateCSSProp } from '../plugins/style-plugins'
import { InlineStylePlugin } from '../plugins/inline-style-plugin'
import type { StyleInfo } from '../canvas-types'
import { stringifyStyleInfo } from '../canvas-types'
import { TailwindPlugin } from '../plugins/tailwind-style-plugin'
import { getTailwindConfigCached } from '../../../core/tailwind/tailwind-compilation'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { assertNever } from '../../../core/shared/utils'

export interface InlineStyleTailwindConversionCommand extends BaseCommand {
  type: 'INLINE_STYLE_TAILWIND_CONVERSION'

  direction: 'TO_INLINE_STYLE' | 'TO_TAILWIND'
  elementPaths: ElementPath[]
}

export function inlineStyleTailwindConversionCommand(
  whenToRun: 'always' | 'on-complete',
  direction: 'TO_INLINE_STYLE' | 'TO_TAILWIND',
  elementPaths: ElementPath[],
): InlineStyleTailwindConversionCommand {
  return {
    type: 'INLINE_STYLE_TAILWIND_CONVERSION',
    whenToRun: whenToRun,
    direction: direction,
    elementPaths: elementPaths,
  }
}

function getStyleInfoUpdates(styleInfo: StyleInfo): {
  stylesToAdd: UpdateCSSProp[]
  stylesToRemove: DeleteCSSProp[]
} {
  const styleInfoString = stringifyStyleInfo(styleInfo)
  const stylesToAdd: UpdateCSSProp[] = mapDropNulls(
    ([property, value]) =>
      value == null
        ? null
        : {
            property: property,
            value: value,
            type: 'set',
          },
    Object.entries(styleInfoString),
  )

  const stylesToRemove: DeleteCSSProp[] = stylesToAdd.map((style) => ({
    property: style.property,
    type: 'delete',
  }))

  return { stylesToAdd, stylesToRemove }
}

function convertInlineStyleToTailwindViaStyleInfo(
  editorState: EditorState,
  elementPaths: ElementPath[],
): EditorStateWithPatches {
  let patches: EditorStatePatch[] = []
  let editorStateWithChanges: EditorState = editorState

  elementPaths.forEach((elementPath) => {
    const styleInfo = InlineStylePlugin.styleInfoFactory({
      projectContents: editorState.projectContents,
    })(elementPath)

    if (styleInfo == null) {
      return
    }

    const { stylesToAdd, stylesToRemove } = getStyleInfoUpdates(styleInfo)

    const { editorStateWithChanges: updatedEditorState } = TailwindPlugin(
      getTailwindConfigCached(editorStateWithChanges),
    ).updateStyles(editorStateWithChanges, elementPath, stylesToAdd)

    const { editorStatePatch: editorStatePatchToRemove, editorStateWithChanges: finalEditorState } =
      InlineStylePlugin.updateStyles(updatedEditorState, elementPath, stylesToRemove)

    patches = [editorStatePatchToRemove]
    editorStateWithChanges = finalEditorState
  })

  return {
    editorStateWithChanges: editorStateWithChanges,
    editorStatePatches: patches,
  }
}

function convertTailwindToInlineStyleViaStyleInfo(
  editorState: EditorState,
  elementPaths: ElementPath[],
): EditorStateWithPatches {
  let patches: EditorStatePatch[] = []
  let editorStateWithChanges: EditorState = editorState

  elementPaths.forEach((elementPath) => {
    const styleInfo = TailwindPlugin(
      getTailwindConfigCached(editorStateWithChanges),
    ).styleInfoFactory({
      projectContents: editorStateWithChanges.projectContents,
    })(elementPath)

    if (styleInfo == null) {
      return
    }

    const { stylesToAdd, stylesToRemove } = getStyleInfoUpdates(styleInfo)

    const { editorStateWithChanges: updatedEditorState } = InlineStylePlugin.updateStyles(
      editorStateWithChanges,
      elementPath,
      stylesToAdd,
    )
    const { editorStatePatch: editorStatePatchToRemove, editorStateWithChanges: finalEditorState } =
      TailwindPlugin(getTailwindConfigCached(updatedEditorState)).updateStyles(
        updatedEditorState,
        elementPath,
        stylesToRemove,
      )
    patches = [editorStatePatchToRemove]
    editorStateWithChanges = finalEditorState
  })

  return {
    editorStateWithChanges: editorStateWithChanges,
    editorStatePatches: patches,
  }
}

function runConversionWithStylePluginsViaStyleInfo(
  editorState: EditorState,
  elementPaths: ElementPath[],
  direction: 'TO_INLINE_STYLE' | 'TO_TAILWIND',
): EditorStateWithPatches {
  switch (direction) {
    case 'TO_INLINE_STYLE':
      return convertTailwindToInlineStyleViaStyleInfo(editorState, elementPaths)
    case 'TO_TAILWIND':
      return convertInlineStyleToTailwindViaStyleInfo(editorState, elementPaths)
    default:
      assertNever(direction)
  }
}

export function runInlineStyleTailwindConversionCommand(
  editorState: EditorState,
  command: InlineStyleTailwindConversionCommand,
): CommandFunctionResult {
  const { editorStatePatches } = runConversionWithStylePluginsViaStyleInfo(
    editorState,
    command.elementPaths,
    command.direction,
  )
  return {
    commandDescription: 'Inline Style Tailwind Conversion',
    editorStatePatches: editorStatePatches,
  }
}
