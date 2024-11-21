import type { ElementPath } from 'utopia-shared/src/types'
import { assertNever } from '../../../core/shared/utils'
import type { EditorState } from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunctionResult } from './commands'
import type { EditorStateWithPatches } from '../plugins/style-plugins'
import { getTailwindConfigCached } from '../../../core/tailwind/tailwind-compilation'
import * as TWindConversionUtils from './inline-style-tailwind-conversion-utils'

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

function convertTailwindToInlineStyle(
  editorState: EditorState,
  elementPaths: ElementPath[],
): EditorStateWithPatches {
  const tailwind = 'p-2 hover:w-4 min-h-inherit inset-x-0'
  const parser = TWindConversionUtils.createTailwindParser(getTailwindConfigCached(editorState))
  const styles = parser.classesToCss(new Set(tailwind.split(' ')))
  if (styles == null) {
    return {
      editorStateWithChanges: editorState,
      editorStatePatches: [],
    }
  }
  const inlineStyle = styles.reduce((acc, style) => {
    acc[style.prop] = style.value
    return acc
  }, {} as Record<string, string | number>)

  console.info('convertTailwindToInlineStyle', inlineStyle)

  return {
    editorStateWithChanges: editorState,
    editorStatePatches: [],
  }
}

function convertInlineStyleToTailwind(
  editorState: EditorState,
  elementPaths: ElementPath[],
): EditorStateWithPatches {
  const result = TWindConversionUtils.convertInlineStyleToTailwind({
    display: 'flex',
    flexDirection: 'column',
    alignItems: 'center',
    justifyContent: 'center',
    width: '100%',
    height: '50%',
    margin: '0 !important',
    backgroundColor: 'transparent',
    minHeight: 'inherit',
  })
  console.info('convertInlineStyleToTailwind', result)

  return {
    editorStateWithChanges: editorState,
    editorStatePatches: [],
  }
}

function runConversionWithStylePluginsViaStyleInfo(
  editorState: EditorState,
  elementPaths: ElementPath[],
  direction: 'TO_INLINE_STYLE' | 'TO_TAILWIND',
): EditorStateWithPatches {
  switch (direction) {
    case 'TO_INLINE_STYLE':
      return convertTailwindToInlineStyle(editorState, elementPaths)
    case 'TO_TAILWIND':
      return convertInlineStyleToTailwind(editorState, elementPaths)
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
