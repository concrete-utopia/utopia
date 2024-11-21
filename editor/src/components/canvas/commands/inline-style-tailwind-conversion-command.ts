import type { ElementPath } from 'utopia-shared/src/types'
import type { BaseCommand, CommandFunctionResult } from './commands'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type { EditorStateWithPatches } from '../plugins/style-plugins'
import { InlineStylePlugin } from '../plugins/inline-style-plugin'
import { TailwindPlugin } from '../plugins/tailwind-style-plugin'
import { getTailwindConfigCached } from '../../../core/tailwind/tailwind-compilation'
import { assertNever } from '../../../core/shared/utils'
import { CssToTailwindTranslator } from 'css-to-tailwind-translator'
import styleObjectToCSSString from 'style-object-to-css-string'
import { getClassNameAttribute } from '../../../core/tailwind/tailwind-options'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getParsedClassList } from '../../../core/tailwind/tailwind-class-list-utils'
import { applyValuesAtPath, deleteValuesAtPath } from './utils/property-utils'
import * as PP from '../../../core/shared/property-path'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'

export function convertInlineStyleToTailwindInner(
  inlineStyle: Record<string, number | string>,
): string {
  const styleString = styleObjectToCSSString(inlineStyle)
  const cssString = `body { ${styleString} }`
  const { data } = CssToTailwindTranslator(cssString)
  return data[0].resultVal
}

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

function convertInlineStyleToTailwind(
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
    patches.push(editorStatePatchToRemove)
    editorStateWithChanges = finalEditorState
  })

  return {
    editorStateWithChanges: editorStateWithChanges,
    editorStatePatches: patches,
  }
}

function toCamelCase(str: string): string {
  return str
    .toLowerCase()
    .replace(/([-_][a-z])/g, (ltr) => ltr.toUpperCase())
    .replace(/[^a-zA-Z]/g, '')
}

function convertTailwindToInlineStyle(
  editorState: EditorState,
  elementPaths: ElementPath[],
): EditorStateWithPatches {
  let patches: EditorStatePatch[] = []
  let editorStateWithChanges: EditorState = editorState

  elementPaths.forEach((elementPath) => {
    const { value: className } = getClassNameAttribute(
      MetadataUtils.getJSXElementFromMetadata(editorState.jsxMetadata, elementPath),
    )
    if (className == null) {
      return
    }

    const parsedClassList = getParsedClassList(className, getTailwindConfigCached(editorState))
    const styleObject = parsedClassList.reduce((style, prop) => {
      if (prop.type === 'unparsed') {
        return style
      }

      prop.ast.valueDef.class.forEach((c) => {
        style[toCamelCase(c)] = prop.ast.value
      })
      return style
    }, {} as Record<string, string>)

    const { editorStateWithChanges: updatedEditorState } = applyValuesAtPath(
      editorState,
      elementPath,
      [
        {
          path: PP.create('style'),
          value: jsExpressionValue(styleObject, emptyComments),
        },
      ],
    )

    const { editorStateWithChanges: finalEditorState, editorStatePatch: editorStatePatchToRemove } =
      deleteValuesAtPath(updatedEditorState, elementPath, [PP.create('className')])

    patches.push(editorStatePatchToRemove)
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
