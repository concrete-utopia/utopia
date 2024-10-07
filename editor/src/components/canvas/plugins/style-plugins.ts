import type { ElementPath } from 'utopia-shared/src/types'
import type { EditorState } from '../../editor/store/editor-state'
import type { StyleInfoFactory } from '../canvas-strategies/canvas-strategy-types'
import { InlineStylePlugin } from './inline-style-plugin'
import { TailwindPlugin } from './tailwind-style-plugin'
import { isTailwindEnabled } from '../../../core/tailwind/tailwind-compilation'

export interface StylePlugin {
  name: string
  styleInfoFactory: StyleInfoFactory
  normalizeFromInlineStyle: (
    editorState: EditorState,
    elementsToNormalize: ElementPath[],
  ) => EditorState
}

export const Plugins = {
  InlineStyle: InlineStylePlugin,
  Tailwind: TailwindPlugin,
} as const

export function getActivePlugin(
  // The `_editorState` is not used now because `isTailwindEnabled` reads a
  // global behind the scenes, it's still passed to make it easy to add new
  // cases here without a big refactor
  _editorState: EditorState,
): StylePlugin {
  if (isTailwindEnabled()) {
    return TailwindPlugin
  }
  return InlineStylePlugin
}
