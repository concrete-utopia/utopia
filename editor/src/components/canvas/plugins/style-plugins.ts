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

export function getActivePlugin(editorState: EditorState): StylePlugin {
  if (isTailwindEnabled()) {
    return TailwindPlugin(editorState.tailwindConfig)
  }
  return InlineStylePlugin
}
