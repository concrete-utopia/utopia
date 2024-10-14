import type { ElementPath } from 'utopia-shared/src/types'
import type { EditorState } from '../../editor/store/editor-state'
import type { StyleInfoFactory } from '../canvas-strategies/canvas-strategy-types'
import { InlineStylePlugin } from './inline-style-plugin'
import { TailwindPlugin } from './tailwind-style-plugin'
import {
  getTailwindConfigCached,
  isTailwindEnabled,
} from '../../../core/tailwind/tailwind-compilation'
import { isFeatureEnabled } from '../../../utils/feature-switches'

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
  if (isFeatureEnabled('Tailwind') && isTailwindEnabled()) {
    return TailwindPlugin(getTailwindConfigCached(editorState))
  }
  return InlineStylePlugin
}
