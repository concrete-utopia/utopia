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

export type StyleUpdate =
  | { type: 'set'; property: string; value: string }
  | { type: 'delete'; property: string }

export interface StylePlugin {
  name: string
  styleInfoFactory: StyleInfoFactory
  updateStyles: (
    editorState: EditorState,
    elementPath: ElementPath,
    updates: StyleUpdate[],
  ) => EditorState
}

export function getActivePlugin(editorState: EditorState): StylePlugin {
  if (isFeatureEnabled('Tailwind') && isTailwindEnabled()) {
    return TailwindPlugin(getTailwindConfigCached(editorState))
  }
  return InlineStylePlugin
}
