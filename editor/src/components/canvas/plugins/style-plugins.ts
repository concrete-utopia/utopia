import type { ElementPath } from 'utopia-shared/src/types'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type {
  InteractionLifecycle,
  StyleInfoFactory,
} from '../canvas-strategies/canvas-strategy-types'
import { InlineStylePlugin } from './inline-style-plugin'
import { TailwindPlugin } from './tailwind-style-plugin'
import {
  getTailwindConfigCached,
  isTailwindEnabled,
} from '../../../core/tailwind/tailwind-compilation'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { assertNever } from '../../../core/shared/utils'

export type StyleUpdate =
  | { type: 'set'; property: string; value: string | number }
  | { type: 'delete'; property: string }

export interface StylePlugin {
  name: string
  styleInfoFactory: StyleInfoFactory
  updateStyles: (
    editorState: EditorState,
    elementPath: ElementPath,
    updates: StyleUpdate[],
  ) => EditorStatePatch[]
}

export function getActivePlugin(editorState: EditorState): StylePlugin {
  if (isFeatureEnabled('Tailwind') && isTailwindEnabled()) {
    return TailwindPlugin(getTailwindConfigCached(editorState))
  }
  return InlineStylePlugin
}

const PropertyPatchValues: Record<string, string> = {
  gap: '0px',
  padding: '0px',
  paddingTop: '0px',
  paddingRight: '0px',
  paddingBottom: '0px',
  paddingLeft: '0px',
}

export function runStyleUpdateMidInteraction(
  editorState: EditorState,
  elementPath: ElementPath,
  updates: StyleUpdate[],
) {
  const withRemovedPropsPatched = updates.map((p): StyleUpdate => {
    switch (p.type) {
      case 'set':
        return p
      case 'delete':
        return { type: 'set', property: p.property, value: PropertyPatchValues[p.property] }
    }
  })
  return InlineStylePlugin.updateStyles(editorState, elementPath, withRemovedPropsPatched)
}

export function runStyleUpdateForStrategy(
  commandLifecycle: InteractionLifecycle,
  editorState: EditorState,
  elementPath: ElementPath,
  updates: StyleUpdate[],
) {
  switch (commandLifecycle) {
    case 'mid-interaction':
      return runStyleUpdateMidInteraction(editorState, elementPath, updates)
    case 'end-interaction':
      return runStyleUpdateForPropertyUpdate(editorState, elementPath, updates)
    default:
      assertNever(commandLifecycle)
  }
}

export function runStyleUpdateForPropertyUpdate(
  editorState: EditorState,
  elementPath: ElementPath,
  updates: StyleUpdate[],
) {
  return getActivePlugin(editorState).updateStyles(editorState, elementPath, updates)
}
