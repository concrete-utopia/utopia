import type { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { stripNulls } from '../../../../core/shared/array-utils'
import { assertNever } from '../../../../core/shared/utils'
import type {
  DerivedState,
  EditorState,
  PostActionMenuData,
} from '../../../editor/store/editor-state'
import type { CanvasCommand } from '../../commands/commands'
import {
  PasteWithPropsPreservedPostActionChoice,
  PasteWithPropsReplacedPostActionChoice,
  PasteHereWithPropsPreservedPostActionChoice,
  PasteHereWithPropsReplacedPostActionChoice,
  PasteToReplaceWithPropsReplacedPostActionChoice,
  PasteToReplaceWithPropsPreservedPostActionChoice,
} from './post-action-paste'

export interface PostActionChoice {
  name: string
  id: string
  run: (
    state: EditorState,
    derivedState: DerivedState,
    builtInDependencies: BuiltInDependencies,
  ) => CanvasCommand[] | null
}

export function generatePostactionChoices(data: PostActionMenuData): PostActionChoice[] {
  switch (data.type) {
    case 'PASTE':
      return stripNulls([
        PasteWithPropsReplacedPostActionChoice(data),
        PasteWithPropsPreservedPostActionChoice(data),
      ])
    case 'PASTE_HERE':
      return stripNulls([
        PasteHereWithPropsReplacedPostActionChoice(data),
        PasteHereWithPropsPreservedPostActionChoice(data),
      ])
    case 'PASTE_TO_REPLACE':
      return stripNulls([
        PasteToReplaceWithPropsReplacedPostActionChoice(data),
        PasteToReplaceWithPropsPreservedPostActionChoice(data),
      ])
    default:
      assertNever(data)
  }
}
