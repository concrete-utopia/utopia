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
} from './post-action-paste'
import {
  PasteHereWithPropsPreservedPostActionChoice,
  PasteHereWithPropsReplacedPostActionChoice,
} from './post-action-paste-here'

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
    default:
      assertNever(data)
  }
}
