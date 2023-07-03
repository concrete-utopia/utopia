import { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { stripNulls } from '../../../../core/shared/array-utils'
import { assertNever } from '../../../../core/shared/utils'
import { EditorState, PostActionMenuData } from '../../../editor/store/editor-state'
import { CanvasCommand } from '../../commands/commands'
import {
  PasteWithPropsPreservedPostActionChoice,
  PasteWithPropsReplacedPostActionChoice,
} from './post-action-paste'

export interface PostActionChoice {
  name: string
  id: string
  run: (state: EditorState, builtInDependencies: BuiltInDependencies) => CanvasCommand[] | null
}

export function generatePostactionChoices(data: PostActionMenuData): PostActionChoice[] {
  switch (data.type) {
    case 'PASTE':
      return stripNulls([
        PasteWithPropsReplacedPostActionChoice(data),
        PasteWithPropsPreservedPostActionChoice(data),
      ])
    default:
      assertNever(data.type)
  }
}
