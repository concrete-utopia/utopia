import { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { assertNever } from '../../../../core/shared/utils'
import { EditorState, PostActionMenuData } from '../../../editor/store/editor-state'
import { CanvasCommand } from '../../commands/commands'
import { PostActionInteractionType } from '../interaction-state'
import {
  PasteWithPropsPreservedPostActionChoice,
  PasteWithPropsReplacedPostActionChoice,
} from './post-action-paste'

export interface PostActionChoice {
  name: string
  id: string
  run: (
    state: EditorState,
    builtInDependencies: BuiltInDependencies,
    postActionMenuData: PostActionMenuData,
  ) => CanvasCommand[] | null
}

export function generatePostactionChoices(type: PostActionInteractionType): PostActionChoice[] {
  switch (type) {
    case 'PASTE':
      return [PasteWithPropsPreservedPostActionChoice, PasteWithPropsReplacedPostActionChoice]
    default:
      assertNever(type)
  }
}
