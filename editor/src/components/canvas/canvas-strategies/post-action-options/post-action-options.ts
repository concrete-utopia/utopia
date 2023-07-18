import type { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { stripNulls } from '../../../../core/shared/array-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import type { EditorAction } from '../../../editor/action-types'
import {
  executePostActionMenuChoice,
  startPostActionSession,
} from '../../../editor/actions/action-creators'
import type {
  DerivedState,
  EditorState,
  InternalClipboard,
  PasteToReplacePostActionMenuData,
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

export function createPasteToReplacePostActionActions(
  selectedViews: Array<ElementPath>,
  internalClipboard: InternalClipboard,
): Array<EditorAction> | null {
  const pasteToReplacePostActionMenuData: PasteToReplacePostActionMenuData = {
    type: 'PASTE_TO_REPLACE',
    targets: selectedViews,
    internalClipboard: internalClipboard,
  }

  const defaultChoice = stripNulls([
    PasteToReplaceWithPropsReplacedPostActionChoice(pasteToReplacePostActionMenuData),
    PasteToReplaceWithPropsPreservedPostActionChoice(pasteToReplacePostActionMenuData),
  ]).at(0)

  if (defaultChoice != null) {
    return [
      startPostActionSession(pasteToReplacePostActionMenuData),
      executePostActionMenuChoice(defaultChoice),
    ]
  }
  return null
}
