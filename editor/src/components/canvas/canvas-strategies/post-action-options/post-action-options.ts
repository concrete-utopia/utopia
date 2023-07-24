import type { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { stripNulls } from '../../../../core/shared/array-utils'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import type { CanvasPoint } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import type { IndexPosition } from '../../../../utils/utils'
import type { EditorAction } from '../../../editor/action-types'
import {
  executePostActionMenuChoice,
  startPostActionSession,
} from '../../../editor/actions/action-creators'
import type {
  AllElementProps,
  DerivedState,
  EditorState,
  InternalClipboard,
  NavigatorReparentPostActionMenuData,
  PasteToReplacePostActionMenuData,
  PostActionMenuData,
} from '../../../editor/store/editor-state'
import type { CanvasCommand } from '../../commands/commands'
import {
  NavigatorReparentPropsPreservedPostActionChoice,
  NavigatorReparentPropsReplacedPostActionChoice,
} from './navigator-reparent'
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
    case 'NAVIGATOR_REPARENT':
      return stripNulls([
        NavigatorReparentPropsReplacedPostActionChoice(data),
        NavigatorReparentPropsPreservedPostActionChoice(data),
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

  const defaultChoice =
    PasteToReplaceWithPropsReplacedPostActionChoice(pasteToReplacePostActionMenuData) ??
    PasteToReplaceWithPropsPreservedPostActionChoice(pasteToReplacePostActionMenuData)

  if (defaultChoice != null) {
    return [
      startPostActionSession(pasteToReplacePostActionMenuData),
      executePostActionMenuChoice(defaultChoice),
    ]
  }
  return null
}

export function createNavigatorReparentPostActionActions(
  dragSources: Array<ElementPath>,
  targetParent: ElementPath,
  indexPosition: IndexPosition,
  canvasViewportCenter: CanvasPoint,
  jsxMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
): Array<EditorAction> {
  const navigatorReparentPostActionMenuData: NavigatorReparentPostActionMenuData = {
    type: 'NAVIGATOR_REPARENT',
    dragSources: dragSources,
    targetParent: targetParent,
    indexPosition: indexPosition,
    canvasViewportCenter: canvasViewportCenter,
    jsxMetadata: jsxMetadata,
    allElementProps: allElementProps,
  }

  const defaultChoice =
    NavigatorReparentPropsReplacedPostActionChoice(navigatorReparentPostActionMenuData) ??
    NavigatorReparentPropsPreservedPostActionChoice(navigatorReparentPostActionMenuData)

  if (defaultChoice != null) {
    return [
      startPostActionSession(navigatorReparentPostActionMenuData),
      executePostActionMenuChoice(defaultChoice),
    ]
  }
  return []
}
