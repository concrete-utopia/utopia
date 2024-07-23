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
  PropsPreservedNavigatorReparentPostActionChoice,
  PropsReplacedNavigatorReparentPostActionChoice,
} from './navigator-reparent'
import {
  PropsPreservedPastePostActionChoice,
  PropsReplacedPastePostActionChoice,
  PropsPreservedPasteHerePostActionChoice,
  PropsReplacedPasteHerePostActionChoice,
  PropsPreservedPasteToReplacePostActionChoice,
  PropsReplacedPasteToReplacePostActionChoice,
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
        PropsReplacedPastePostActionChoice(data),
        PropsPreservedPastePostActionChoice(data),
      ])
    case 'PASTE_HERE':
      return stripNulls([
        PropsReplacedPasteHerePostActionChoice(data),
        PropsPreservedPasteHerePostActionChoice(data),
      ])
    case 'PASTE_TO_REPLACE':
      return stripNulls([
        PropsReplacedPasteToReplacePostActionChoice(data),
        PropsPreservedPasteToReplacePostActionChoice(data),
      ])
    case 'NAVIGATOR_REPARENT':
      return stripNulls([
        PropsReplacedNavigatorReparentPostActionChoice(data),
        PropsPreservedNavigatorReparentPostActionChoice(data),
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
    PropsReplacedPasteToReplacePostActionChoice(pasteToReplacePostActionMenuData) ??
    PropsPreservedPasteToReplacePostActionChoice(pasteToReplacePostActionMenuData)

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
  domReconstructedMetadata: ElementInstanceMetadataMap,
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
    domReconstructedMetadata: domReconstructedMetadata,
  }

  const defaultChoice =
    PropsReplacedNavigatorReparentPostActionChoice(navigatorReparentPostActionMenuData) ??
    PropsPreservedNavigatorReparentPostActionChoice(navigatorReparentPostActionMenuData)

  if (defaultChoice != null) {
    return [
      startPostActionSession(navigatorReparentPostActionMenuData),
      executePostActionMenuChoice(defaultChoice),
    ]
  }
  return []
}
