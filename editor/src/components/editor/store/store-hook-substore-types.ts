import { uniq } from '../../../core/shared/array-utils'
import { omit, pick } from '../../../core/shared/object-utils'
import type { EditorDispatch } from '../action-types'
import type {
  DerivedState,
  EditorState,
  EditorStorePatched,
  EditorStoreShared,
  ThemeSetting,
} from './editor-state'
import { EmptyEditorStateForKeysOnly } from './store-hook-substore-helpers'

export type Substates = {
  metadata: MetadataSubstate
  selectedViews: SelectedViewsSubstate
  focusedElement: FocusedElementPathSubstate
  highlightedHoveredViews: HighlightedHoveredViewsSubstate
  projectContents: ProjectContentSubstate
  canvas: CanvasSubstate
  canvasOffset: CanvasOffsetSubstate
  derived: { derived: DerivedState }
  restOfEditor: RestOfEditorState
  restOfStore: Omit<EditorStorePatched, 'editor' | 'derived'>
  /**@deprecated hurts performance, please avoid using it */
  fullStore: EditorStorePatched
  theme: ThemeSubstate
  github: GithubSubstate
  builtInDependencies: BuiltInDependenciesSubstate
  userState: UserStateSubstate
  projectServerState: ProjectServerStateSubstate
  variablesInScope: VariablesInScopeSubstate
  propertyControlsInfo: PropertyControlsInfoSubstate
  metadataAndPropertyControlsInfo: MetadataAndPropertyControlsInfoSubstate
}

export type StoreKey = keyof Substates

// ProjectContentSubstate
export const projectContentsKeys = ['projectContents'] as const
const emptyProjectContents = {
  editor: pick(projectContentsKeys, EmptyEditorStateForKeysOnly),
} as const
export type ProjectContentSubstate = typeof emptyProjectContents

// MetadataSubstate
export const metadataSubstateKeys = [
  'selectedViews',
  'focusedElementPath',
  'spyMetadata',
  'domMetadata',
  'jsxMetadata',
  'elementPathTree',
  'allElementProps',
] as const
const emptyMetadataSubstate = {
  editor: pick(metadataSubstateKeys, EmptyEditorStateForKeysOnly),
} as const
export type MetadataSubstate = typeof emptyMetadataSubstate

// SelectedViewsSubstate
export const selectedViewsSubstateKeys = ['selectedViews'] as const
const emptySelectedViewsSubstate = {
  editor: pick(selectedViewsSubstateKeys, EmptyEditorStateForKeysOnly),
} as const
export type SelectedViewsSubstate = typeof emptySelectedViewsSubstate

// FocusedElementPathSubstate
export const focusedElementPathSubstateKeys = ['focusedElementPath'] as const
const emptyFocusedElementPathSubstate = {
  editor: pick(focusedElementPathSubstateKeys, EmptyEditorStateForKeysOnly),
} as const
export type FocusedElementPathSubstate = typeof emptyFocusedElementPathSubstate

// HighlightedHoveredViewsSubstate
export const highlightedHoveredViewsSubstateKeys = ['highlightedViews', 'hoveredViews'] as const
const emptyHighlightedHoveredViewsSubstate = {
  editor: pick(highlightedHoveredViewsSubstateKeys, EmptyEditorStateForKeysOnly),
} as const
export type HighlightedHoveredViewsSubstate = typeof emptyHighlightedHoveredViewsSubstate

// CanvasOffsetSubstate
export const canvasOffsetSubstateKeys = [
  'realCanvasOffset',
  'roundedCanvasOffset',
  'scale',
] as const
const emptyCanvasOffsetSubstate = {
  editor: { canvas: pick(canvasOffsetSubstateKeys, EmptyEditorStateForKeysOnly.canvas) },
} as const
export type CanvasOffsetSubstate = typeof emptyCanvasOffsetSubstate

// CanvasScaleSubstate
export const canvasScaleSubstateKeys = ['scale'] as const
const emptyCanvasScaleSubstate = {
  editor: { canvas: pick(['scale'], EmptyEditorStateForKeysOnly.canvas) },
} as const
export type CanvasScaleSubstate = typeof emptyCanvasScaleSubstate

// CanvasSubstate
const canvasKey = ['canvas'] as const
const emptyCanvasSubstate = {
  editor: {
    canvas: omit(
      [
        // TODO how to use the type of canvasOffsetSubstateKeys here?
        'realCanvasOffset',
        'roundedCanvasOffset',
      ],
      EmptyEditorStateForKeysOnly.canvas,
    ),
  },
} as const
export type CanvasSubstate = typeof emptyCanvasSubstate
export const canvasSubstateKeys = Object.keys(emptyCanvasSubstate.editor.canvas) as Array<
  keyof CanvasSubstate['editor']['canvas']
>

// VariablesInScopeSubstate
export const variablesInScopeSubstateKeys = [
  'variablesInScope',
  'selectedViews',
  'jsxMetadata',
] as const
const emptyVariablesInScopeSubstate = {
  editor: pick(variablesInScopeSubstateKeys, EmptyEditorStateForKeysOnly),
} as const
export type VariablesInScopeSubstate = typeof emptyVariablesInScopeSubstate

// MultiplayerSubstate
export const multiplayerSubstateKeys = ['collaborators'] as const
const emptyMultiplayerSubstate = {
  editor: pick(multiplayerSubstateKeys, EmptyEditorStateForKeysOnly),
} as const
export type MultiplayerSubstate = typeof emptyMultiplayerSubstate

// property controls info
export const propertyControlsInfoSubstateKeys = [
  'projectContents',
  'propertyControlsInfo',
  'selectedViews',
] as const
const propertyControlsInfoSubstate = {
  editor: pick(propertyControlsInfoSubstateKeys, EmptyEditorStateForKeysOnly),
} as const
export type PropertyControlsInfoSubstate = typeof propertyControlsInfoSubstate

export type MetadataAndPropertyControlsInfoSubstate = MetadataSubstate &
  PropertyControlsInfoSubstate

export interface DerivedSubstate {
  derived: DerivedState
}

export interface DispatchSubstate {
  dispatch: EditorDispatch
}

export interface ThemeSubstate {
  userState: { themeConfig: ThemeSetting | null }
}

// GithubSubstate
export const githubSubstateKeys = ['githubSettings', 'githubOperations', 'githubData'] as const
export const emptyGithubSubstate = {
  editor: pick(githubSubstateKeys, EmptyEditorStateForKeysOnly),
} as const
export type GithubSubstate = typeof emptyGithubSubstate

// All the EditorState substate keys
const editorSubstatesKeysCollected = uniq([
  ...projectContentsKeys,
  ...metadataSubstateKeys,
  ...selectedViewsSubstateKeys,
  ...focusedElementPathSubstateKeys,
  ...highlightedHoveredViewsSubstateKeys,
  ...canvasKey,
  ...multiplayerSubstateKeys,
])
const emptyRestOfEditorState = {
  editor: omit(editorSubstatesKeysCollected, EmptyEditorStateForKeysOnly),
} as const
export type RestOfEditorState = typeof emptyRestOfEditorState
export const restOfEditorStateKeys = Object.keys(emptyRestOfEditorState.editor) as Array<
  keyof RestOfEditorState['editor']
>

export type BuiltInDependenciesSubstate = {
  builtInDependencies: EditorStoreShared['builtInDependencies']
}

export type UserStateSubstate = {
  userState: EditorStoreShared['userState']
}

export type CanvasAndMetadataSubstate = {
  editor: Pick<EditorState, 'jsxMetadata'>
} & CanvasSubstate

export type ProjectContentAndMetadataSubstate = ProjectContentSubstate & MetadataSubstate

export type ProjectContentAndMetadataAndVariablesInScopeSubstate = ProjectContentSubstate &
  MetadataSubstate &
  VariablesInScopeSubstate

export type NavigatorSubstate = {
  editor: Pick<EditorState, 'navigator'>
}

export type PostActionInteractionSessionSubstate = {
  postActionInteractionSession: EditorStoreShared['postActionInteractionSession']
}

export interface ProjectServerStateSubstate {
  projectServerState: EditorStoreShared['projectServerState']
}

export interface OnlineStateSubstate {
  onlineState: EditorStoreShared['onlineState']
}

export const restOfStoreKeys: ReadonlyArray<keyof Omit<EditorStorePatched, 'editor' | 'derived'>> =
  [
    'storeName',
    'strategyState',
    'history',
    'userState',
    'workers',
    'persistence',
    'builtInDependencies',
    'saveCountThisSession',
    'projectServerState',
    'onlineState',
  ]
