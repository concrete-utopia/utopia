import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { ProjectContentTreeRoot } from '../../assets'
import { EditorDispatch } from '../action-types'
import {
  AllElementProps,
  DerivedState,
  EditorState,
  EditorStateCanvas,
  EditorStorePatched,
  EditorStoreShared,
  emptyGithubData,
  emptyGithubSettings,
  ThemeSetting,
} from './editor-state'

export interface ProjectContentSubstate {
  editor: {
    projectContents: ProjectContentTreeRoot
  }
}

export interface MetadataSubstate {
  editor: {
    selectedViews: Array<ElementPath> // duplicated from SelectedHighlightedViewsSubstate, for convenience!
    focusedElementPath: ElementPath | null
    spyMetadata: ElementInstanceMetadataMap // this is coming from the canvas spy report.
    domMetadata: ElementInstanceMetadataMap // this is coming from the dom walking report.
    jsxMetadata: ElementInstanceMetadataMap // this is a merged result of the two above.
    allElementProps: AllElementProps // the final, resolved, static props value for each element. // This is the counterpart of jsxMetadata. we only update allElementProps when we update jsxMetadata
    // _currentAllElementProps_KILLME: AllElementProps // This is the counterpart of domMetadata and spyMetadata. we update _currentAllElementProps_KILLME every time we update domMetadata/spyMetadata
  }
}

export interface SelectedViewsSubstate {
  editor: {
    selectedViews: Array<ElementPath>
  }
}

export interface FocusedElementPathSubstate {
  editor: {
    focusedElementPath: ElementPath | null
  }
}

export interface HighlightedViewsSubstate {
  editor: {
    // selectedViews: Array<ElementPath>
    highlightedViews: Array<ElementPath>
    hoveredViews: Array<ElementPath>
  }
}

export interface CanvasSubstate {
  editor: {
    canvas: Omit<EditorStateCanvas, 'realCanvasOffset' | 'roundedCanvasOffset'>
  }
}

export const canvasSubstateKeys = [
  'elementsToRerender',
  'visible',
  'dragState',
  'interactionSession',
  'snappingThreshold',
  'textEditor',
  'selectionControlsVisible',
  'cursor',
  'duplicationState',
  'base64Blobs',
  'mountCount',
  'canvasContentInvalidateCount',
  'domWalkerInvalidateCount',
  'openFile',
  'scrollAnimation',
  'transientProperties',
  'resizeOptions',
  'domWalkerAdditionalElementsToUpdate',
  'controls',
] as const

export interface CanvasOffsetSubstate {
  editor: {
    canvas: Pick<EditorStateCanvas, 'realCanvasOffset' | 'roundedCanvasOffset' | 'scale'>
  }
}

export interface DerivedSubstate {
  derived: DerivedState
}

export interface DispatchSubstate {
  dispatch: EditorDispatch
}

export interface ThemeSubstate {
  userState: { themeConfig: ThemeSetting | null }
}

export type GithubSubstateKeys =
  | 'githubSettings'
  | 'githubOperations'
  | 'githubChecksums'
  | 'githubData'
  | 'assetChecksums'

export type GithubSubstate = {
  editor: Pick<EditorState, GithubSubstateKeys>
}

export type BuiltInDependenciesSubstate = {
  builtInDependencies: EditorStoreShared['builtInDependencies']
}

export type UserStateSubstate = {
  userState: EditorStoreShared['userState']
}

export type CanvasAndMetadataSubstate = {
  editor: Pick<EditorState, 'jsxMetadata'>
} & CanvasSubstate

export const emptyGithubSubstate: GithubSubstate = {
  editor: {
    githubSettings: emptyGithubSettings(),
    githubOperations: [],
    githubChecksums: null,
    githubData: emptyGithubData(),
    assetChecksums: {},
  },
}

export type RestOfEditorState = Omit<
  EditorState,
  | 'projectContents'
  | 'canvas'
  | 'jsxMetadata'
  | 'allElementProps'
  | 'spyMetadata'
  | 'domMetadata'
  | 'selectedViews'
  | 'highlightedViews'
  | 'hoveredViews'
  | '_currentAllElementProps_KILLME'
  | 'focusedElementPath'
  | GithubSubstateKeys
> // not comprehensive

export const restOfEditorStateKeys: ReadonlyArray<keyof RestOfEditorState> = [
  'id',
  'vscodeBridgeId',
  'forkedFromProjectId',
  'appID',
  'projectName',
  'projectDescription',
  'projectVersion',
  'isLoaded',
  'branchContents',
  'codeResultCache',
  'propertyControlsInfo',
  'nodeModules',
  'hiddenInstances',
  'displayNoneInstances',
  'warnedInstances',
  'lockedElements',
  'mode',
  'focusedPanel',
  'keysPressed',
  'mouseButtonsPressed',
  'openPopupId',
  'toasts',
  'cursorStack',
  'leftMenu',
  'rightMenu',
  'interfaceDesigner',
  'floatingInsertMenu',
  'inspector',
  'fileBrowser',
  'dependencyList',
  'genericExternalResources',
  'googleFontsResources',
  'projectSettings',
  'navigator',
  'topmenu',
  'preview',
  'home',
  'lastUsedFont',
  'modal',
  'localProjectList',
  'projectList',
  'showcaseProjects',
  'codeEditingEnabled',
  'codeEditorErrors',
  'thumbnailLastGenerated',
  'pasteTargetsToIgnore',
  'parseOrPrintInFlight',
  'safeMode',
  'saveError',
  'vscodeBridgeReady',
  'vscodeReady',
  'config',
  'vscodeLoadingScreenVisible',
  'indexedDBFailed',
  'forceParseFiles',
  'imageDragSessionState',
  'refreshingDependencies',
] as const

export const restOfStoreKeys: ReadonlyArray<keyof Omit<EditorStorePatched, 'editor' | 'derived'>> =
  ['storeName', 'strategyState', 'history', 'workers', 'persistence', 'alreadySaved']
