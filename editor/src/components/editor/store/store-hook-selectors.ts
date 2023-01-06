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
    spyMetadata: ElementInstanceMetadataMap // this is coming from the canvas spy report.
    domMetadata: ElementInstanceMetadataMap // this is coming from the dom walking report.
    jsxMetadata: ElementInstanceMetadataMap // this is a merged result of the two above.
    allElementProps: AllElementProps // the final, resolved, static props value for each element. // This is the counterpart of jsxMetadata. we only update allElementProps when we update jsxMetadata
    _currentAllElementProps_KILLME: AllElementProps // This is the counterpart of domMetadata and spyMetadata. we update _currentAllElementProps_KILLME every time we update domMetadata/spyMetadata
  }
}

export interface SelectedHighlightedViewsSubstate {
  editor: {
    selectedViews: Array<ElementPath>
    highlightedViews: Array<ElementPath>
    hoveredViews: Array<ElementPath>
  }
}

export interface CanvasSubstate {
  editor: {
    canvas: Omit<EditorStateCanvas, 'realCanvasOffset' | 'roundedCanvasOffset'>
  }
}

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

export type EditorStateWOScrollOffset = Omit<EditorStorePatched, 'editor'> & {
  editor: Omit<EditorState, 'canvas'> & {
    canvas: Omit<EditorStateCanvas, 'realCanvasOffset' | 'roundedCanvasOffset'>
  }
}

export type OldEditorState = Omit<
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
> // not comprehensive
