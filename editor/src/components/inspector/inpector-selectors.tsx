import { createSelector } from 'reselect'
import { ElementPathTrees } from '../../core/shared/element-path-tree'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import { useRefEditorState } from '../editor/store/store-hook'
import { MetadataSubstate, SelectedViewsSubstate } from '../editor/store/store-hook-substore-types'
import {
  DefaultFlexDirection,
  detectFlexAlignJustifyContent,
  detectFlexDirection,
  detectPackedSpacedSetting,
  numberOfFlexContainers,
} from './inspector-common'

export const metadataSelector = (store: MetadataSubstate): ElementInstanceMetadataMap =>
  store.editor.jsxMetadata

export const pathTreesSelector = (store: MetadataSubstate): ElementPathTrees =>
  store.editor.elementPathTree

export const selectedViewsSelector = (store: SelectedViewsSubstate): ElementPath[] =>
  store.editor.selectedViews

export const flexDirectionSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  (metadata, selectedViews) =>
    selectedViews.length === 0
      ? DefaultFlexDirection
      : detectFlexDirection(metadata, selectedViews),
)

export const justifyAlignSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  detectFlexAlignJustifyContent,
)

export const packedFlexSettingSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  detectPackedSpacedSetting,
)

export const numberOfFlexContainersSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  numberOfFlexContainers,
)

export function useComputedSizeRef(prop: 'width' | 'height'): { readonly current: number | null } {
  return useRefEditorState((store) => {
    const metadata = metadataSelector(store)
    const elementPath = selectedViewsSelector(store)[0]
    if (elementPath == null) {
      return null
    }

    const localFrame = MetadataUtils.getFrameOrZeroRect(elementPath, metadata)
    return localFrame[prop]
  })
}
