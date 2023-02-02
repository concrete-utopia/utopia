import { createSelector } from 'reselect'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
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
