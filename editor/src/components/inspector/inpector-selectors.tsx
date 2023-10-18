import { createSelector } from 'reselect'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import type { ElementPathTrees } from '../../core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import type { ElementPath } from '../../core/shared/project-file-types'
import type { AllElementProps } from '../editor/store/editor-state'
import { useRefEditorState } from '../editor/store/store-hook'
import type {
  MetadataSubstate,
  SelectedViewsSubstate,
} from '../editor/store/store-hook-substore-types'
import {
  DefaultFlexDirection,
  detectFlexAlignJustifyContent,
  detectFlexDirection,
  detectPackedSpacedSetting,
  numberOfFlexContainers,
} from './inspector-common'
import { nullIfInfinity, zeroRectangle } from '../../core/shared/math-utils'

export const metadataSelector = (store: MetadataSubstate): ElementInstanceMetadataMap =>
  store.editor.jsxMetadata

export const pathTreesSelector = (store: MetadataSubstate): ElementPathTrees =>
  store.editor.elementPathTree

export const selectedViewsSelector = (store: SelectedViewsSubstate): ElementPath[] =>
  store.editor.selectedViews

export const allElementPropsSelector = (store: MetadataSubstate): AllElementProps =>
  store.editor.allElementProps

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

export function useNonRoundedComputedSizeRef(prop: 'width' | 'height'): {
  readonly current: number | null
} {
  return useRefEditorState((store) => {
    const metadata = metadataSelector(store)
    const elementPath = selectedViewsSelector(store)[0]
    if (elementPath == null) {
      return null
    }

    const localFrame =
      nullIfInfinity(
        MetadataUtils.findElementByElementPath(metadata, elementPath)?.nonRoundedGlobalFrame,
      ) ?? zeroRectangle
    return localFrame[prop]
  })
}
