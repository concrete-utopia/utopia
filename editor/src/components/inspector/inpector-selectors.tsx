import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import { MetadataSubstate } from '../editor/store/store-hook-selectors'

export const metadataSelector = (store: MetadataSubstate): ElementInstanceMetadataMap =>
  store.editor.jsxMetadata

export const selectedViewsSelector = (store: {
  editor: { selectedViews: ElementPath[] }
}): ElementPath[] => store.editor.selectedViews
