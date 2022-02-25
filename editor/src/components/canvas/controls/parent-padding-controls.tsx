import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { useEditorState } from '../../editor/store/store-hook'
import { PaddingControls } from './padding-controls'

export const ParentPaddingControl = React.memo(() => {
  const frame = useEditorState((store) => {
    const selectedView = store.editor.selectedViews[0]
    const parentElement = MetadataUtils.getParent(store.editor.jsxMetadata, selectedView)
    return parentElement?.globalFrame
  }, 'frame')
  const padding = useEditorState((store) => {
    const selectedView = store.editor.selectedViews[0]
    const parentElement = MetadataUtils.getParent(store.editor.jsxMetadata, selectedView)
    return parentElement?.specialSizeMeasurements.padding ?? null
  }, 'padding')

  if (frame == null) {
    return null
  }
  return <PaddingControls padding={padding} frame={frame} />
})
