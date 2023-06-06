import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { OutlineControl } from './select-mode/simple-outline-control'

export const AbsoluteChildrenOutline = React.memo(() => {
  const absoluteChildren = useEditorState(
    Substores.metadata,
    (store) => {
      return store.editor.selectedViews.flatMap((view) => {
        return MetadataUtils.getChildrenPathsOrdered(
          store.editor.jsxMetadata,
          store.editor.elementPathTree,
          view,
        ).filter((child) => {
          const metadata = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, child)
          return MetadataUtils.isPositionAbsolute(metadata)
        })
      })
    },
    'AbsoluteChildrenOutline absoluteChildrenBoundingBox',
  )

  if (absoluteChildren.length === 0) {
    return null
  }
  return (
    <CanvasOffsetWrapper>
      <OutlineControl
        targets={absoluteChildren}
        outlineStyle={'dotted'}
        color='multiselect-bounds'
      />
    </CanvasOffsetWrapper>
  )
})
