import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { OutlineControl } from './select-mode/simple-outline-control'

export const AbsoluteChildrenOutline = React.memo(() => {
  const elementWithAbsoluteChildren = useEditorState(
    Substores.metadata,
    (store) => {
      return store.editor.selectedViews.flatMap((view) => {
        const children = MetadataUtils.getChildrenPaths(store.editor.jsxMetadata, view).filter(
          (child) => {
            const metadata = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, child)
            return MetadataUtils.isPositionAbsolute(metadata)
          },
        )
        return [...children, view]
      })
    },
    'AbsoluteChildrenOutline elementWithAbsoluteChildren',
  )

  if (elementWithAbsoluteChildren.length === 0) {
    return null
  }
  return (
    <CanvasOffsetWrapper>
      <OutlineControl
        targets={elementWithAbsoluteChildren}
        outlineStyle={'dotted'}
        color='multiselect-bounds'
      />
    </CanvasOffsetWrapper>
  )
})
