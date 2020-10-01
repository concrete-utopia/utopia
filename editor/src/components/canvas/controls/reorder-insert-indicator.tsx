import * as React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { TemplatePath } from '../../../core/shared/project-file-types'
import { useEditorState } from '../../editor/store/store-hook'

export const ReorderInsertIndicator = (props: {
  target: TemplatePath | null
  showBeforeChildIndex: number
}) => {
  const { x, y } = useEditorState((store) => {
    const targetParent = MetadataUtils.getElementByTemplatePathMaybe(
      store.editor.jsxMetadataKILLME,
      props.target,
    )
    if (targetParent != null) {
      const childToPutIndexBefore = targetParent.children[props.showBeforeChildIndex]
      if (childToPutIndexBefore != null && childToPutIndexBefore.globalFrame != null) {
        return { x: childToPutIndexBefore.globalFrame.x, y: childToPutIndexBefore.globalFrame.y }
      }
    }

    return { x: 0, y: 0 }
  })

  return (
    <div
      style={{
        position: 'absolute',
        backgroundColor: 'red',
        height: 150,
        width: 2,
        left: x,
        top: y,
      }}
    />
  )
}
