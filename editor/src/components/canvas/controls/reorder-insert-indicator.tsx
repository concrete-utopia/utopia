import * as React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { TemplatePath } from '../../../core/shared/project-file-types'
import { useEditorState } from '../../editor/store/store-hook'

export const ReorderInsertIndicator = (props: {
  target: TemplatePath | null
  showBeforeChildIndex: number
}) => {
  const { x, y, horizontal } = useEditorState((store) => {
    const canvasOffset = store.editor.canvas.roundedCanvasOffset
    const targetParent = MetadataUtils.getElementByTemplatePathMaybe(
      store.editor.jsxMetadataKILLME,
      props.target,
    )
    if (targetParent != null) {
      const childToPutIndexBefore = targetParent.children[props.showBeforeChildIndex]
      if (childToPutIndexBefore != null && childToPutIndexBefore.globalFrame != null) {
        return {
          x: childToPutIndexBefore.globalFrame.x + canvasOffset.x,
          y: childToPutIndexBefore.globalFrame.y + canvasOffset.y,
          horizontal: childToPutIndexBefore.specialSizeMeasurements?.parentFlexDirection === 'row',
        }
      }
    }

    return { x: 0, y: 0, horizontal: false }
  })

  return (
    <div
      style={{
        position: 'absolute',
        backgroundColor: 'red',
        height: horizontal ? 150 : 2,
        width: horizontal ? 2 : 150,
        left: x,
        top: y,
      }}
    />
  )
}
