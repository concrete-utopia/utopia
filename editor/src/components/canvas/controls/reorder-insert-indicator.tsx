import * as React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { TemplatePath } from '../../../core/shared/project-file-types'
import { UtopiaTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'

export const ReorderInsertIndicator = (props: {
  target: TemplatePath | null
  showBeforeChildIndex: number
}) => {
  const { x, y, horizontal, flow, size } = useEditorState((store) => {
    const canvasOffset = store.editor.canvas.roundedCanvasOffset
    const targetParent = MetadataUtils.getElementByTemplatePathMaybe(
      store.editor.jsxMetadataKILLME,
      props.target,
    )
    if (targetParent != null) {
      const parentFrame = MetadataUtils.getFrameInCanvasCoords(
        targetParent.templatePath,
        store.editor.jsxMetadataKILLME,
      )
      const childToPutIndexBefore = targetParent.children[props.showBeforeChildIndex]
      if (childToPutIndexBefore != null && childToPutIndexBefore.globalFrame != null) {
        return {
          x: childToPutIndexBefore.globalFrame.x + canvasOffset.x,
          y: childToPutIndexBefore.globalFrame.y + canvasOffset.y,
          horizontal: childToPutIndexBefore.specialSizeMeasurements?.parentFlexDirection === 'row',
          size:
            childToPutIndexBefore.specialSizeMeasurements?.parentFlexDirection === 'row'
              ? (parentFrame?.height ?? 0) / 2
              : (parentFrame?.width ?? 0) / 2,
          flow:
            childToPutIndexBefore.specialSizeMeasurements?.immediateParentProvidesLayout === false,
        }
      }
    }

    return { x: 0, y: 0, horizontal: false }
  })

  if (flow) {
    return (
      <>
        <div
          style={{
            position: 'absolute',
            backgroundColor: UtopiaTheme.color.brandNeonPink.value,
            height: 15,
            width: 2,
            left: x,
            top: y,
          }}
        />
        <div
          style={{
            position: 'absolute',
            backgroundColor: UtopiaTheme.color.brandNeonPink.value,
            height: 2,
            width: 15,
            left: x,
            top: y,
          }}
        />
      </>
    )
  } else {
    return (
      <div
        style={{
          position: 'absolute',
          backgroundColor: UtopiaTheme.color.brandNeonPink.value,
          height: horizontal ? size : 2,
          width: horizontal ? 2 : size,
          left: x,
          top: y,
        }}
      />
    )
  }
}
