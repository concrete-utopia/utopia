import * as React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { TemplatePath } from '../../../core/shared/project-file-types'
import { UtopiaTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'

export const ReorderInsertIndicator: React.FunctionComponent<{
  target: TemplatePath | null
  showAtChildIndex: number
  beforeOrAfter: 'before' | 'after'
}> = (props) => {
  const after = props.beforeOrAfter === 'after'
  const { x, y, horizontal, flow } = useEditorState((store) => {
    const canvasOffset = store.editor.canvas.roundedCanvasOffset
    const targetParent = MetadataUtils.getElementByTemplatePathMaybe(
      store.editor.jsxMetadataKILLME.elements,
      props.target,
    )
    if (targetParent != null && props.target != null) {
      const childToPutIndexBefore = MetadataUtils.getChildrenHandlingGroups(
        store.editor.jsxMetadataKILLME,
        props.target,
        false,
      )[props.showAtChildIndex]
      if (childToPutIndexBefore != null && childToPutIndexBefore.globalFrame != null) {
        const isHorizontal =
          childToPutIndexBefore.specialSizeMeasurements?.parentFlexDirection === 'row'
        return {
          x:
            childToPutIndexBefore.globalFrame.x +
            canvasOffset.x +
            (after && isHorizontal ? childToPutIndexBefore.globalFrame.width : 0),
          y:
            childToPutIndexBefore.globalFrame.y +
            canvasOffset.y +
            (after && !isHorizontal ? childToPutIndexBefore.globalFrame.height : 0),
          horizontal: isHorizontal,
          flow: MetadataUtils.isFlowElement(childToPutIndexBefore),
        }
      }
    }

    return { x: 0, y: 0, horizontal: false, flow: false }
  }, 'reorder-insert-indicator')

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
          height: horizontal ? 50 : 2,
          width: horizontal ? 2 : 50,
          left: x,
          top: y,
        }}
      />
    )
  }
}
