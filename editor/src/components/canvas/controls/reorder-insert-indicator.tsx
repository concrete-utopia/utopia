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
  const { x, y } = useEditorState((store) => {
    const canvasOffset = store.editor.canvas.roundedCanvasOffset
    const targetParent = MetadataUtils.getElementByTemplatePathMaybe(
      store.editor.jsxMetadataKILLME,
      props.target,
    )
    if (targetParent != null) {
      const childToPutIndexBefore = targetParent.children[props.showAtChildIndex]
      if (childToPutIndexBefore != null && childToPutIndexBefore.globalFrame != null) {
        return {
          x:
            childToPutIndexBefore.globalFrame.x +
            canvasOffset.x +
            (after ? childToPutIndexBefore.globalFrame.width : 0),
          y:
            childToPutIndexBefore.globalFrame.y +
            canvasOffset.y +
            (after ? childToPutIndexBefore.globalFrame.height : 0),
        }
      }
    }

    return { x: 0, y: 0, width: 0, height: 0 }
  })

  return (
    <>
      <div
        style={{
          position: 'absolute',
          backgroundColor: UtopiaTheme.color.brandNeonPink.value,
          height: 15,
          width: 2,
          left: after ? x - 2 : x,
          top: after ? y - 15 : y,
        }}
      />
      <div
        style={{
          position: 'absolute',
          backgroundColor: UtopiaTheme.color.brandNeonPink.value,
          height: 2,
          width: 15,
          left: after ? x - 15 : x,
          top: after ? y - 2 : y,
        }}
      />
    </>
  )
}
