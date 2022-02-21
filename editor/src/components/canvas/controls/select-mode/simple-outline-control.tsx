import React from 'react'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { useColorTheme } from '../../../../uuiui'
import { useEditorState } from '../../../editor/store/store-hook'
import { useBoundingBoxControl } from '../bounding-box-hooks'
import { getSelectionColor } from '../outline-control'

interface OutlineControlProps {
  localSelectedElements: Array<ElementPath>
}

export const OutlineControl = React.memo<OutlineControlProps>((props) => {
  const colorTheme = useColorTheme()
  const localSelectedElements = props.localSelectedElements

  const scale = useEditorState((store) => store.editor.canvas.scale, 'OutlineControl')
  const outlineOffset = 0.5 / scale
  const outlineWidthHeightOffset = -outlineOffset * 3

  const colors = useEditorState((store) => {
    return localSelectedElements.map((path) =>
      getSelectionColor(
        path,
        store.editor.jsxMetadata,
        store.editor.focusedElementPath,
        colorTheme,
      ),
    )
  }, 'OutlineControl colors')

  const outlineRef = useBoundingBoxControl(localSelectedElements, (ref, boundingBox) => {
    ref.current.style.left = boundingBox.x + outlineOffset + 'px'
    ref.current.style.top = boundingBox.y + outlineOffset + 'px'
    ref.current.style.width = boundingBox.width + outlineWidthHeightOffset + 'px'
    ref.current.style.height = boundingBox.height + outlineWidthHeightOffset + 'px'
  })

  if (localSelectedElements.length > 0) {
    return (
      <div
        ref={outlineRef}
        className='role-outline'
        style={{
          position: 'absolute',
          boxSizing: 'border-box',
          boxShadow: `0px 0px 0px ${1 / scale}px ${colors[0]}`,
          pointerEvents: 'none',
          transform: `translate(var(--utopia-canvas-offset-x), var(--utopia-canvas-offset-y))`,
        }}
      />
    )
  }
  return null
})
