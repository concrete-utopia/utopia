import React from 'react'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { useColorTheme } from '../../../../uuiui'
import { useEditorState } from '../../../editor/store/store-hook'
import { useBoundingBox } from '../bounding-box-hooks'
import { getSelectionColor } from '../outline-control'

interface OutlineControlProps {
  localSelectedElements: Array<ElementPath>
}

export const OutlineControl = React.memo<OutlineControlProps>((props) => {
  const colorTheme = useColorTheme()
  const localSelectedElements = props.localSelectedElements

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

  const outlineRef = useBoundingBox(localSelectedElements, (ref, boundingBox) => {
    ref.current.style.left = `calc(${boundingBox.x}px + 0.5px / var(--utopia-canvas-scale))`
    ref.current.style.top = `calc(${boundingBox.y}px + 0.5px / var(--utopia-canvas-scale))`
    ref.current.style.width = `calc(${boundingBox.width}px - 0.5px / var(--utopia-canvas-scale) * 3)`
    ref.current.style.height = `calc(${boundingBox.height}px - 0.5px / var(--utopia-canvas-scale) * 3)`
  })

  if (localSelectedElements.length > 0) {
    return (
      <div
        ref={outlineRef}
        className='role-outline'
        style={{
          position: 'absolute',
          boxSizing: 'border-box',
          boxShadow: `0px 0px 0px calc(1px / var(--utopia-canvas-scale)) ${colors[0]}`,
          pointerEvents: 'none',
          transform: `translate(var(--utopia-canvas-offset-x), var(--utopia-canvas-offset-y))`,
        }}
      />
    )
  }
  return null
})
