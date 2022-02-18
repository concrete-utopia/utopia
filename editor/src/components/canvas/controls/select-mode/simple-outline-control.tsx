import React from 'react'
import * as EP from '../../../../core/shared/element-path'
import { boundingRectangleArray } from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { useColorTheme } from '../../../../uuiui'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { findFramesFromDOM, useMutationObserver } from '../observer-hooks'
import { getSelectionColor } from '../outline-control'

const OutlineOffset = 0.5
const OutlineWidthHeightOffset = -OutlineOffset * 3

interface OutlineControlProps {
  selectedElements: Array<ElementPath>
}

export const OutlineControl = React.memo<OutlineControlProps>((props) => {
  const colorTheme = useColorTheme()
  const selectedElements = props.selectedElements
  const outlineRef = React.useRef<HTMLDivElement>(null)
  const selectedElementsRef = React.useRef(selectedElements) // TODO new-canvas-controls@localSelectedViews should be atom-like so we can get a live ref to it
  selectedElementsRef.current = selectedElements

  const scale = useEditorState((store) => store.editor.canvas.scale, 'OutlineControl')
  const colors = useEditorState((store) => {
    return selectedElementsRef.current.map((path) =>
      getSelectionColor(
        path,
        store.editor.jsxMetadata,
        store.editor.focusedElementPath,
        colorTheme,
      ),
    )
  }, 'OutlineControl colors')

  const observerCallback = React.useCallback(() => {
    const frames = findFramesFromDOM(selectedElementsRef.current)
    const boundingBox = boundingRectangleArray(frames)
    if (boundingBox != null && outlineRef.current != null) {
      outlineRef.current.style.left = boundingBox.x + OutlineOffset + 'px'
      outlineRef.current.style.top = boundingBox.y + OutlineOffset + 'px'
      outlineRef.current.style.width = boundingBox.width + OutlineWidthHeightOffset + 'px'
      outlineRef.current.style.height = boundingBox.height + OutlineWidthHeightOffset + 'px'
    }
  }, [selectedElementsRef])

  const observerRef = useMutationObserver(selectedElements, observerCallback)

  if (selectedElements.length > 0) {
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
