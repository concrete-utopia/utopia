import React from 'react'
import * as EP from '../../../../core/shared/element-path'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { when } from '../../../../utils/react-conditionals'
import { useColorTheme } from '../../../../uuiui'
import { useEditorState } from '../../../editor/store/store-hook'
import { useBoundingBox } from '../bounding-box-hooks'
import { getSelectionColor } from '../outline-control'

interface MultiSelectOutlineControlProps {
  localSelectedElements: Array<ElementPath>
}

export const MultiSelectOutlineControl = React.memo<MultiSelectOutlineControlProps>((props) => {
  const localSelectedElements = props.localSelectedElements
  return (
    <>
      {[
        <OutlineControl
          key='multiselect-outline'
          targets={localSelectedElements}
          color='multiselect-bounds'
        />,
        ...localSelectedElements.map((path) => (
          <OutlineControl key={EP.toString(path)} targets={[path]} color='primary' />
        )),
      ]}
    </>
  )
})

interface OutlineControlProps {
  targets: Array<ElementPath>
  color: 'primary' | 'multiselect-bounds'
}

const OutlineControl = React.memo<OutlineControlProps>((props) => {
  const colorTheme = useColorTheme()
  const targets = props.targets

  const colors = useEditorState((store) => {
    return targets.map((path) =>
      getSelectionColor(
        path,
        store.editor.jsxMetadata,
        store.editor.focusedElementPath,
        colorTheme,
      ),
    )
  }, 'OutlineControl colors')

  const outlineRef = useBoundingBox(targets, (ref, boundingBox) => {
    ref.current.style.left = `calc(${boundingBox.x}px + 0.5px / var(--utopia-canvas-scale))`
    ref.current.style.top = `calc(${boundingBox.y}px + 0.5px / var(--utopia-canvas-scale))`
    ref.current.style.width = `calc(${boundingBox.width}px - 0.5px / var(--utopia-canvas-scale) * 3)`
    ref.current.style.height = `calc(${boundingBox.height}px - 0.5px / var(--utopia-canvas-scale) * 3)`
  })

  const color =
    props.color === 'primary' ? colors[0] : colorTheme.canvasSelectionSecondaryOutline.value

  if (targets.length > 0) {
    return (
      <div
        ref={outlineRef}
        className='role-outline'
        style={{
          position: 'absolute',
          boxSizing: 'border-box',
          boxShadow: `0px 0px 0px calc(1px / var(--utopia-canvas-scale)) ${color}`,
          pointerEvents: 'none',
          transform: `translate(var(--utopia-canvas-offset-x), var(--utopia-canvas-offset-y))`,
        }}
      />
    )
  }
  return null
})
