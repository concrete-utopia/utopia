import React from 'react'
import * as EP from '../../../../core/shared/element-path'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { when } from '../../../../utils/react-conditionals'
import { useColorTheme } from '../../../../uuiui'
import { useEditorState } from '../../../editor/store/store-hook'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { getSelectionColor } from '../outline-control'
import { isZeroSizedElement } from '../outline-utils'

interface MultiSelectOutlineControlProps {
  localSelectedElements: Array<ElementPath>
}

export const MultiSelectOutlineControl = React.memo<MultiSelectOutlineControlProps>((props) => {
  const hiddenInstances = useEditorState(
    (store) => store.editor.hiddenInstances,
    'MultiSelectOutlineControl hiddenInstances',
  )
  const localSelectedElements = props.localSelectedElements.filter(
    (sv) => !hiddenInstances.includes(sv) && !EP.isStoryboardPath(sv),
  )
  return (
    <CanvasOffsetWrapper>
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
    </CanvasOffsetWrapper>
  )
})

interface OutlineControlProps {
  targets: Array<ElementPath>
  color: 'primary' | 'multiselect-bounds'
}

const OutlineControl = React.memo<OutlineControlProps>((props) => {
  const colorTheme = useColorTheme()
  const targets = props.targets
  const scale = useEditorState((store) => store.editor.canvas.scale, 'OutlineControl scale')

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

  const outlineRef = useBoundingBox(targets, (ref, boundingBox, canvasScale) => {
    if (isZeroSizedElement(boundingBox)) {
      ref.current.style.display = 'none'
    } else {
      ref.current.style.display = 'block'
      ref.current.style.left = `${boundingBox.x - 0.5 / canvasScale}px`
      ref.current.style.top = `${boundingBox.y - 0.5 / canvasScale}px`
      ref.current.style.width = `${boundingBox.width + 1 / canvasScale}px`
      ref.current.style.height = `${boundingBox.height + 1 / canvasScale}px`
    }
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
          borderColor: color,
          borderWidth: `${1 / scale}px`,
          borderStyle: 'solid',
          pointerEvents: 'none',
        }}
      />
    )
  }
  return null
})
