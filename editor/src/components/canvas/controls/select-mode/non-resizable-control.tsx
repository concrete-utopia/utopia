import React from 'react'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { NO_OP } from '../../../../core/shared/utils'
import { useColorTheme } from '../../../../uuiui'
import { EditorStorePatched } from '../../../editor/store/editor-state'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import type { EdgePosition } from '../../canvas-types'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

export const NonResizableControlTestId = 'non-resizable-control'

const selectedElementsSelector = (store: { editor: { selectedViews: ElementPath[] } }) =>
  store.editor.selectedViews

export const NonResizableControl = controlForStrategyMemoized(() => {
  const selectedElements = useEditorState(
    Substores.selectedViews,
    selectedElementsSelector,
    'NonResizableControl selectedElements',
  )

  const controlRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    ref.current.style.display = 'block'
    ref.current.style.left = boundingBox.x + 'px'
    ref.current.style.top = boundingBox.y + 'px'
    ref.current.style.width = boundingBox.width + 'px'
    ref.current.style.height = boundingBox.height + 'px'
  })

  const topLeftRef = useBoundingBox(selectedElements, NO_OP)
  const topRightRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    ref.current.style.left = boundingBox.width + 'px'
  })
  const bottomLeftRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    ref.current.style.top = boundingBox.height + 'px'
  })
  const bottomRightRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    ref.current.style.left = boundingBox.width + 'px'
    ref.current.style.top = boundingBox.height + 'px'
  })

  if (selectedElements.length === 0) {
    return null
  }

  return (
    <CanvasOffsetWrapper>
      <div
        ref={controlRef}
        style={{
          position: 'absolute',
          pointerEvents: 'none',
        }}
        data-testid={NonResizableControlTestId}
      >
        <NonResizablePoint ref={topLeftRef} position={{ x: 0, y: 0 }} />
        <NonResizablePoint ref={topRightRef} position={{ x: 1, y: 0 }} />
        <NonResizablePoint ref={bottomLeftRef} position={{ x: 0, y: 1 }} />
        <NonResizablePoint ref={bottomRightRef} position={{ x: 1, y: 1 }} />
      </div>
    </CanvasOffsetWrapper>
  )
})

interface NonResizablePointProps {
  position: EdgePosition
}

const NonResizablePointSize = 6
const NonResizablePointOffset = NonResizablePointSize / 2
const NonResizablePoint = React.memo(
  React.forwardRef<HTMLDivElement, NonResizablePointProps>((props, ref) => {
    const colorTheme = useColorTheme()
    const scale = useEditorState(
      Substores.canvas,
      (store) => store.editor.canvas.scale,
      'NonResizablePoint scale',
    )

    return (
      <div
        ref={ref}
        style={{
          position: 'absolute',
          pointerEvents: 'none',
        }}
      >
        <div
          style={{
            position: 'relative',
            width: NonResizablePointSize / scale,
            height: NonResizablePointSize / scale,
            top: -NonResizablePointOffset / scale,
            left: -NonResizablePointOffset / scale,
            background: colorTheme.canvasControlsSizeBoxBackground.value,
            border: `${1 / scale}px solid ${colorTheme.canvasControlsSizeBoxShadowColor50.value}`,
            borderRadius: '50%',
          }}
          data-testid={`non-resizable-${props.position.x}-${props.position.y}`}
        />
      </div>
    )
  }),
)
