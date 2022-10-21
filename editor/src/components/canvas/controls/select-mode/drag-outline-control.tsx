import React from 'react'
import {
  boundingRectangleArray,
  CanvasRectangle,
  offsetRect,
} from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { useColorTheme } from '../../../../uuiui'
import { useEditorState } from '../../../editor/store/store-hook'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { getMultiselectBounds } from '../../canvas-strategies/strategies/shared-move-strategies-helpers'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

export const DragOutlineControlTestId = 'drag-outline-control'

interface DragTargetsElementPaths {
  type: 'element-paths'
  targets: Array<ElementPath>
}

export function dragTargetsElementPaths(targets: Array<ElementPath>): DragTargetsElementPaths {
  return {
    type: 'element-paths',
    targets: targets,
  }
}

interface DragTargetsFrames {
  type: 'frames'
  frames: Array<CanvasRectangle>
}

export function dragTargetsFrame(frames: Array<CanvasRectangle>): DragTargetsFrames {
  return {
    type: 'frames',
    frames: frames,
  }
}

type DragOutlineControlProps = DragTargetsElementPaths | DragTargetsFrames

export const DragOutlineControl = controlForStrategyMemoized((props: DragOutlineControlProps) => {
  const scale = useEditorState((store) => store.editor.canvas.scale, 'OutlineControl scale')
  const frame = useFrameFromProps(props)

  const colorTheme = useColorTheme()

  if (frame == null) {
    return null
  }

  return (
    <CanvasOffsetWrapper>
      <div
        data-testid={DragOutlineControlTestId}
        style={{
          position: 'absolute',
          top: frame.y,
          left: frame.x,
          width: frame.width,
          height: frame.height,
          boxSizing: 'border-box',
          boxShadow: `0px 0px 0px ${1 / scale}px  ${colorTheme.canvasSelectionFocusable.value}`,
          opacity: '50%',
        }}
      />
    </CanvasOffsetWrapper>
  )
})

function useFrameFromProps(props: DragOutlineControlProps): CanvasRectangle | null {
  const bounds = useEditorState((store) => {
    switch (props.type) {
      case 'frames':
        return boundingRectangleArray(props.frames)
      case 'element-paths':
        return getMultiselectBounds(store.strategyState.startingMetadata, props.targets)
      default:
        assertNever(props)
    }
  }, 'GhostOutline frame')

  const dragVector = useEditorState((store) => {
    if (store.editor.canvas.interactionSession?.interactionData.type !== 'DRAG') {
      return null
    }
    return store.editor.canvas.interactionSession.interactionData.drag
  }, 'GhostOutline dragVector')

  if (bounds == null || dragVector == null) {
    return null
  }

  return offsetRect(bounds, dragVector)
}
