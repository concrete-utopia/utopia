import React from 'react'
import type { CanvasRectangle } from '../../../../core/shared/math-utils'
import { boundingRectangleArray, offsetRect } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { useColorTheme } from '../../../../uuiui'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
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

interface DragTargetsElementPathsLive {
  type: 'element-paths-live'
  targets: Array<ElementPath>
}

export function dragTargetsElementPathsLive(
  targets: Array<ElementPath>,
): DragTargetsElementPathsLive {
  return {
    type: 'element-paths-live',
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

type DragOutlineControlProps =
  | DragTargetsElementPaths
  | DragTargetsElementPathsLive
  | DragTargetsFrames

export const DragOutlineControl = controlForStrategyMemoized((props: DragOutlineControlProps) => {
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'OutlineControl scale',
  )
  const frame = useFrameFromProps(props)
  const isAbsoluteStrategy = useEditorState(
    Substores.restOfStore,
    (store) =>
      store.strategyState.sortedApplicableStrategies != null &&
      store.strategyState.sortedApplicableStrategies?.length > 0
        ? store.strategyState.sortedApplicableStrategies[0].strategy.id
            .toLowerCase()
            .includes('abs')
        : false,
    'DragOutlineControl isAbsoluteStrategy',
  )

  const colorTheme = useColorTheme()

  if (frame == null || isAbsoluteStrategy) {
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
  const bounds = useEditorState(
    Substores.fullStore,
    (store) => {
      switch (props.type) {
        case 'frames':
          return boundingRectangleArray(props.frames)
        case 'element-paths':
          return getMultiselectBounds(store.strategyState.startingMetadata, props.targets)
        case 'element-paths-live':
          return getMultiselectBounds(
            store.editor.canvas.interactionSession?.latestMetadata ??
              store.strategyState.startingMetadata,
            props.targets,
          )
        default:
          assertNever(props)
      }
    },
    'GhostOutline frame',
  )

  const dragVector = useEditorState(
    Substores.canvas,
    (store) => {
      if (store.editor.canvas.interactionSession?.interactionData.type !== 'DRAG') {
        return null
      }
      return store.editor.canvas.interactionSession.interactionData.drag
    },
    'GhostOutline dragVector',
  )

  if (bounds == null || dragVector == null) {
    return null
  }

  if (props.type === 'element-paths-live') {
    return bounds
  } else {
    return offsetRect(bounds, dragVector)
  }
}
