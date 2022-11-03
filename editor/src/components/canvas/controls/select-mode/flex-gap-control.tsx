import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { stripNulls } from '../../../../core/shared/array-utils'
import { toString } from '../../../../core/shared/element-path'
import {
  canvasRectangle,
  CanvasRectangle,
  CanvasVector,
  size,
  Size,
  windowPoint,
} from '../../../../core/shared/math-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { Modifier } from '../../../../utils/modifiers'
import { EditorDispatch } from '../../../editor/action-types'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { createInteractionViaMouse, flexGapHandle } from '../../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import { cursorFromFlexDirection, PathWithBounds, SimpleFlexDirection } from '../../gap-utils'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'

interface FlexGapControlProps {
  selectedElement: ElementPath
  flexDirection: SimpleFlexDirection
  controlBounds: Array<PathWithBounds>
}

export const FlexGapControlTestId = 'FlexGapControlTestId'
export const FlexGapControlHandleTestId = 'FlexGapControlHandleTestId'

export const FlexGapControl = controlForStrategyMemoized<FlexGapControlProps>((props) => {
  const { selectedElement, flexDirection, controlBounds } = props

  const { dispatch, scale } = useEditorState(
    (store) => ({
      dispatch: store.dispatch,
      scale: store.editor.canvas.scale,
    }),
    'FlexGapControl dispatch scale',
  )

  const canvasOffset = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)

  const controlRef = useBoundingBox([selectedElement], (ref, boundingBox) => {
    if (isZeroSizedElement(boundingBox)) {
      ref.current.style.display = 'none'
    } else {
      ref.current.style.display = 'block'
      ref.current.style.left = boundingBox.x + 'px'
      ref.current.style.top = boundingBox.y + 'px'
      ref.current.style.width = boundingBox.width + 'px'
      ref.current.style.height = boundingBox.height + 'px'
    }
  })

  const onMouseDown = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      startInteraction(e, dispatch, canvasOffset.current, scale)
    },
    [canvasOffset, dispatch, scale],
  )

  const { width, height } = handleDimensions(flexDirection, scale)

  return (
    <CanvasOffsetWrapper>
      <div data-testid={FlexGapControlTestId} ref={controlRef}>
        {controlBounds.map(({ bounds, path }) => (
          <div
            key={toString(path)} // so as not to make balint mad
            style={{
              position: 'absolute',
              left: bounds.x,
              top: bounds.y,
              width: bounds.width,
              height: bounds.height,
              backgroundColor: `rgba(0, 0, 255, 0.1)`,
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
            }}
          >
            <div
              data-testid={FlexGapControlHandleTestId}
              style={{ padding: 5, cursor: cursorFromFlexDirection(flexDirection) }}
              onMouseDown={onMouseDown}
            >
              <div
                style={{
                  width: width,
                  height: height,
                  backgroundColor: 'red',
                }}
              />
            </div>
          </div>
        ))}
      </div>
    </CanvasOffsetWrapper>
  )
})

function handleDimensions(flexDirection: SimpleFlexDirection, scale: number): Size {
  if (flexDirection === 'row' || flexDirection === 'row-reverse') {
    return size(2 / scale, 12 / scale)
  }
  if (flexDirection === 'column' || flexDirection === 'column-reverse') {
    return size(12 / scale, 2 / scale)
  }
  assertNever(flexDirection)
}

function startInteraction(
  event: React.MouseEvent<HTMLDivElement>,
  dispatch: EditorDispatch,
  canvasOffset: CanvasVector,
  scale: number,
) {
  event.stopPropagation()
  if (event.buttons === 1 && event.button !== 2) {
    const canvasPositions = windowToCanvasCoordinates(
      scale,
      canvasOffset,
      windowPoint({ x: event.nativeEvent.x, y: event.nativeEvent.y }),
    )
    dispatch([
      CanvasActions.createInteractionSession(
        createInteractionViaMouse(
          canvasPositions.canvasPositionRaw,
          Modifier.modifiersForEvent(event),
          flexGapHandle(),
        ),
      ),
    ])
  }
}
