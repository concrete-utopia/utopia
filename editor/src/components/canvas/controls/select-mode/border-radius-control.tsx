import React from 'react'
import { CanvasVector, Size, windowPoint } from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { Modifier } from '../../../../utils/modifiers'
import { when } from '../../../../utils/react-conditionals'
import { useColorTheme } from '../../../../uuiui'
import { EditorDispatch } from '../../../editor/action-types'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { CSSNumber } from '../../../inspector/common/css-utils'
import { BorderRadiusHandleSize, handlePosition } from '../../border-radius-utis'
import CanvasActions from '../../canvas-actions'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import {
  borderRadiusResizeHandle,
  createInteractionViaMouse,
} from '../../canvas-strategies/interaction-state'
import {
  EdgePosition,
  EdgePositionBottomLeft,
  EdgePositionBottomRight,
  EdgePositionTopLeft,
  EdgePositionTopRight,
} from '../../canvas-types'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'
import { CSSNumberLabel, CSSNumberWithRenderedValue, useHoverWithDelay } from './controls-common'

export const CircularHandleTestId = (position: EdgePosition): string =>
  `circular-handle-${position.x}-${position.y}`

export interface BorderRadiusControlProps {
  selectedElement: ElementPath
  elementSize: Size
  borderRadius: CSSNumberWithRenderedValue
  showIndicatorOnEdge: EdgePosition | null
  indicatorValue: CSSNumber
}

export const BorderRadiusControl = controlForStrategyMemoized<BorderRadiusControlProps>((props) => {
  const { selectedElement, borderRadius, elementSize, showIndicatorOnEdge, indicatorValue } = props

  const canvasOffset = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
  const { dispatch, scale, isDragging } = useEditorState(
    (store) => ({
      dispatch: store.dispatch,
      scale: store.editor.canvas.scale,

      isDragging:
        store.editor.canvas.interactionSession?.activeControl.type ===
        'BORDER_RADIUS_RESIZE_HANDLE',
    }),
    'BorderRadiusControl dispatch scale',
  )

  const colorTheme = useColorTheme()

  const [backgroundShown, setBackgroundShown] = React.useState<boolean>(false)

  const [controlHoverStart, controlHoverEnd] = useHoverWithDelay(200, setBackgroundShown)

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

  return (
    <CanvasOffsetWrapper>
      <div
        onMouseEnter={controlHoverStart}
        onMouseLeave={controlHoverEnd}
        ref={controlRef}
        style={{ position: 'absolute' }}
      >
        {[
          EdgePositionTopLeft,
          EdgePositionTopRight,
          EdgePositionBottomLeft,
          EdgePositionBottomRight,
        ].map((edgePosition) => (
          <CircularHandle
            key={CircularHandleTestId(edgePosition)}
            borderRadius={borderRadius}
            isDragging={isDragging}
            backgroundShown={backgroundShown}
            scale={scale}
            color={colorTheme.brandNeonPink.value}
            canvasOffsetRef={canvasOffset}
            dispatch={dispatch}
            edgePosition={edgePosition}
            elementSize={elementSize}
            showIndicatorFromParent={
              showIndicatorOnEdge?.x === edgePosition.x && showIndicatorOnEdge?.y === edgePosition.y
            }
            indicatorValue={indicatorValue}
          />
        ))}
      </div>
    </CanvasOffsetWrapper>
  )
})

interface CircularHandleProp {
  borderRadius: CSSNumberWithRenderedValue
  canvasOffsetRef: { current: CanvasVector }
  dispatch: EditorDispatch
  edgePosition: EdgePosition
  indicatorValue: CSSNumber
  isDragging: boolean
  backgroundShown: boolean
  scale: number
  color: string
  elementSize: Size
  showIndicatorFromParent: boolean
}

const CircularHandle = React.memo((props: CircularHandleProp) => {
  const {
    borderRadius,
    isDragging,
    backgroundShown,
    scale,
    indicatorValue,
    color,
    canvasOffsetRef,
    dispatch,
    edgePosition,
    elementSize,
    showIndicatorFromParent,
  } = props

  const [hovered, setHovered] = React.useState<boolean>(false)
  const handleHoverStart = React.useCallback(() => setHovered(true), [])
  const handleHoverEnd = React.useCallback(() => setHovered(false), [])

  const shouldShowIndicator = (!isDragging && hovered) || showIndicatorFromParent
  const shouldShowHandle = isDragging || backgroundShown

  const size = BorderRadiusHandleSize(scale)
  const position = handlePosition(borderRadius.renderedValuePx, elementSize, edgePosition, scale)

  return (
    <div
      data-testid={CircularHandleTestId(edgePosition)}
      style={{
        position: 'absolute',
        left: position.x,
        top: position.y,
      }}
      onMouseEnter={handleHoverStart}
      onMouseLeave={handleHoverEnd}
      onMouseDown={(e) =>
        startInteraction(e, dispatch, canvasOffsetRef.current, scale, edgePosition)
      }
    >
      <>
        {when(
          shouldShowIndicator,
          <div
            style={{
              position: 'absolute',
              paddingLeft: 20 / scale,
              paddingTop: 20 / scale,
              pointerEvents: 'none',
            }}
          >
            <CSSNumberLabel prefix='Radius' value={indicatorValue} scale={scale} color={color} />
          </div>,
        )}
        {when(
          shouldShowHandle,
          <div
            style={{
              width: size,
              height: size,
              background: 'white',
              border: `${1 / scale}px solid blue`,
              borderRadius: '50%',
            }}
          />,
        )}
      </>
    </div>
  )
})

function startInteraction(
  event: React.MouseEvent<HTMLDivElement>,
  dispatch: EditorDispatch,
  canvasOffset: CanvasVector,
  scale: number,
  edgePosition: EdgePosition,
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
          borderRadiusResizeHandle(edgePosition),
        ),
      ),
    ])
  }
}
