import React from 'react'
import * as EP from '../../../../core/shared/element-path'
import { CanvasVector, Size, windowPoint } from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { Modifier } from '../../../../utils/modifiers'
import { when } from '../../../../utils/react-conditionals'
import { useColorTheme } from '../../../../uuiui'
import { EditorDispatch } from '../../../editor/action-types'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { printCSSNumber } from '../../../inspector/common/css-utils'
import {
  BorderRadiusAdjustMode,
  BorderRadiusControlMinimumForDisplay,
  BorderRadiusCorner,
  BorderRadiusCorners,
  BorderRadiusHandleBorderWidth,
  BorderRadiusHandleDotSize,
  BorderRadiusHandleHitArea,
  BorderRadiusHandleSize,
  BorderRadiusSides,
  handlePosition,
} from '../../border-radius-control-utils'
import CanvasActions from '../../canvas-actions'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import {
  borderRadiusResizeHandle,
  createInteractionViaMouse,
} from '../../canvas-strategies/interaction-state'
import { CSSCursor } from '../../canvas-types'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'
import { CanvasLabel, CSSNumberWithRenderedValue } from './controls-common'

export const CircularHandleTestId = (corner: BorderRadiusCorner): string =>
  `circular-handle-${corner}`

export interface BorderRadiusControlProps {
  selectedElement: ElementPath
  elementSize: Size
  borderRadius: BorderRadiusSides<CSSNumberWithRenderedValue>
  showIndicatorOnCorner: BorderRadiusCorner | null
  mode: BorderRadiusAdjustMode
}

export const BorderRadiusControl = controlForStrategyMemoized<BorderRadiusControlProps>((props) => {
  const {
    selectedElement,
    borderRadius,
    elementSize,
    showIndicatorOnCorner: showIndicatorOnEdge,
    mode,
  } = props

  const canvasOffset = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
  const dispatch = useDispatch()
  const { scale, isDragging } = useEditorState(
    Substores.canvas,
    (store) => ({
      scale: store.editor.canvas.scale,
      isDragging:
        store.editor.canvas.interactionSession?.activeControl.type ===
        'BORDER_RADIUS_RESIZE_HANDLE',
    }),
    'BorderRadiusControl isDragging scale',
  )

  const hoveredViews = useEditorState(
    Substores.highlightedHoveredViews,
    (store) => store.editor.hoveredViews,
    'BorderRadiusControl hoveredViews',
  )

  const backgroundShown = hoveredViews.some((p) => EP.pathsEqual(p, selectedElement))

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

  const borderRadius = useEditorState(
    Substores.metadata,
    (store) => borderRadiusSelector(store, selectedElement),
    'BorderRadiusControl borderRadius',
  )

  if (borderRadius == null) {
    return null
  }

  return (
    <CanvasOffsetWrapper>
      <div ref={controlRef} style={{ position: 'absolute', pointerEvents: 'none' }}>
        {BorderRadiusCorners.map((corner) => (
          <CircularHandle
            key={CircularHandleTestId(corner)}
            borderRadius={borderRadius.borderRadius[corner]}
            isDragging={isDragging}
            backgroundShown={backgroundShown}
            scale={scale}
            canvasOffsetRef={canvasOffset}
            dispatch={dispatch}
            corner={corner}
            elementSize={elementSize}
            showIndicatorFromParent={showIndicatorOnEdge === corner}
            showDot={mode === 'individual'}
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
  corner: BorderRadiusCorner
  isDragging: boolean
  backgroundShown: boolean
  scale: number
  elementSize: Size
  showIndicatorFromParent: boolean
  showDot: boolean
}

const CircularHandle = React.memo((props: CircularHandleProp) => {
  const {
    borderRadius,
    isDragging,
    backgroundShown,
    scale,
    canvasOffsetRef,
    dispatch,
    corner,
    elementSize,
    showIndicatorFromParent,
    showDot,
  } = props

  const [hovered, setHovered] = React.useState<boolean>(false)

  const colorTheme = useColorTheme()

  const handleHoverStart = React.useCallback(() => setHovered(true), [])
  const handleHoverEnd = React.useCallback(() => setHovered(false), [])

  const handleMouseDown = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) =>
      startInteraction(e, dispatch, canvasOffsetRef.current, scale, corner),
    [canvasOffsetRef, corner, dispatch, scale],
  )

  const shouldShowIndicator = (!isDragging && hovered) || showIndicatorFromParent
  const shouldShowHandle = isDragging || backgroundShown

  const { padding, size } = BorderRadiusHandleSize(scale)
  const position = handlePosition(
    isDragging
      ? borderRadius.renderedValuePx
      : Math.max(borderRadius.renderedValuePx, BorderRadiusControlMinimumForDisplay(scale)),
    elementSize,
    corner,
    scale,
  )

  return (
    <div
      data-testid={CircularHandleTestId(corner)}
      style={{
        position: 'absolute',
        left: position.x,
        top: position.y,
        padding: padding,
        pointerEvents: 'all',
        cursor: CSSCursor.Radius,
      }}
      onMouseEnter={handleHoverStart}
      onMouseLeave={handleHoverEnd}
      onMouseDown={handleMouseDown}
    >
      <>
        {when(
          shouldShowIndicator,
          <div
            style={{
              position: 'absolute',
              paddingLeft: BorderRadiusHandleHitArea(scale),
              paddingTop: BorderRadiusHandleHitArea(scale),
              pointerEvents: 'none',
            }}
          >
            <CanvasLabel
              value={`${printCSSNumber(borderRadius.value, null)}`}
              scale={scale}
              color={colorTheme.brandNeonPink.value}
              textColor={colorTheme.white.value}
            />
          </div>,
        )}
        <div
          style={{
            visibility: shouldShowHandle ? 'visible' : 'hidden',
            width: 6,
            height: 6,
            backgroundColor: 'white',
            transition: 'transform .1s ease-in-out',
            zoom: 1 / scale,
            transformOrigin: 'bottom left',
            boxShadow: '0 1px 2px 0 rgba(52,52,52,0.35), 0px 0px 0px 0.5px rgba(166,166,166,0.82)',
            borderRadius: '50%',
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
          }}
        >
          <div
            style={{
              visibility: shouldShowHandle && showDot ? 'visible' : 'hidden',
              width: 2,
              height: 2,
              backgroundColor: 'rgba(166,166,166,0.82)',
              borderRadius: '50%',
            }}
          />
        </div>
      </>
    </div>
  )
})

function startInteraction(
  event: React.MouseEvent<HTMLDivElement>,
  dispatch: EditorDispatch,
  canvasOffset: CanvasVector,
  scale: number,
  corner: BorderRadiusCorner,
) {
  if (event.buttons === 1 && event.button !== 2) {
    event.stopPropagation()
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
          borderRadiusResizeHandle(corner),
          'zero-drag-not-permitted',
        ),
      ),
    ])
  }
}
