import React, { useState } from 'react'
import * as EP from '../../../../core/shared/element-path'
import {
  CanvasRectangle,
  CanvasVector,
  size,
  Size,
  windowPoint,
} from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { Modifier } from '../../../../utils/modifiers'
import { when } from '../../../../utils/react-conditionals'
import { useColorTheme } from '../../../../uuiui'
import { EditorDispatch } from '../../../editor/action-types'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { CSSNumber, FlexDirection, printCSSNumber } from '../../../inspector/common/css-utils'
import CanvasActions from '../../canvas-actions'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { createInteractionViaMouse, flexGapHandle } from '../../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import { cursorFromFlexDirection, gapControlBoundsFromMetadata } from '../../gap-utils'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'
import {
  CanvasLabel,
  CSSNumberWithRenderedValue,
  PillHandle,
  StripedBackgroundCSS,
  StripeOpacity,
  useHoverWithDelay,
} from './controls-common'

interface FlexGapControlProps {
  selectedElement: ElementPath
  flexDirection: FlexDirection
  updatedGapValue: CSSNumberWithRenderedValue
}

export const FlexGapControlTestId = 'FlexGapControlTestId'
export const FlexGapControlHandleTestId = 'FlexGapControlHandleTestId'

export const FlexGapControl = controlForStrategyMemoized<FlexGapControlProps>((props) => {
  const { selectedElement, flexDirection, updatedGapValue } = props
  const colorTheme = useColorTheme()
  const indicatorColor = colorTheme.brandNeonPink.o(StripeOpacity).value

  const [indicatorShown, setIndicatorShown] = useState<string | null>(null)
  const [backgroundShown, setBackgroundShown] = useState<boolean>(false)

  const [controlHoverStart, controlHoverEnd] = useHoverWithDelay(0, setBackgroundShown)

  const handleHoverStart = React.useCallback((id: string) => setIndicatorShown(id), [])
  const handleHoverEnd = React.useCallback(() => setIndicatorShown(null), [])

  const { dispatch, scale, metadata, isDragging } = useEditorState(
    (store) => ({
      dispatch: store.dispatch,
      scale: store.editor.canvas.scale,
      metadata: store.editor.canvas.interactionSession?.latestMetadata ?? store.editor.jsxMetadata,
      isDragging: store.editor.canvas.interactionSession?.activeControl.type === 'FLEX_GAP_HANDLE',
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

  const controlBounds = gapControlBoundsFromMetadata(
    metadata,
    selectedElement,
    updatedGapValue.renderedValuePx,
    flexDirection,
  )

  const onMouseDown = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      startInteraction(e, dispatch, canvasOffset.current, scale)
    },
    [canvasOffset, dispatch, scale],
  )

  return (
    <CanvasOffsetWrapper>
      <div data-testid={FlexGapControlTestId} ref={controlRef}>
        {controlBounds.map(({ bounds, path: p }) => {
          const path = EP.toString(p)
          return (
            <GapControlSegment
              key={path}
              hoverStart={controlHoverStart}
              hoverEnd={controlHoverEnd}
              handleHoverStart={handleHoverStart}
              handleHoverEnd={handleHoverEnd}
              onMouseDown={onMouseDown}
              indicatorShown={indicatorShown}
              path={path}
              bounds={bounds}
              flexDirection={flexDirection}
              indicatorColor={indicatorColor}
              scale={scale}
              backgroundShown={backgroundShown}
              isDragging={isDragging}
              gapValue={updatedGapValue.value}
            />
          )
        })}
      </div>
    </CanvasOffsetWrapper>
  )
})

interface GapControlSizeConstants {
  dragBorderWidth: number
  paddingIndicatorOffset: number
  hitAreaPadding: number
  borderWidth: number
}

const DefaultGapControlSizeConstants: GapControlSizeConstants = {
  dragBorderWidth: 1,
  borderWidth: 1,
  paddingIndicatorOffset: 10,
  hitAreaPadding: 5,
}

const gapControlSizeConstants = (
  constants: GapControlSizeConstants,
  scale: number,
): GapControlSizeConstants => ({
  dragBorderWidth: constants.dragBorderWidth / scale,
  borderWidth: constants.borderWidth / scale,
  paddingIndicatorOffset: constants.paddingIndicatorOffset / scale,
  hitAreaPadding: constants.hitAreaPadding / scale,
})

interface GapControlSegmentProps {
  hoverStart: React.MouseEventHandler
  hoverEnd: React.MouseEventHandler
  handleHoverStart: (_: string) => void
  handleHoverEnd: () => void
  onMouseDown: React.MouseEventHandler
  bounds: CanvasRectangle
  flexDirection: FlexDirection
  gapValue: CSSNumber
  indicatorShown: string | null
  path: string
  indicatorColor: string
  scale: number
  isDragging: boolean
  backgroundShown: boolean
}

const GapControlSegment = React.memo<GapControlSegmentProps>((props) => {
  const {
    hoverStart,
    hoverEnd,
    handleHoverEnd,
    handleHoverStart,
    onMouseDown,
    indicatorShown,
    bounds,
    isDragging,
    gapValue,
    flexDirection,
    indicatorColor,
    scale,
    path,
    backgroundShown,
  } = props

  const colorTheme = useColorTheme()

  const { dragBorderWidth, hitAreaPadding, paddingIndicatorOffset, borderWidth } =
    gapControlSizeConstants(DefaultGapControlSizeConstants, scale)
  const { width, height } = handleDimensions(flexDirection, scale)

  const handleHoverStartInner = React.useCallback(
    () => handleHoverStart(path),
    [handleHoverStart, path],
  )

  const shouldShowIndicator = React.useCallback(
    (p: string) => !isDragging && indicatorShown === p,
    [indicatorShown, isDragging],
  )

  const shouldShowBackground = !isDragging && backgroundShown

  return (
    <div
      key={path}
      onMouseEnter={hoverStart}
      onMouseLeave={hoverEnd}
      style={{
        position: 'absolute',
        left: bounds.x,
        top: bounds.y,
        width: bounds.width,
        height: bounds.height,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        border: isDragging ? `${dragBorderWidth}px solid ${indicatorColor}` : undefined,
        ...(shouldShowBackground ? StripedBackgroundCSS(indicatorColor, scale) : {}),
      }}
    >
      <div
        data-testid={FlexGapControlHandleTestId}
        style={{ padding: hitAreaPadding, cursor: cursorFromFlexDirection(flexDirection) }}
        onMouseDown={onMouseDown}
        onMouseEnter={handleHoverStartInner}
        onMouseLeave={handleHoverEnd}
      >
        {when(
          shouldShowIndicator(path),
          <div
            style={{
              position: 'absolute',
              paddingTop: paddingIndicatorOffset,
              paddingLeft: paddingIndicatorOffset,
              pointerEvents: 'none',
            }}
          >
            <CanvasLabel
              value={printCSSNumber(gapValue, null)}
              scale={scale}
              color={colorTheme.brandNeonPink.value}
            />
          </div>,
        )}
        {when(
          backgroundShown,
          <PillHandle
            width={width}
            height={height}
            pillColor={colorTheme.brandNeonPink.value}
            borderWidth={borderWidth}
          />,
        )}
      </div>
    </div>
  )
})

function handleDimensions(flexDirection: FlexDirection, scale: number): Size {
  if (flexDirection === 'row' || flexDirection === 'row-reverse') {
    return size(4 / scale, 12 / scale)
  }
  if (flexDirection === 'column' || flexDirection === 'column-reverse') {
    return size(12 / scale, 4 / scale)
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
