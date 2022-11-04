import React, { useState } from 'react'
import { toString } from '../../../../core/shared/element-path'
import { CanvasVector, size, Size, windowPoint } from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { Modifier } from '../../../../utils/modifiers'
import { useColorTheme } from '../../../../uuiui'
import { EditorDispatch } from '../../../editor/action-types'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { cssNumber } from '../../../inspector/common/css-utils'
import CanvasActions from '../../canvas-actions'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { createInteractionViaMouse, flexGapHandle } from '../../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import {
  cursorFromFlexDirection,
  gapControlBoundsFromMetadata,
  SimpleFlexDirection,
} from '../../gap-utils'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'
import {
  CSSNumberLabel,
  PillHandle,
  StripedBackgroundCSS,
  useHoverWithDelay,
} from './control-common'

interface FlexGapControlProps {
  selectedElement: ElementPath
  flexDirection: SimpleFlexDirection
  updatedGapValue: number
}

export const FlexGapControlTestId = 'FlexGapControlTestId'
export const FlexGapControlHandleTestId = 'FlexGapControlHandleTestId'

const PaddingIndicatorOffset = 10
const HitAreaPadding = 5

export const FlexGapControl = controlForStrategyMemoized<FlexGapControlProps>((props) => {
  const { selectedElement, flexDirection, updatedGapValue } = props
  const colorTheme = useColorTheme()
  const indicatorColor = colorTheme.brandNeonPink.value

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
    updatedGapValue,
    flexDirection,
  )

  const onMouseDown = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      startInteraction(e, dispatch, canvasOffset.current, scale)
    },
    [canvasOffset, dispatch, scale],
  )

  const { width, height } = handleDimensions(flexDirection, scale)
  const borderWidth = 1 / scale

  const [paddingIndicatorOffset, hitAreaPadding, dragBorderWidth] = [
    PaddingIndicatorOffset,
    HitAreaPadding,
    1,
  ].map((v) => v / scale)

  const shouldShowBackground = !isDragging && backgroundShown
  const shouldShowIndicator = (path: string) => !isDragging && indicatorShown === path

  return (
    <CanvasOffsetWrapper>
      <div data-testid={FlexGapControlTestId} ref={controlRef}>
        {controlBounds.map(({ bounds, path: p }) => {
          const path = toString(p)
          return (
            <div
              key={path}
              onMouseEnter={controlHoverStart}
              onMouseLeave={controlHoverEnd}
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
                onMouseEnter={() => handleHoverStart(path)}
                onMouseLeave={handleHoverEnd}
              >
                {shouldShowIndicator(path) && (
                  <div
                    style={{
                      position: 'absolute',
                      paddingTop: paddingIndicatorOffset,
                      paddingLeft: paddingIndicatorOffset,
                      pointerEvents: 'none',
                    }}
                  >
                    <CSSNumberLabel
                      value={cssNumber(33, null)}
                      scale={scale}
                      color={indicatorColor}
                    />
                  </div>
                )}
                <PillHandle
                  width={width}
                  height={height}
                  color={colorTheme.brandNeonPink.value}
                  borderWidth={borderWidth}
                />
              </div>
            </div>
          )
        })}
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
