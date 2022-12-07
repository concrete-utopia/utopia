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
import { isFeatureEnabled } from '../../../../utils/feature-switches'
import { Modifier } from '../../../../utils/modifiers'
import { when } from '../../../../utils/react-conditionals'
import { useColorTheme, UtopiaStyles } from '../../../../uuiui'
import { EditorDispatch } from '../../../editor/action-types'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { CSSNumber, FlexDirection, printCSSNumber } from '../../../inspector/common/css-utils'
import CanvasActions from '../../canvas-actions'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { createInteractionViaMouse, flexGapHandle } from '../../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import { cursorFromFlexDirection, gapControlBoundsFromMetadata } from '../../gap-utils'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import {
  CanvasLabel,
  CSSNumberWithRenderedValue,
  PillHandle,
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
  const indicatorColor = colorTheme.brandNeonPink.value

  const hoveredViews = useEditorState(
    (store) => store.editor.hoveredViews,
    'FlexGapControl hoveredViews',
  )

  const [elementHovered, setElementHovered] = useState<boolean>(false)

  const [backgroundShown, setBackgroundShown] = React.useState<boolean>(false)
  const [controlHoverStart, controlHoverEnd] = useHoverWithDelay(0, setBackgroundShown)

  const timeoutRef = React.useRef<NodeJS.Timeout | null>(null)
  React.useEffect(() => {
    const timeoutHandle = timeoutRef.current
    if (timeoutHandle != null) {
      clearTimeout(timeoutHandle)
    }

    if (hoveredViews.includes(selectedElement)) {
      timeoutRef.current = setTimeout(() => setElementHovered(true), 200)
    } else {
      setElementHovered(false)
    }
  }, [hoveredViews, selectedElement])

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
      <div data-testid={FlexGapControlTestId} style={{ pointerEvents: 'none' }}>
        {controlBounds.map(({ bounds, path: p }) => {
          const path = EP.toString(p)
          return (
            <GapControlSegment
              key={path}
              hoverStart={controlHoverStart}
              hoverEnd={controlHoverEnd}
              onMouseDown={onMouseDown}
              elementHovered={elementHovered}
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
  onMouseDown: React.MouseEventHandler
  bounds: CanvasRectangle
  flexDirection: FlexDirection
  gapValue: CSSNumber
  elementHovered: boolean
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
    onMouseDown,
    bounds,
    isDragging,
    gapValue,
    flexDirection,
    indicatorColor,
    elementHovered,
    scale,
    path,
    backgroundShown,
  } = props

  const colorTheme = useColorTheme()
  const [indicatorShown, setIndicatorShown] = React.useState<boolean>(false)

  const { dragBorderWidth, hitAreaPadding, paddingIndicatorOffset, borderWidth } =
    gapControlSizeConstants(DefaultGapControlSizeConstants, scale)
  const { width, height } = handleDimensions(flexDirection, scale)

  const handleHoverStartInner = React.useCallback(() => {
    setIndicatorShown(true)
  }, [])

  const handleHoverEndInner = React.useCallback(
    (e: React.MouseEvent) => {
      hoverEnd(e)
      setIndicatorShown(false)
    },
    [hoverEnd],
  )

  const shouldShowIndicator = !isDragging && indicatorShown
  const shouldShowHandle = !isDragging && elementHovered
  const shouldShowBackground = !isDragging && backgroundShown

  return (
    <div
      key={path}
      onMouseEnter={hoverStart}
      onMouseLeave={handleHoverEndInner}
      style={{
        pointerEvents: 'all',
        position: 'absolute',
        left: bounds.x,
        top: bounds.y,
        width: bounds.width,
        height: bounds.height,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        border: isDragging ? `${dragBorderWidth}px solid ${indicatorColor}` : undefined,
        ...(shouldShowBackground
          ? UtopiaStyles.backgrounds.stripedBackground(indicatorColor, scale)
          : {}),
      }}
    >
      <div
        data-testid={FlexGapControlHandleTestId}
        style={{
          visibility: shouldShowHandle ? 'visible' : 'hidden',
          padding: hitAreaPadding,
          cursor: cursorFromFlexDirection(flexDirection),
        }}
        onMouseDown={onMouseDown}
        onMouseEnter={handleHoverStartInner}
      >
        <div
          style={{
            position: 'absolute',
            paddingTop: paddingIndicatorOffset,
            paddingLeft: paddingIndicatorOffset,
            pointerEvents: 'none',
          }}
        >
          {when(
            shouldShowIndicator,
            <CanvasLabel
              value={printCSSNumber(gapValue, null)}
              scale={scale}
              color={colorTheme.brandNeonPink.value}
              textColor={colorTheme.white.value}
            />,
          )}
        </div>
        <PillHandle
          width={width}
          height={height}
          pillColor={colorTheme.brandNeonPink.value}
          borderWidth={borderWidth}
        />
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
          flexGapHandle(),
        ),
      ),
    ])
  }
}
