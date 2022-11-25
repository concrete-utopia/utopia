import React, { useState } from 'react'
import * as EP from '../../../../core/shared/element-path'
import { CanvasRectangle, size, Size } from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { when } from '../../../../utils/react-conditionals'
import { useColorTheme, UtopiaStyles } from '../../../../uuiui'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { CSSNumber, FlexDirection, printCSSNumber } from '../../../inspector/common/css-utils'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { disabledHandle, flexGapHandle } from '../../canvas-strategies/interaction-state'
import { cursorFromFlexDirection, gapControlBoundsFromMetadata } from '../../gap-utils'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import {
  CanvasLabel,
  CSSNumberWithRenderedValue,
  DisabledColor,
  PillHandle,
  startResizeInteraction,
  useHoverWithDelay,
} from './controls-common'

interface FlexGapControlProps {
  selectedElement: ElementPath
  flexDirection: FlexDirection
  updatedGapValue: CSSNumberWithRenderedValue
  disabled?: boolean
}

export const FlexGapControlTestId = (disabled: boolean): string =>
  'FlexGapControlTestId' + (disabled ? 'Disabled' : '')

export const FlexGapControlHandleTestId = (disabled: boolean): string =>
  'FlexGapControlHandleTestId' + (disabled ? 'Disabled' : '')

export const FlexGapControl = controlForStrategyMemoized<FlexGapControlProps>((props) => {
  const { selectedElement, flexDirection, updatedGapValue, disabled } = props
  const colorTheme = useColorTheme()
  const indicatorColor = colorTheme.brandNeonPink.value

  const hoveredViews = useEditorState(
    (store) => store.editor.hoveredViews,
    'FlexGapControl hoveredViews',
  )

  const [indicatorShown, setIndicatorShown] = useState<string | null>(null)

  const [backgroundShown, setBackgroundShown] = React.useState<boolean>(false)
  const [controlHoverStart, controlHoverEnd] = useHoverWithDelay(0, setBackgroundShown)

  const timeoutRef = React.useRef<NodeJS.Timeout | null>(null)
  React.useEffect(() => {
    const timeoutHandle = timeoutRef.current
    if (timeoutHandle != null) {
      clearTimeout(timeoutHandle)
    }

    if (hoveredViews.includes(selectedElement)) {
      timeoutRef.current = setTimeout(() => setBackgroundShown(true), 200)
    } else {
      setBackgroundShown(false)
    }
  }, [hoveredViews, selectedElement])

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

  const controlBounds = gapControlBoundsFromMetadata(
    metadata,
    selectedElement,
    updatedGapValue.renderedValuePx,
    flexDirection,
  )

  const onMouseDown = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      const handle = props.disabled ? disabledHandle() : flexGapHandle()
      startResizeInteraction(e, dispatch, handle, canvasOffset.current, scale)
    },
    [canvasOffset, dispatch, props.disabled, scale],
  )

  return (
    <CanvasOffsetWrapper>
      <div data-testid={FlexGapControlTestId(disabled === true)} style={{ pointerEvents: 'none' }}>
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
              disabled={disabled === true}
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
  disabled: boolean
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
    disabled,
  } = props

  const colorTheme = useColorTheme()
  const [stripesShown, setStripesShown] = React.useState<boolean>(false)

  const { dragBorderWidth, hitAreaPadding, paddingIndicatorOffset, borderWidth } =
    gapControlSizeConstants(DefaultGapControlSizeConstants, scale)
  const { width, height } = handleDimensions(flexDirection, scale)

  const handleHoverStartInner = React.useCallback(() => {
    handleHoverStart(path)
    setStripesShown(true)
  }, [handleHoverStart, path])

  const handleHoverEndInner = React.useCallback(
    (e: React.MouseEvent) => {
      hoverEnd(e)
      setStripesShown(false)
    },
    [hoverEnd],
  )

  const shouldShowIndicator = React.useCallback(
    (p: string) => !isDragging && indicatorShown === p,
    [indicatorShown, isDragging],
  )

  const shouldShowBackground = !isDragging && backgroundShown && stripesShown

  const stripeColor = disabled ? DisabledColor(colorTheme) : indicatorColor
  const handleColor = disabled ? DisabledColor(colorTheme) : colorTheme.brandNeonPink.value

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
          ? UtopiaStyles.backgrounds.stripedBackground(stripeColor, scale)
          : {}),
      }}
    >
      <div
        data-testid={FlexGapControlHandleTestId(disabled)}
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
              color={handleColor}
              textColor={colorTheme.white.value}
            />
          </div>,
        )}
        {when(
          backgroundShown,
          <PillHandle
            width={width}
            height={height}
            pillColor={handleColor}
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
