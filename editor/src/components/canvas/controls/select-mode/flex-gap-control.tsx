import React, { useState } from 'react'
import * as EP from '../../../../core/shared/element-path'
import { emptyComments, jsxAttributeValue } from '../../../../core/shared/element-template'
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
import { useColorTheme, UtopiaStyles } from '../../../../uuiui'
import { EditorDispatch } from '../../../editor/action-types'
import { setProperty } from '../../../editor/actions/action-creators'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { CSSNumber, FlexDirection, printCSSNumber } from '../../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
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

  const [numberInputShown, setNumberInputShown] = React.useState<boolean>(false)

  const showNumberInput = React.useCallback(() => setNumberInputShown(true), [])
  React.useEffect(() => {
    if (isDragging) {
      setNumberInputShown(false)
    }
  }, [hoveredViews, isDragging, selectedElement])

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

  const { width, height } = size(50, 20)
  const numberInputRef = useBoundingBox([selectedElement], (ref, boundingBox) => {
    if (isZeroSizedElement(boundingBox)) {
      ref.current.style.display = 'none'
    } else {
      ref.current.style.display = 'block'
      ref.current.style.left = boundingBox.x + (boundingBox.width - width) / 2 + 'px'
      ref.current.style.top = boundingBox.y + 'px'
      ref.current.style.width = width + 'px'
      ref.current.style.height = height + 'px'
    }
  })

  const controlRef = useBoundingBox([selectedElement], (ref, boundingBox) => {
    if (isZeroSizedElement(boundingBox)) {
      ref.current.style.display = 'none'
    } else {
      ref.current.style.display = 'block'
      ref.current.style.left = boundingBox.x + 'px'
      ref.current.style.top = boundingBox.y + 'px'
      ref.current.style.width = width + 'px'
      ref.current.style.height = height + 'px'
    }
  })

  const onNumberInputUpdate = React.useCallback(
    (n: number) => {
      dispatch([
        setProperty(
          selectedElement,
          stylePropPathMappingFn('gap', ['style']),
          jsxAttributeValue(
            printCSSNumber({ value: n, unit: updatedGapValue.value.unit }, null),
            emptyComments,
          ),
        ),
      ])
    },
    [dispatch, selectedElement, updatedGapValue.value.unit],
  )

  return (
    <CanvasOffsetWrapper>
      <div ref={controlRef} data-testid={FlexGapControlTestId} style={{ pointerEvents: 'none' }}>
        {when(
          numberInputShown,
          <div
            ref={numberInputRef}
            style={{ position: 'absolute', zIndex: 1, pointerEvents: 'all' }}
          >
            <NumberInput
              id={`${FlexGapControlTestId}NumberInput`}
              initialValue={updatedGapValue.renderedValuePx}
              onChange={onNumberInputUpdate}
            />
          </div>,
        )}
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
              showNumberInput={showNumberInput}
              indicatorShown={indicatorShown}
              path={path}
              bounds={bounds}
              flexDirection={flexDirection}
              indicatorColor={indicatorColor}
              scale={scale}
              backgroundShown={backgroundShown || numberInputShown}
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
  showNumberInput: () => void
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
    showNumberInput,
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
        style={{ padding: hitAreaPadding, cursor: cursorFromFlexDirection(flexDirection) }}
        onMouseDown={onMouseDown}
        onMouseUp={showNumberInput}
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

interface NumberInputProps {
  id: string
  initialValue: number
  onChange: (_: number) => void
}

const NumberInput = React.memo<NumberInputProps>((props) => {
  const { onChange } = props
  const [currentValue, setCurrentValue] = React.useState<number>(props.initialValue)
  const onValueChange = React.useCallback((event: React.ChangeEvent<HTMLInputElement>) => {
    event.preventDefault()
    event.stopPropagation()

    const {
      target: { value },
    } = event
    const valueNumeric = parseFloat(value)
    setCurrentValue(valueNumeric)
  }, [])

  const onKeyDown = React.useCallback(
    (e: React.KeyboardEvent<HTMLInputElement>) => {
      if (e.code === 'Enter') {
        onChange(currentValue)
      }
    },
    [currentValue, onChange],
  )

  return (
    <>
      <input
        onKeyDown={onKeyDown}
        type='number'
        id={props.id}
        name={props.id}
        value={currentValue}
        onChange={onValueChange}
      />
    </>
  )
})
