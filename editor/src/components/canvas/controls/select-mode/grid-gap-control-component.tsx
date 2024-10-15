import React, { useState } from 'react'
import type { CanvasRectangle, CanvasVector, Size } from '../../../../core/shared/math-utils'
import { size, windowPoint } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { Modifier } from '../../../../utils/modifiers'
import { when } from '../../../../utils/react-conditionals'
import { useColorTheme, UtopiaStyles } from '../../../../uuiui'
import type { EditorDispatch } from '../../../editor/action-types'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import type { CSSNumber } from '../../../inspector/common/css-utils'
import { cssNumber, printCSSNumber } from '../../../inspector/common/css-utils'
import CanvasActions from '../../canvas-actions'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { createInteractionViaMouse, gridGapHandle } from '../../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import type { Axis } from '../../gap-utils'
import { maybeGridGapData, gridGapControlBoundsFromMetadata } from '../../gap-utils'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import type { CSSNumberWithRenderedValue } from './controls-common'
import { CanvasLabel, fallbackEmptyValue, PillHandle, useHoverWithDelay } from './controls-common'
import { CSSCursor } from '../../../../uuiui-deps'
import { useBoundingBox } from '../bounding-box-hooks'
import { isZeroSizedElement } from '../outline-utils'
import { createArrayWithLength } from '../../../../core/shared/array-utils'
import { useGridData } from '../grid-controls-for-strategies'

export interface GridGapControlProps {
  selectedElement: ElementPath
  updatedGapValueRow: CSSNumberWithRenderedValue | null
  updatedGapValueColumn: CSSNumberWithRenderedValue | null
}

export const GridGapControlTestId = 'grid-gap-control'
export const GridGapControlHandleTestId = 'grid-gap-control-handle'
// background delay when hovering the gap
export const GridGapBackgroundHoverDelay = 1500
// background delay when hovering the handle itself
const GapHandleBackgroundHoverDelay = 750
// px threshold for showing the gap handles even without hovering the gap itself
// (for narrow gaps)
const GapHandleGapWidthThreshold = 10

interface GapControlSizeConstants {
  paddingIndicatorOffset: number
  hitAreaPadding: number
  borderWidth: number
}

const DefaultGapControlSizeConstants: GapControlSizeConstants = {
  borderWidth: 1,
  paddingIndicatorOffset: 10,
  hitAreaPadding: 5,
}

const gapControlSizeConstants = (
  constants: GapControlSizeConstants,
  scale: number,
): GapControlSizeConstants => ({
  borderWidth: constants.borderWidth / scale,
  paddingIndicatorOffset: constants.paddingIndicatorOffset / scale,
  hitAreaPadding: constants.hitAreaPadding / scale,
})

type GridGapHandleProps = {
  gapId: string
  index: number
  scale: number
  gapValue: CSSNumberWithRenderedValue
  axis: Axis
  onMouseDown: React.MouseEventHandler
  isDragging: boolean
  onHandleHoverStartInner: (e: React.MouseEvent, index: number) => void
  indicatorShown: number | null
  elementHovered: boolean
  gapIsHovered: boolean
  backgroundShown: boolean
}
export function GridGapHandle({
  gapId,
  index,
  scale,
  gapValue,
  axis,
  onMouseDown,
  onHandleHoverStartInner,
  isDragging,
  indicatorShown,
  elementHovered,
  gapIsHovered,
  backgroundShown,
}: GridGapHandleProps) {
  const { width, height } = handleDimensions(axis, scale)
  const { hitAreaPadding, paddingIndicatorOffset, borderWidth } = gapControlSizeConstants(
    DefaultGapControlSizeConstants,
    scale,
  )
  const colorTheme = useColorTheme()
  const shouldShowIndicator = !isDragging && indicatorShown === index
  let shouldShowHandle = !isDragging && gapIsHovered
  // show the handle also if the gap is too narrow to hover
  if (!gapIsHovered && !backgroundShown) {
    shouldShowHandle = elementHovered && gapValue.renderedValuePx <= GapHandleGapWidthThreshold
  }
  const handleOpacity = gapIsHovered ? 1 : 0.3

  const onHandleHoverStart = React.useCallback(
    (e: React.MouseEvent) => {
      onHandleHoverStartInner(e, index)
    },
    [onHandleHoverStartInner, index],
  )

  const rowGapStyles =
    axis === 'row'
      ? ({
          left: '50%',
          top: '50%',
          transform: 'translate(-50%, -50%)',
          position: 'absolute',
          gridArea: `1/${index + 1}/2/${index + 2}`,
        } as const)
      : {}
  return (
    <div
      data-testid={`${GridGapControlHandleTestId}-${gapId}`}
      style={{
        visibility: shouldShowHandle ? 'visible' : 'hidden',
        pointerEvents: 'all',
        padding: hitAreaPadding,
        cursor: axis === 'row' ? CSSCursor.GapNS : CSSCursor.GapEW,
        opacity: handleOpacity,
        ...rowGapStyles,
      }}
      onMouseDown={onMouseDown}
      onMouseEnter={onHandleHoverStart}
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
            value={printCSSNumber(
              gapValue.value ?? cssNumber(gapValue.renderedValuePx, 'px'),
              null,
            )}
            scale={scale}
            color={colorTheme.brandNeonOrange.value}
            textColor={colorTheme.white.value}
          />,
        )}
      </div>
      <PillHandle
        width={width}
        height={height}
        pillColor={colorTheme.brandNeonOrange.value}
        borderWidth={borderWidth}
      />
    </div>
  )
}

function handleDimensions(axis: Axis, scale: number): Size {
  if (axis === 'row') {
    return size(12 / scale, 4 / scale)
  }
  if (axis === 'column') {
    return size(4 / scale, 12 / scale)
  }
  assertNever(axis)
}

export function startInteraction(
  event: React.MouseEvent<HTMLDivElement>,
  dispatch: EditorDispatch,
  canvasOffset: CanvasVector,
  scale: number,
  axis: Axis,
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
          gridGapHandle(axis),
          'zero-drag-not-permitted',
        ),
      ),
    ])
  }
}
