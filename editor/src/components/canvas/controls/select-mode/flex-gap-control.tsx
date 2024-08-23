import React, { useState } from 'react'
import * as EP from '../../../../core/shared/element-path'
import type { CanvasRectangle, CanvasVector, Size } from '../../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  isFiniteRectangle,
  size,
  windowPoint,
  zeroSize,
} from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { Modifier } from '../../../../utils/modifiers'
import { when } from '../../../../utils/react-conditionals'
import { useColorTheme, UtopiaStyles } from '../../../../uuiui'
import type { EditorDispatch } from '../../../editor/action-types'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import type { CSSNumber, FlexDirection } from '../../../inspector/common/css-utils'
import { printCSSNumber } from '../../../inspector/common/css-utils'
import CanvasActions from '../../canvas-actions'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { createInteractionViaMouse, flexGapHandle } from '../../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import {
  cursorFromFlexDirection,
  maybeFlexGapData,
  gapControlBoundsFromMetadata,
  recurseIntoChildrenOfMapOrFragment,
} from '../../gap-utils'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import type { CSSNumberWithRenderedValue } from './controls-common'
import { CanvasLabel, fallbackEmptyValue, PillHandle, useHoverWithDelay } from './controls-common'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import {
  getFlexJustifyContent,
  type FlexAlignment,
  type FlexJustifyContent,
  getFlexAlignment,
} from '../../../inspector/inspector-common'
import {
  flipFlexDirection,
  isReversedFlexDirection,
  reverseFlexAlignment,
  reverseJustifyContent,
} from '../../../../core/model/flex-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'

interface FlexGapControlProps {
  selectedElement: ElementPath
  updatedGapValue: CSSNumberWithRenderedValue | null
}

export const FlexGapControlTestId = 'flex-gap-control'
export const FlexGapControlHandleTestId = 'flex-gap-control-handle'

export const FlexGapControl = controlForStrategyMemoized<FlexGapControlProps>((props) => {
  const { selectedElement, updatedGapValue } = props
  const colorTheme = useColorTheme()
  const accentColor = colorTheme.gapControlsBg.value

  const hoveredViews = useEditorState(
    Substores.highlightedHoveredViews,
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

  const dispatch = useDispatch()
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'FlexGapControl scale',
  )
  const metadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'FlexGapControl metadata',
  )

  const elementPathTrees = useEditorState(
    Substores.metadata,
    (store) => store.editor.elementPathTree,
    'FlexGapControl metadata',
  )

  const allElementProps = useEditorState(
    Substores.metadata,
    (store) => store.editor.allElementProps,
    'FlexGapControl metadata',
  )

  const isDragging = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.interactionSession?.activeControl.type === 'FLEX_GAP_HANDLE',
    'FlexGapControl isDragging',
  )

  const canvasOffset = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)

  const onMouseDown = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      startInteraction(e, dispatch, canvasOffset.current, scale)
    },
    [canvasOffset, dispatch, scale],
  )

  const children = recurseIntoChildrenOfMapOrFragment(
    metadata,
    allElementProps,
    elementPathTrees,
    selectedElement,
  )
  const flexGap = maybeFlexGapData(metadata, selectedElement)
  if (flexGap == null) {
    return null
  }

  const flexGapValue = updatedGapValue ?? flexGap.value

  const controlBounds = gapControlBoundsFromMetadata(
    metadata,
    selectedElement,
    children.map((c) => c.elementPath),
    flexGapValue.renderedValuePx,
    flexGap.direction,
  )

  const contentArea = React.useMemo((): Size => {
    function valueForDimension(
      directions: FlexDirection[],
      direction: FlexDirection,
      directionSize: number,
      gapSize: number,
    ) {
      return directions.includes(direction) ? directionSize : gapSize
    }

    const bounds = boundingRectangleArray(
      mapDropNulls((c) => {
        const localFrame = MetadataUtils.getLocalFrame(c.elementPath, metadata)
        return localFrame != null && isFiniteRectangle(localFrame) ? localFrame : null
      }, children),
    )

    if (bounds == null) {
      return zeroSize
    } else {
      return {
        width: valueForDimension(
          ['column', 'column-reverse'],
          flexGap.direction,
          bounds.width,
          flexGapValue.renderedValuePx,
        ),
        height: valueForDimension(
          ['row', 'row-reverse'],
          flexGap.direction,
          bounds.height,
          flexGapValue.renderedValuePx,
        ),
      }
    }
  }, [children, flexGap.direction, flexGapValue.renderedValuePx, metadata])

  const justifyContent = React.useMemo(() => {
    return (
      MetadataUtils.findElementByElementPath(metadata, selectedElement)?.specialSizeMeasurements
        .justifyContent ?? null
    )
  }, [metadata, selectedElement])

  const alignItems = React.useMemo(() => {
    return (
      MetadataUtils.findElementByElementPath(metadata, selectedElement)?.specialSizeMeasurements
        .alignItems ?? null
    )
  }, [metadata, selectedElement])

  return (
    <CanvasOffsetWrapper>
      <div data-testid={FlexGapControlTestId} style={{ pointerEvents: 'none' }}>
        {controlBounds.map(({ bounds, path: p }) => {
          const path = EP.toString(p)
          const valueToShow = fallbackEmptyValue(flexGapValue)
          return (
            <GapControlSegment
              key={path}
              hoverStart={controlHoverStart}
              hoverEnd={controlHoverEnd}
              onMouseDown={onMouseDown}
              elementHovered={elementHovered}
              path={path}
              bounds={bounds}
              contentArea={contentArea}
              flexDirection={flexGap.direction}
              accentColor={accentColor}
              scale={scale}
              backgroundShown={backgroundShown}
              isDragging={isDragging}
              gapValue={valueToShow}
              justifyContent={justifyContent}
              alignItems={alignItems}
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
  contentArea: Size
  flexDirection: FlexDirection
  gapValue: CSSNumber
  elementHovered: boolean
  path: string
  accentColor: string
  scale: number
  isDragging: boolean
  backgroundShown: boolean
  justifyContent: FlexJustifyContent | null
  alignItems: FlexAlignment | null
}

const GapControlSegment = React.memo<GapControlSegmentProps>((props) => {
  const {
    hoverStart,
    hoverEnd,
    onMouseDown,
    bounds,
    contentArea,
    isDragging,
    gapValue,
    flexDirection,
    accentColor: accentColor,
    elementHovered,
    scale,
    path,
    backgroundShown,
    justifyContent,
    alignItems,
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

  // Invert the flex direction for the handle.
  const segmentFlexDirection = flipFlexDirection(flexDirection)

  // Effectively flip the properties for the main and cross axis, but only permitting
  // values which we know can be applied to that axis.
  const segmentJustifyContent =
    optionalMap((justify) => {
      // Ensures that the handles appear correctly aligned when the direction is reversed.
      return isReversedFlexDirection(flexDirection) ? reverseJustifyContent(justify) : justify
    }, getFlexJustifyContent(alignItems)) ?? undefined
  const segmentAlignItems =
    optionalMap((alignment) => {
      // Ensures that the handles appear correctly aligned when the direction is reversed.
      return isReversedFlexDirection(flexDirection) ? reverseFlexAlignment(alignment) : alignment
    }, getFlexAlignment(justifyContent)) ?? undefined

  return (
    <div
      key={path}
      onMouseEnter={hoverStart}
      onMouseLeave={handleHoverEndInner}
      data-testid={`gap-control-segment-${path}`}
      style={{
        pointerEvents: 'all',
        position: 'absolute',
        left: bounds.x,
        top: bounds.y,
        width: bounds.width,
        height: bounds.height,
        display: 'flex',
        flexDirection: segmentFlexDirection,
        justifyContent: segmentJustifyContent,
        alignItems: segmentAlignItems,
        border: isDragging ? `${dragBorderWidth}px solid ${accentColor}` : undefined,
        ...(shouldShowBackground
          ? UtopiaStyles.backgrounds.stripedBackground(accentColor, scale)
          : {}),
      }}
    >
      <div
        style={{
          width: contentArea.width,
          height: contentArea.height,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
        }}
      >
        <div
          data-testid={`${FlexGapControlHandleTestId}-${path}`}
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
    </div>
  )
})

function handleDimensions(flexDirection: FlexDirection, scale: number): Size {
  if (flexDirection === 'row' || flexDirection === 'row-reverse') {
    return size(3 / scale, 12 / scale)
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
          'zero-drag-not-permitted',
        ),
      ),
    ])
  }
}
