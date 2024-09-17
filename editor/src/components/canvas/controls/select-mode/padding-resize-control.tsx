import React from 'react'
import type { CanvasSubstate } from '../../../../components/editor/store/store-hook-substore-types'
import type { CanvasRectangle, CanvasVector, Size } from '../../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  canvasRectangle,
  isInfinityRectangle,
  size,
  windowPoint,
  zeroRectangle,
} from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import type { Modifiers } from '../../../../utils/modifiers'
import { emptyModifiers, Modifier } from '../../../../utils/modifiers'
import { useColorTheme, UtopiaStyles } from '../../../../uuiui'
import type { EditorDispatch } from '../../../editor/action-types'
import { useDispatch } from '../../../editor/store/dispatch-context'
import type { EditorStorePatched } from '../../../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { printCSSNumber } from '../../../inspector/common/css-utils'
import CanvasActions from '../../canvas-actions'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import type { InteractionSession } from '../../canvas-strategies/interaction-state'
import {
  createInteractionViaMouse,
  paddingResizeHandle,
} from '../../canvas-strategies/interaction-state'
import type { EdgePiece } from '../../canvas-types'
import { CSSCursor, isHorizontalEdgePiece } from '../../canvas-types'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import {
  combinePaddings,
  paddingAdjustMode,
  paddingFromSpecialSizeMeasurements,
  PaddingIndictorOffset,
  simplePaddingFromMetadata,
} from '../../padding-utils'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'
import type { CSSNumberWithRenderedValue } from './controls-common'
import { CanvasLabel, fallbackEmptyValue, PillHandle, useHoverWithDelay } from './controls-common'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'

export const paddingControlTestId = (edge: EdgePiece): string => `padding-control-${edge}`
export const paddingControlHandleTestId = (edge: EdgePiece): string =>
  `padding-control-handle-${edge}`

export const PaddingResizeControlContainerTestId = 'PaddingResizeControlContainerTestId'

type Orientation = 'vertical' | 'horizontal'

interface ResizeContolProps {
  edge: EdgePiece
  boundingBox: CanvasRectangle
  shownByParent: boolean
  setShownByParent: (_: boolean) => void
  paddingValue: CSSNumberWithRenderedValue
}

function sizeFromOrientation(orientation: Orientation, desiredSize: Size): Size {
  switch (orientation) {
    case 'horizontal':
      return size(desiredSize.height, desiredSize.width)
    case 'vertical':
      return desiredSize
    default:
      assertNever(orientation)
  }
}

export const PaddingResizeControlHoverTimeout: number = 0

const PaddingResizeControlWidth = 3
const PaddingResizeControlHeight = 12
const PaddingResizeControlBorder = 1
const PaddingResizeDragBorder = 1
const PaddingResizeControlHitAreaPaddingMainSide = 5
const PaddingResizeControlHitAreaPaddingUnaffectedSide = 1

type StoreSelector<T> = (s: CanvasSubstate) => T

const scaleSelector: StoreSelector<number> = (store) => store.editor.canvas.scale

function opposite(padding: EdgePiece): EdgePiece {
  switch (padding) {
    case 'top':
      return 'bottom'
    case 'bottom':
      return 'top'
    case 'left':
      return 'right'
    case 'right':
      return 'left'
    default:
      assertNever(padding)
  }
}

function draggedEdges(modifiers: Modifiers, edge: EdgePiece): Array<EdgePiece> {
  const mode = paddingAdjustMode(modifiers)
  switch (mode) {
    case 'all':
      return ['bottom', 'top', 'left', 'right']
    case 'cross-axis':
      return [edge, opposite(edge)]
    case 'individual':
      return [edge]
    default:
      assertNever(mode)
  }
}

function modifiersFrom(interactionSession: InteractionSession) {
  return interactionSession.interactionData.type === 'DRAG'
    ? interactionSession.interactionData.modifiers
    : emptyModifiers
}

const isDraggingSelector = (store: EditorStorePatched, edge: EdgePiece): boolean =>
  store.editor.canvas.interactionSession?.activeControl.type === 'PADDING_RESIZE_HANDLE' &&
  draggedEdges(
    modifiersFrom(store.editor.canvas.interactionSession),
    store.editor.canvas.interactionSession.activeControl.edgePiece,
  ).includes(edge)

const PaddingResizeControlI = React.memo((props: ResizeContolProps) => {
  const { setShownByParent } = props
  const dispatch = useDispatch()
  const { scale, isDragging } = useEditorState(
    Substores.fullStore,
    (store) => ({
      scale: scaleSelector(store),
      isDragging: isDraggingSelector(store, props.edge),
    }),
    'PaddingResizeControl scale isDragging',
  )

  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
  const [indicatorShown, setIndicatorShown] = React.useState<boolean>(false)
  const [stripesShown, setStripesShown] = React.useState<boolean>(false)

  const colorTheme = useColorTheme()

  const [hoverStartDelayed, hoverEndDelayed] = useHoverWithDelay(
    PaddingResizeControlHoverTimeout,
    (h) => setShownByParent(h),
  )

  const backgroundHoverEnd = React.useCallback(
    (e: React.MouseEvent) => {
      setStripesShown(false)
      hoverEndDelayed(e)
    },
    [hoverEndDelayed],
  )

  const hoverStart = React.useCallback((e: React.MouseEvent) => {
    setStripesShown(true)
    setIndicatorShown(true)
  }, [])

  const hoverEnd = React.useCallback((e: React.MouseEvent) => {
    setIndicatorShown(false)
  }, [])

  const onEdgeMouseDown = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      setShownByParent(true)
      startResizeInteraction(event, dispatch, props.edge, canvasOffsetRef.current, scale)
    },
    [setShownByParent, dispatch, props.edge, canvasOffsetRef, scale],
  )

  const { cursor, orientation } = edgePieceDerivedProps(props.edge)

  const shown = !isDragging && props.shownByParent
  const backgroundShown = props.shownByParent && !isDragging && stripesShown

  const { width, height } = sizeFromOrientation(
    orientation,
    size(PaddingResizeControlWidth / scale, PaddingResizeControlHeight / scale),
  )

  const [hitAreaPaddingMainSide, hitAreaPaddingUnaffectedSide, borderWidth, dragBorderWidth] = [
    PaddingResizeControlHitAreaPaddingMainSide,
    PaddingResizeControlHitAreaPaddingUnaffectedSide,
    PaddingResizeControlBorder,
    PaddingResizeDragBorder,
  ].map((v) => v / scale)

  // We only want the mouse catchment area to be inside the element, so to prevent it from
  // overflowing we apply padding on all sides of the pill control that are inside the element,
  // and then use a margin to top up the difference so that the pill is always in the correct place
  const controlPaddingForEdge = React.useMemo(
    () => Math.min(hitAreaPaddingMainSide, props.paddingValue.renderedValuePx / 2 / scale),
    [props.paddingValue, hitAreaPaddingMainSide, scale],
  )
  const controlMarginForEdge = React.useMemo(
    () => hitAreaPaddingMainSide - controlPaddingForEdge,
    [hitAreaPaddingMainSide, controlPaddingForEdge],
  )
  const horizontalPadding = React.useMemo(
    () =>
      isHorizontalEdgePiece(props.edge) ? hitAreaPaddingMainSide : hitAreaPaddingUnaffectedSide,
    [props.edge, hitAreaPaddingMainSide, hitAreaPaddingUnaffectedSide],
  )
  const verticalPadding = React.useMemo(
    () =>
      isHorizontalEdgePiece(props.edge) ? hitAreaPaddingUnaffectedSide : hitAreaPaddingMainSide,
    [props.edge, hitAreaPaddingMainSide, hitAreaPaddingUnaffectedSide],
  )

  const stripeColor = colorTheme.brandNeonPink.value
  const color = colorTheme.brandNeonPink.value

  const points = React.useMemo((): {
    top: number
    left: number
    width: number | string
    height: number | string
  } => {
    return {
      top:
        props.edge === 'bottom' ? props.boundingBox.height - props.paddingValue.renderedValuePx : 0,
      left:
        props.edge === 'right' ? props.boundingBox.width - props.paddingValue.renderedValuePx : 0,
      width:
        props.edge === 'top' || props.edge === 'bottom'
          ? '100%'
          : props.paddingValue.renderedValuePx,
      height:
        props.edge === 'left' || props.edge === 'right'
          ? '100%'
          : props.paddingValue.renderedValuePx,
    }
  }, [props.edge, props.boundingBox, props.paddingValue])

  return (
    <div
      onMouseLeave={backgroundHoverEnd}
      onMouseEnter={hoverStartDelayed}
      data-testid={paddingControlTestId(props.edge)}
      style={{
        pointerEvents: 'all',
        position: 'absolute',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        left: points.left,
        top: points.top,
        width: points.width,
        height: points.height,
        border: isDragging ? `${dragBorderWidth}px solid ${color}` : undefined,
        ...(backgroundShown ? UtopiaStyles.backgrounds.stripedBackground(stripeColor, scale) : {}),
      }}
    >
      <div
        data-testid={paddingControlHandleTestId(props.edge)}
        onMouseDown={onEdgeMouseDown}
        onMouseEnter={hoverStart}
        onMouseLeave={hoverEnd}
        style={{
          pointerEvents: 'all',
          opacity: shown ? 1 : 0,
          position: 'absolute',
          paddingLeft: props.edge === 'left' ? controlPaddingForEdge : horizontalPadding,
          paddingRight: props.edge === 'right' ? controlPaddingForEdge : horizontalPadding,
          paddingTop: props.edge === 'top' ? controlPaddingForEdge : verticalPadding,
          paddingBottom: props.edge === 'bottom' ? controlPaddingForEdge : verticalPadding,
          marginLeft: props.edge === 'left' ? controlMarginForEdge : 0,
          marginRight: props.edge === 'right' ? controlMarginForEdge : 0,
          marginTop: props.edge === 'top' ? controlMarginForEdge : 0,
          marginBottom: props.edge === 'bottom' ? controlMarginForEdge : 0,
          cursor: cursor,
          zIndex: 1,
        }}
      >
        {!isDragging && indicatorShown && (
          <div
            style={{
              pointerEvents: 'none',
              position: 'absolute',
              paddingTop: PaddingIndictorOffset(scale),
              paddingLeft: PaddingIndictorOffset(scale),
            }}
          >
            <CanvasLabel
              value={printCSSNumber(fallbackEmptyValue(props.paddingValue), 'px')}
              scale={scale}
              color={color}
              textColor={colorTheme.white.value}
            />
          </div>
        )}
        <PillHandle width={width} height={height} pillColor={color} borderWidth={borderWidth} />
      </div>
    </div>
  )
})

PaddingResizeControlI.displayName = 'PaddingResizeControlI'

interface PaddingControlProps {
  targets: Array<ElementPath>
}

export const PaddingResizeControl = controlForStrategyMemoized((props: PaddingControlProps) => {
  const selectedElements = props.targets
  const elementMetadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'Padding controls metadata',
  )

  const hoveredViews = useEditorState(
    Substores.highlightedHoveredViews,
    (store) => store.editor.hoveredViews,
    'PaddingResizeControl hoveredViews',
  )

  const controlRef = useBoundingBox(
    selectedElements,
    (ref, safeGappedBoundingBox, realBoundingBox) => {
      if (isZeroSizedElement(realBoundingBox)) {
        ref.current.style.display = 'none'
      } else {
        ref.current.style.display = 'block'
        ref.current.style.left = safeGappedBoundingBox.x + 'px'
        ref.current.style.top = safeGappedBoundingBox.y + 'px'
        ref.current.style.width = safeGappedBoundingBox.width + 'px'
        ref.current.style.height = safeGappedBoundingBox.height + 'px'
      }
    },
  )

  const timeoutRef = React.useRef<NodeJS.Timeout | null>(null)

  const [anyControlHovered, setAnyControlHovered] = React.useState<boolean>(false)
  const [selectedElementHovered, setSelectedElementHovered] = React.useState<boolean>(false)
  React.useEffect(() => {
    const timeoutHandle = timeoutRef.current
    const shouldBeShown = hoveredViews.includes(selectedElements[0])

    if (timeoutHandle != null) {
      clearTimeout(timeoutHandle)
    }

    if (shouldBeShown) {
      timeoutRef.current = setTimeout(
        () => setSelectedElementHovered(true),
        PaddingResizeControlHoverTimeout,
      )
    } else {
      setSelectedElementHovered(false)
    }
  }, [hoveredViews, selectedElements])

  const currentPadding = React.useMemo(() => {
    return combinePaddings(
      paddingFromSpecialSizeMeasurements(elementMetadata, selectedElements[0]),
      simplePaddingFromMetadata(elementMetadata, selectedElements[0]),
    )
  }, [elementMetadata, selectedElements])

  const shownByParent = selectedElementHovered || anyControlHovered

  const boundingBox = useEditorState(
    Substores.metadata,
    (store) => {
      const selectedFrames = mapDropNulls((view) => {
        const frame = MetadataUtils.getFrameInCanvasCoords(view, store.editor.jsxMetadata)
        return frame == null || isInfinityRectangle(frame) ? null : frame
      }, selectedElements)

      return boundingRectangleArray(selectedFrames) ?? canvasRectangle(zeroRectangle)
    },
    'PaddingResizeControl boundingBox',
  )

  return (
    <CanvasOffsetWrapper>
      <div
        data-testid={PaddingResizeControlContainerTestId}
        ref={controlRef}
        style={{
          position: 'absolute',
          pointerEvents: 'none',
        }}
      >
        <PaddingResizeControlI
          edge={'right'}
          boundingBox={boundingBox}
          shownByParent={shownByParent}
          setShownByParent={setAnyControlHovered}
          paddingValue={currentPadding.paddingRight}
        />
        <PaddingResizeControlI
          edge={'bottom'}
          boundingBox={boundingBox}
          shownByParent={shownByParent}
          setShownByParent={setAnyControlHovered}
          paddingValue={currentPadding.paddingBottom}
        />
        <PaddingResizeControlI
          edge={'left'}
          boundingBox={boundingBox}
          shownByParent={shownByParent}
          setShownByParent={setAnyControlHovered}
          paddingValue={currentPadding.paddingLeft}
        />
        <PaddingResizeControlI
          edge={'top'}
          boundingBox={boundingBox}
          shownByParent={shownByParent}
          setShownByParent={setAnyControlHovered}
          paddingValue={currentPadding.paddingTop}
        />
      </div>
    </CanvasOffsetWrapper>
  )
})

function startResizeInteraction(
  event: React.MouseEvent<HTMLDivElement>,
  dispatch: EditorDispatch,
  edge: EdgePiece,
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
          paddingResizeHandle(edge),
          'zero-drag-not-permitted',
        ),
      ),
    ])
  }
}

function edgePieceDerivedProps(edgePiece: EdgePiece): {
  cursor: CSSCursor
  orientation: Orientation
} {
  switch (edgePiece) {
    case 'right':
    case 'left':
      return { cursor: CSSCursor.ColResize, orientation: 'vertical' }
    case 'bottom':
    case 'top':
      return { cursor: CSSCursor.RowResize, orientation: 'horizontal' }
    default:
      assertNever(edgePiece)
  }
}
