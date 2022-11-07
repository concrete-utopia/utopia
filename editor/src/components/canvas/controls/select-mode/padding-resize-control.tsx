import React from 'react'
import { CanvasVector, size, Size, windowPoint } from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { Modifier } from '../../../../utils/modifiers'
import { useColorTheme } from '../../../../uuiui'
import { EditorDispatch } from '../../../editor/action-types'
import { EditorStorePatched } from '../../../editor/store/editor-state'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import {
  createInteractionViaMouse,
  paddingResizeHandle,
} from '../../canvas-strategies/interaction-state'
import { CSSCursor, EdgePiece } from '../../canvas-types'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import { PaddingMeasurement, simplePaddingFromMetadata } from '../../padding-utils'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'
import { PaddingValueLabel } from './padding-value-indicator'
import { useMaybeHighlightElement } from './select-mode-hooks'

export const paddingControlTestId = (edge: EdgePiece): string => `padding-control-${edge}`
export const paddingControlHandleTestId = (edge: EdgePiece): string =>
  `padding-control-handle-${edge}`

export const PaddingResizeControlContainerTestId = 'PaddingResizeControlContainerTestId'

type Orientation = 'vertical' | 'horizontal'

interface ResizeContolProps {
  edge: EdgePiece
  hiddenByParent: boolean
  paddingValue: PaddingMeasurement
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

export const PaddingResizeControlHoverTimeout: number = 200

type Timeout = ReturnType<typeof setTimeout>

const PaddingResizeControlWidth = 2
const PaddingResizeControlHeight = 12
const PaddingResizeControlBorder = 0.5
const PaddingResizeDragBorder = 1
const PaddingResizeControlHitAreaWidth = 10
const PaddingIndicatorOffset = 10

type StoreSelector<T> = (s: EditorStorePatched) => T

const scaleSelector: StoreSelector<number> = (store) => store.editor.canvas.scale
const dispatchSelector: StoreSelector<EditorDispatch> = (store) => store.dispatch
const isDraggingSelector = (store: EditorStorePatched, edge: EdgePiece): boolean =>
  store.editor.canvas.interactionSession?.activeControl.type === 'PADDING_RESIZE_HANDLE' &&
  store.editor.canvas.interactionSession?.activeControl.edgePiece === edge

const PaddingResizeControlI = React.memo(
  React.forwardRef<HTMLDivElement, ResizeContolProps>((props, ref) => {
    const { scale, dispatch, isDragging } = useEditorState(
      (store) => ({
        scale: scaleSelector(store),
        dispatch: dispatchSelector(store),
        isDragging: isDraggingSelector(store, props.edge),
      }),
      'PaddingResizeControl scale, dispatch, isDragging',
    )

    const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
    const { maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()
    const [indicatorShown, setIndicatorShown] = React.useState<boolean>(false)

    const colorTheme = useColorTheme()

    const [hidden, setHidden] = React.useState<boolean>(true)
    const [hoverStartDelayed, hoverEndDelayed] = useHoverWithDelay(
      PaddingResizeControlHoverTimeout,
      (h) => setHidden(!h),
    )

    const hoverStart = React.useCallback(
      (e: React.MouseEvent) => {
        setIndicatorShown(true)
        hoverStartDelayed(e)
      },
      [hoverStartDelayed],
    )

    const hoverEnd = React.useCallback((e: React.MouseEvent) => {
      setIndicatorShown(false)
    }, [])

    const onEdgeMouseDown = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>) => {
        setHidden(true)
        startResizeInteraction(event, dispatch, props.edge, canvasOffsetRef.current, scale)
      },
      [dispatch, props.edge, canvasOffsetRef, scale],
    )

    const onMouseUp = React.useCallback(() => setHidden(false), [])

    const onMouseMove = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>) => {
        maybeClearHighlightsOnHoverEnd()
        event.stopPropagation()
      },
      [maybeClearHighlightsOnHoverEnd],
    )

    const { cursor, orientation } = edgePieceDerivedProps(props.edge)

    const shown = !(props.hiddenByParent && hidden)

    const { width, height } = sizeFromOrientation(
      orientation,
      size(PaddingResizeControlWidth / scale, PaddingResizeControlHeight / scale),
    )

    const [hitAreaWidth, borderWidth, dragBorderWidth, paddingIndicatorOffset] = [
      PaddingResizeControlHitAreaWidth,
      PaddingResizeControlBorder,
      PaddingResizeDragBorder,
      PaddingIndicatorOffset,
    ].map((v) => v / scale)

    const stripeColor = colorTheme.brandNeonPink.o(50).value
    const color = colorTheme.brandNeonPink.value

    return (
      <div
        onMouseLeave={hoverEndDelayed}
        ref={ref}
        data-testid={paddingControlTestId(props.edge)}
        style={{
          position: 'absolute',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          backgroundImage: hidden
            ? undefined
            : `linear-gradient(135deg, ${stripeColor} 12.5%, rgba(255,255,255,0) 12.5%, rgba(255,255,255,0) 50%, ${stripeColor} 50%, ${stripeColor} 62%, rgba(255,255,255,0) 62%, rgba(255,255,255,0) 100%)`,
          backgroundSize: hidden ? undefined : `${20 / scale}px ${20 / scale}px`,
          border: isDragging ? `${dragBorderWidth}px solid ${color}` : undefined,
        }}
      >
        <div
          data-testid={paddingControlHandleTestId(props.edge)}
          onMouseDown={onEdgeMouseDown}
          onMouseMove={onMouseMove}
          onMouseEnter={hoverStart}
          onMouseLeave={hoverEnd}
          onMouseUp={onMouseUp}
          style={{
            visibility: shown ? 'visible' : 'hidden',
            position: 'absolute',
            padding: hitAreaWidth,
            cursor: cursor,
          }}
        >
          {!isDragging && indicatorShown && (
            <div
              style={{
                position: 'absolute',
                paddingTop: paddingIndicatorOffset,
                paddingLeft: paddingIndicatorOffset,
              }}
            >
              <PaddingValueLabel value={props.paddingValue.value} scale={scale} color={color} />
            </div>
          )}
          <div
            style={{
              boxSizing: 'border-box',
              width: width,
              height: height,
              backgroundColor: color,
              border: `${borderWidth}px solid rgba(255, 255, 255)`,
            }}
          />
        </div>
      </div>
    )
  }),
)

interface PaddingControlProps {
  targets: Array<ElementPath>
}

export const PaddingResizeControl = controlForStrategyMemoized((props: PaddingControlProps) => {
  const selectedElements = props.targets
  const elementMetadata = useRefEditorState((store) => store.editor.jsxMetadata)
  const allElementProps = useRefEditorState((store) => store.editor.allElementProps)

  const numberToPxValue = (n: number) => n + 'px'

  const controlRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
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

  const currentPadding = simplePaddingFromMetadata(
    elementMetadata.current,
    allElementProps.current,
    selectedElements[0],
  )

  const leftRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    const padding = simplePaddingFromMetadata(
      elementMetadata.current,
      allElementProps.current,
      selectedElements[0],
    )
    ref.current.style.height = numberToPxValue(boundingBox.height)
    ref.current.style.width = numberToPxValue(padding.paddingLeft.renderedValuePx)
  })

  const topRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    const padding = simplePaddingFromMetadata(
      elementMetadata.current,
      allElementProps.current,
      selectedElements[0],
    )
    ref.current.style.width = numberToPxValue(boundingBox.width)
    ref.current.style.height = numberToPxValue(padding.paddingTop.renderedValuePx)
  })

  const rightRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    const padding = simplePaddingFromMetadata(
      elementMetadata.current,
      allElementProps.current,
      selectedElements[0],
    )
    ref.current.style.left = numberToPxValue(
      boundingBox.width - padding.paddingRight.renderedValuePx,
    )
    ref.current.style.height = numberToPxValue(boundingBox.height)
    ref.current.style.width = numberToPxValue(padding.paddingRight.renderedValuePx)
  })

  const bottomRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    const padding = simplePaddingFromMetadata(
      elementMetadata.current,
      allElementProps.current,
      selectedElements[0],
    )
    ref.current.style.top = numberToPxValue(
      boundingBox.height - padding.paddingBottom.renderedValuePx,
    )
    ref.current.style.width = numberToPxValue(boundingBox.width)
    ref.current.style.height = numberToPxValue(padding.paddingBottom.renderedValuePx)
  })

  const [hoverHidden, setHoverHidden] = React.useState<boolean>(true)
  const [hoverStart, hoverEnd] = useHoverWithDelay(PaddingResizeControlHoverTimeout, (h) =>
    setHoverHidden(!h),
  )

  return (
    <CanvasOffsetWrapper>
      <div
        data-testid={PaddingResizeControlContainerTestId}
        onMouseEnter={hoverStart}
        onMouseLeave={hoverEnd}
        ref={controlRef}
        style={{
          position: 'absolute',
        }}
      >
        <PaddingResizeControlI
          ref={rightRef}
          edge={'right'}
          hiddenByParent={hoverHidden}
          paddingValue={currentPadding.paddingRight}
        />
        <PaddingResizeControlI
          ref={bottomRef}
          edge={'bottom'}
          hiddenByParent={hoverHidden}
          paddingValue={currentPadding.paddingBottom}
        />
        <PaddingResizeControlI
          ref={leftRef}
          edge={'left'}
          hiddenByParent={hoverHidden}
          paddingValue={currentPadding.paddingLeft}
        />
        <PaddingResizeControlI
          ref={topRef}
          edge={'top'}
          hiddenByParent={hoverHidden}
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
          paddingResizeHandle(edge),
        ),
      ),
    ])
  }
}

function useHoverWithDelay(
  delay: number,
  update: (hovered: boolean) => void,
): [React.MouseEventHandler, React.MouseEventHandler] {
  const fadeInTimeout = React.useRef<Timeout | null>(null)

  const onHoverEnd = () => {
    if (fadeInTimeout.current) {
      clearTimeout(fadeInTimeout.current)
    }
    fadeInTimeout.current = null
    update(false)
  }

  const onHoverStart = () => {
    fadeInTimeout.current = setTimeout(() => update(true), delay)
  }

  return [onHoverStart, onHoverEnd]
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
