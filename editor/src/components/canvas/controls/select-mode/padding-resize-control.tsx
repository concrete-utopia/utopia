import React from 'react'
import { size, Size } from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { isFeatureEnabled } from '../../../../utils/feature-switches'
import { Modifier } from '../../../../utils/modifiers'
import { useColorTheme, UtopiaStyles } from '../../../../uuiui'
import { EditorDispatch } from '../../../editor/action-types'
import { EditorStorePatched } from '../../../editor/store/editor-state'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { printCSSNumber } from '../../../inspector/common/css-utils'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { disabledHandle, paddingResizeHandle } from '../../canvas-strategies/interaction-state'
import { CSSCursor, EdgePiece } from '../../canvas-types'
import {
  combinePaddings,
  CSSPaddingMappedValues,
  paddingFromSpecialSizeMeasurements,
  PaddingIndictorOffset,
  simplePaddingFromMetadata,
} from '../../padding-utils'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'
import {
  CanvasLabel,
  CSSNumberWithRenderedValue,
  DisabledColor,
  PillHandle,
  startResizeInteraction,
  unitlessCSSNumberWithRenderedValue,
  useHoverWithDelay,
} from './controls-common'

export const PaddingControlTestId = (edge: EdgePiece, disabled: boolean): string =>
  `padding-control${disabled ? '-disabled' : ''}-${edge}`
export const PaddingControlHandleTestId = (edge: EdgePiece, disabled: boolean): string =>
  `padding-control-handle${disabled ? '-disabled' : ''}-${edge}`

export const PaddingResizeControlContainerTestId = 'PaddingResizeControlContainerTestId'

type Orientation = 'vertical' | 'horizontal'

interface ResizeContolProps {
  edge: EdgePiece
  disabled: boolean
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

const PaddingResizeControlWidth = 4
const PaddingResizeControlHeight = 12
const PaddingResizeControlBorder = 1
const PaddingResizeDragBorder = 1
const PaddingResizeControlHitAreaWidth = 3

type StoreSelector<T> = (s: EditorStorePatched) => T

const scaleSelector: StoreSelector<number> = (store) => store.editor.canvas.scale
const dispatchSelector: StoreSelector<EditorDispatch> = (store) => store.dispatch
const isDraggingSelector = (store: EditorStorePatched, edge: EdgePiece): boolean =>
  store.editor.canvas.interactionSession?.activeControl.type === 'PADDING_RESIZE_HANDLE' &&
  store.editor.canvas.interactionSession?.activeControl.edgePiece === edge

const PaddingResizeControlI = React.memo(
  React.forwardRef<HTMLDivElement, ResizeContolProps>((props, ref) => {
    const { setShownByParent } = props
    const { scale, dispatch, isDragging } = useEditorState(
      (store) => ({
        scale: scaleSelector(store),
        dispatch: dispatchSelector(store),
        isDragging: isDraggingSelector(store, props.edge),
      }),
      'PaddingResizeControl scale, dispatch, isDragging',
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
        const handle = props.disabled ? disabledHandle() : paddingResizeHandle(props.edge)
        startResizeInteraction(event, dispatch, handle, canvasOffsetRef.current, scale)
      },
      [setShownByParent, props.disabled, props.edge, dispatch, canvasOffsetRef, scale],
    )

    const { cursor, orientation } = edgePieceDerivedProps(props.edge)

    const shown = !isDragging && props.shownByParent
    const backgroundShown = props.shownByParent && !isDragging && stripesShown

    const { width, height } = sizeFromOrientation(
      orientation,
      size(PaddingResizeControlWidth / scale, PaddingResizeControlHeight / scale),
    )

    const [hitAreaWidth, borderWidth, dragBorderWidth] = [
      PaddingResizeControlHitAreaWidth,
      PaddingResizeControlBorder,
      PaddingResizeDragBorder,
    ].map((v) => v / scale)

    const [indicatorColor, stripeColor, borderColor] = props.disabled
      ? [DisabledColor(colorTheme), DisabledColor(colorTheme), DisabledColor(colorTheme)]
      : [
          colorTheme.brandNeonPink.value,
          colorTheme.brandNeonPink.value,
          colorTheme.brandNeonPink.value,
        ]

    return (
      <div
        onMouseLeave={backgroundHoverEnd}
        onMouseEnter={hoverStartDelayed}
        ref={ref}
        data-testid={PaddingControlTestId(props.edge, props.disabled)}
        style={{
          pointerEvents: 'all',
          position: 'absolute',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          border: isDragging ? `${dragBorderWidth}px solid ${borderColor}` : undefined,
          ...(backgroundShown
            ? UtopiaStyles.backgrounds.stripedBackground(stripeColor, scale)
            : {}),
        }}
      >
        <div
          data-testid={PaddingControlHandleTestId(props.edge, props.disabled)}
          onMouseDown={onEdgeMouseDown}
          onMouseEnter={hoverStart}
          onMouseLeave={hoverEnd}
          style={{
            pointerEvents: 'all',
            opacity: shown ? 1 : 0,
            position: 'absolute',
            padding: hitAreaWidth,
            cursor: cursor,
            zIndex: 1,
          }}
        >
          {!isDragging && indicatorShown && (
            <div
              style={{
                position: 'absolute',
                paddingTop: PaddingIndictorOffset(scale),
                paddingLeft: PaddingIndictorOffset(scale),
              }}
            >
              <CanvasLabel
                value={printCSSNumber(props.paddingValue.value, null)}
                scale={scale}
                color={indicatorColor}
                textColor={colorTheme.white.value}
              />
            </div>
          )}
          <PillHandle
            width={width}
            height={height}
            pillColor={indicatorColor}
            borderWidth={borderWidth}
          />
        </div>
      </div>
    )
  }),
)

interface PaddingControlProps {
  targets: Array<ElementPath>
  disabled: boolean
  currentPadding: CSSPaddingMappedValues<CSSNumberWithRenderedValue | undefined>
}

export const PaddingResizeControl = controlForStrategyMemoized((props: PaddingControlProps) => {
  const { targets, disabled, currentPadding } = props
  const selectedElements = targets
  const elementMetadata = useRefEditorState((store) => store.editor.jsxMetadata)

  const hoveredViews = useEditorState(
    (store) => store.editor.hoveredViews,
    'PaddingResizeControl hoveredViews',
  )

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

  const leftRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    const padding = combinePaddings(
      paddingFromSpecialSizeMeasurements(elementMetadata.current, selectedElements[0]),
      simplePaddingFromMetadata(elementMetadata.current, selectedElements[0]),
    )

    ref.current.style.height = numberToPxValue(boundingBox.height)
    ref.current.style.width = numberToPxValue(padding.paddingLeft?.renderedValuePx ?? 0)
  })

  const topRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    const padding = combinePaddings(
      paddingFromSpecialSizeMeasurements(elementMetadata.current, selectedElements[0]),
      simplePaddingFromMetadata(elementMetadata.current, selectedElements[0]),
    )
    ref.current.style.width = numberToPxValue(boundingBox.width)
    ref.current.style.height = numberToPxValue(padding.paddingTop?.renderedValuePx ?? 0)
  })

  const rightRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    const padding = combinePaddings(
      paddingFromSpecialSizeMeasurements(elementMetadata.current, selectedElements[0]),
      simplePaddingFromMetadata(elementMetadata.current, selectedElements[0]),
    )
    ref.current.style.left = numberToPxValue(
      boundingBox.width - (padding.paddingRight?.renderedValuePx ?? 0),
    )
    ref.current.style.height = numberToPxValue(boundingBox.height)
    ref.current.style.width = numberToPxValue(padding.paddingRight?.renderedValuePx ?? 0)
  })

  const bottomRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    const padding = combinePaddings(
      paddingFromSpecialSizeMeasurements(elementMetadata.current, selectedElements[0]),
      simplePaddingFromMetadata(elementMetadata.current, selectedElements[0]),
    )
    ref.current.style.top = numberToPxValue(
      boundingBox.height - (padding.paddingBottom?.renderedValuePx ?? 0),
    )
    ref.current.style.width = numberToPxValue(boundingBox.width)
    ref.current.style.height = numberToPxValue(padding.paddingBottom?.renderedValuePx ?? 0)
  })

  const shownByParent = selectedElementHovered || anyControlHovered

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
          ref={rightRef}
          edge={'right'}
          disabled={disabled}
          shownByParent={shownByParent}
          setShownByParent={setAnyControlHovered}
          paddingValue={currentPadding.paddingRight ?? unitlessCSSNumberWithRenderedValue(0)}
        />
        <PaddingResizeControlI
          ref={bottomRef}
          edge={'bottom'}
          disabled={disabled}
          shownByParent={shownByParent}
          setShownByParent={setAnyControlHovered}
          paddingValue={currentPadding.paddingBottom ?? unitlessCSSNumberWithRenderedValue(0)}
        />
        <PaddingResizeControlI
          ref={leftRef}
          edge={'left'}
          disabled={disabled}
          shownByParent={shownByParent}
          setShownByParent={setAnyControlHovered}
          paddingValue={currentPadding.paddingLeft ?? unitlessCSSNumberWithRenderedValue(0)}
        />
        <PaddingResizeControlI
          ref={topRef}
          edge={'top'}
          shownByParent={shownByParent}
          setShownByParent={setAnyControlHovered}
          paddingValue={currentPadding.paddingTop ?? unitlessCSSNumberWithRenderedValue(0)}
          disabled={disabled}
        />
      </div>
    </CanvasOffsetWrapper>
  )
})

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
