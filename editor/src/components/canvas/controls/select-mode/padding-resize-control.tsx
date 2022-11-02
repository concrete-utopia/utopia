import React from 'react'
import { CanvasVector, windowPoint } from '../../../../core/shared/math-utils'
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
import { simplePaddingFromMetadata } from '../../padding-utils'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'
import { useMaybeHighlightElement } from './select-mode-hooks'

export const paddingControlTestId = (edge: EdgePiece): string => `padding-control-${edge}`
export const paddingControlHandleTestId = (edge: EdgePiece): string =>
  `padding-control-handle-${edge}`

export const PaddingResizeControlContainerTestId = 'PaddingResizeControlContainerTestId'

type Orientation = 'vertical' | 'horizontal'

interface ResizeContolProps {
  edge: EdgePiece
  hiddenByParent: boolean
}

const transformFromOrientation = (orientation: Orientation) => {
  switch (orientation) {
    case 'horizontal':
      return '90deg'
    case 'vertical':
      return '0deg'
    default:
      assertNever(orientation)
  }
}

export const PaddingResizeControlHoverTimeout: number = 200

type Timeout = ReturnType<typeof setTimeout>

const PaddingResizeControlWidth = 4
const PaddingResizeControlHeight = 24
const PaddingResizeControlBorder = 1
const PaddingResizeControlI = React.memo(
  React.forwardRef<HTMLDivElement, ResizeContolProps>((props, ref) => {
    const scale = useEditorState((store) => store.editor.canvas.scale, 'PaddingResizeControl scale')
    const dispatch = useEditorState((store) => store.dispatch, 'PaddingResizeControl dispatch')
    const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
    const { maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()

    const colorTheme = useColorTheme()

    const [hidden, setHidden] = React.useState<boolean>(true)
    const [hoverStart, hoverEnd] = useHoverWithDelay(PaddingResizeControlHoverTimeout, (h) =>
      setHidden(!h),
    )

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

    const width = PaddingResizeControlWidth / scale
    const height = PaddingResizeControlHeight / scale
    const borderWidth = PaddingResizeControlBorder / scale
    const color = colorTheme.brandNeonPink.value
    return (
      <div
        onMouseLeave={hoverEnd}
        ref={ref}
        data-testid={paddingControlTestId(props.edge)}
        style={{
          position: 'absolute',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          backgroundImage: hidden
            ? undefined
            : `linear-gradient(135deg, ${color} 12.5%, rgba(255,255,255,0) 12.5%, rgba(255,255,255,0) 50%, ${color} 50%, ${color} 62%, rgba(255,255,255,0) 62%, rgba(255,255,255,0) 100%)`,
          backgroundSize: hidden ? undefined : `${20 / scale}px ${20 / scale}px`,
        }}
      >
        {
          <div
            data-testid={paddingControlHandleTestId(props.edge)}
            onMouseDown={onEdgeMouseDown}
            onMouseMove={onMouseMove}
            onMouseEnter={hoverStart}
            onMouseUp={onMouseUp}
            style={{
              position: 'absolute',
              width: width,
              height: height,
              backgroundColor: color,
              visibility: shown ? 'visible' : 'hidden',
              cursor: cursor,
              pointerEvents: 'initial',
              border: `${borderWidth}px solid rgb(255, 255, 255, 1)`,
              borderRadius: 2,
              transform: `rotate(${transformFromOrientation(orientation)})`,
            }}
          />
        }
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

  const widthWithtDefaultPx = (w: number) => w + 'px'
  const heightWithtDefaultPx = (h: number) => h + 'px'

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

  const leftRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    const padding = simplePaddingFromMetadata(elementMetadata.current, selectedElements[0])
    ref.current.style.height = heightWithtDefaultPx(boundingBox.height)
    ref.current.style.width = widthWithtDefaultPx(padding.paddingLeft)
  })

  const topRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    const padding = simplePaddingFromMetadata(elementMetadata.current, selectedElements[0])
    ref.current.style.width = boundingBox.width + 'px'
    ref.current.style.height = padding.paddingTop + 'px'
  })

  const rightRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    const padding = simplePaddingFromMetadata(elementMetadata.current, selectedElements[0])
    ref.current.style.left = boundingBox.width - padding.paddingRight + 'px'
    ref.current.style.height = heightWithtDefaultPx(boundingBox.height)
    ref.current.style.width = widthWithtDefaultPx(padding.paddingRight)
  })

  const bottomRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    const padding = simplePaddingFromMetadata(elementMetadata.current, selectedElements[0])
    ref.current.style.top = boundingBox.height - padding.paddingBottom + 'px'
    ref.current.style.width = boundingBox.width + 'px'
    ref.current.style.height = padding.paddingBottom + 'px'
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
        <PaddingResizeControlI ref={rightRef} edge={'right'} hiddenByParent={hoverHidden} />
        <PaddingResizeControlI ref={bottomRef} edge={'bottom'} hiddenByParent={hoverHidden} />
        <PaddingResizeControlI ref={leftRef} edge={'left'} hiddenByParent={hoverHidden} />
        <PaddingResizeControlI ref={topRef} edge={'top'} hiddenByParent={hoverHidden} />
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
