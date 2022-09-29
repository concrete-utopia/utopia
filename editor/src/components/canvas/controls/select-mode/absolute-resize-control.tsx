import React from 'react'
import { CanvasVector, windowPoint } from '../../../../core/shared/math-utils'
import { NO_OP } from '../../../../core/shared/utils'
import { Modifier } from '../../../../utils/modifiers'
import { useColorTheme } from '../../../../uuiui'
import { EditorDispatch } from '../../../editor/action-types'
import { EditorStorePatched } from '../../../editor/store/editor-state'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { createInteractionViaMouse } from '../../canvas-strategies/interaction-state'
import { CSSCursor, EdgePosition } from '../../canvas-types'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'
import { useMaybeHighlightElement } from './select-mode-hooks'

const selectedElementsSelector = (store: EditorStorePatched) => store.editor.selectedViews

export const AbsoluteResizeControl = controlForStrategyMemoized((props) => {
  const selectedElements = useEditorState(
    selectedElementsSelector,
    'AbsoluteResizeControl selectedElements',
  )

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
    ref.current.style.height = boundingBox.height + 'px'
  })
  const topRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    ref.current.style.width = boundingBox.width + 'px'
  })
  const rightRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    ref.current.style.left = boundingBox.width + 'px'
    ref.current.style.height = boundingBox.height + 'px'
  })

  const bottomRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    ref.current.style.top = boundingBox.height + 'px'
    ref.current.style.width = boundingBox.width + 'px'
  })

  const topLeftRef = useBoundingBox(selectedElements, NO_OP)
  const topRightRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    ref.current.style.left = boundingBox.width + 'px'
  })
  const bottomLeftRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    ref.current.style.top = boundingBox.height + 'px'
  })
  const bottomRightRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    ref.current.style.left = boundingBox.width + 'px'
    ref.current.style.top = boundingBox.height + 'px'
  })

  return (
    <CanvasOffsetWrapper>
      <div
        ref={controlRef}
        style={{
          position: 'absolute',
        }}
      >
        <ResizeEdge
          ref={rightRef}
          position={{ x: 1, y: 0.5 }}
          cursor={CSSCursor.ResizeEW}
          direction='vertical'
        />
        <ResizeEdge
          ref={bottomRef}
          position={{ x: 0.5, y: 1 }}
          cursor={CSSCursor.ResizeNS}
          direction='horizontal'
        />
        <ResizeEdge
          ref={leftRef}
          position={{ x: 0, y: 0.5 }}
          cursor={CSSCursor.ResizeEW}
          direction='vertical'
        />
        <ResizeEdge
          ref={topRef}
          position={{ x: 0.5, y: 0 }}
          cursor={CSSCursor.ResizeNS}
          direction='horizontal'
        />
        <ResizePoint ref={topLeftRef} position={{ x: 0, y: 0 }} cursor={CSSCursor.ResizeNWSE} />
        <ResizePoint ref={topRightRef} position={{ x: 1, y: 0 }} cursor={CSSCursor.ResizeNESW} />
        <ResizePoint ref={bottomLeftRef} position={{ x: 0, y: 1 }} cursor={CSSCursor.ResizeNESW} />
        <ResizePoint ref={bottomRightRef} position={{ x: 1, y: 1 }} cursor={CSSCursor.ResizeNWSE} />
      </div>
    </CanvasOffsetWrapper>
  )
})

interface ResizePointProps {
  cursor: CSSCursor
  position: EdgePosition
}

const ResizePointMouseAreaSize = 12
const ResizePointMouseAreaOffset = ResizePointMouseAreaSize / 2
const ResizePointSize = 6
const ResizePointOffset = ResizePointSize / 2
const ResizePoint = React.memo(
  React.forwardRef<HTMLDivElement, ResizePointProps>((props, ref) => {
    const colorTheme = useColorTheme()
    const { maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()
    const scale = useEditorState((store) => store.editor.canvas.scale, 'ResizeEdge scale')
    const dispatch = useEditorState((store) => store.dispatch, 'ResizeEdge dispatch')
    const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)

    const onPointMouseDown = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>) => {
        startResizeInteraction(event, dispatch, props.position, canvasOffsetRef.current, scale)
      },
      [dispatch, props.position, canvasOffsetRef, scale],
    )

    const onMouseMove = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>) => {
        maybeClearHighlightsOnHoverEnd()
        event.stopPropagation()
      },
      [maybeClearHighlightsOnHoverEnd],
    )

    return (
      <div
        ref={ref}
        style={{
          position: 'absolute',
          pointerEvents: 'none',
        }}
      >
        <div
          style={{
            position: 'relative',
            width: ResizePointSize / scale,
            height: ResizePointSize / scale,
            top: -ResizePointOffset / scale,
            left: -ResizePointOffset / scale,
            boxSizing: 'border-box',
            borderWidth: 1 / scale,
            backgroundColor: colorTheme.canvasControlsSizeBoxBackground.value,
            borderRadius: '10%',
            borderStyle: 'none',
            borderColor: 'transparent',
            boxShadow: `${colorTheme.canvasControlsSizeBoxShadowColor.o(50).value} 0px 0px
              ${1 / scale}px, ${colorTheme.canvasControlsSizeBoxShadowColor.o(21).value} 0px ${
              1 / scale
            }px ${2 / scale}px ${1 / scale}px`,
          }}
        />
        <div
          style={{
            position: 'relative',
            width: ResizePointMouseAreaSize / scale,
            height: ResizePointMouseAreaSize / scale,
            top: -ResizePointMouseAreaSize / scale,
            left: -ResizePointMouseAreaOffset / scale,
            backgroundColor: 'transparent',
            pointerEvents: 'initial',
            cursor: props.cursor,
          }}
          onMouseDown={onPointMouseDown}
          onMouseMove={onMouseMove}
          data-testid={`absolute-resize-${props.position.x}-${props.position.y}`}
        />
      </div>
    )
  }),
)

interface ResizeEdgeProps {
  cursor: CSSCursor
  direction: 'horizontal' | 'vertical'
  position: EdgePosition
}

const ResizeMouseAreaSize = 10
const ResizeEdge = React.memo(
  React.forwardRef<HTMLDivElement, ResizeEdgeProps>((props, ref) => {
    const scale = useEditorState((store) => store.editor.canvas.scale, 'ResizeEdge scale')
    const dispatch = useEditorState((store) => store.dispatch, 'ResizeEdge dispatch')
    const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
    const { maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()

    const onEdgeMouseDown = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>) => {
        startResizeInteraction(event, dispatch, props.position, canvasOffsetRef.current, scale)
      },
      [dispatch, props.position, canvasOffsetRef, scale],
    )

    const onMouseMove = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>) => {
        maybeClearHighlightsOnHoverEnd()
        event.stopPropagation()
      },
      [maybeClearHighlightsOnHoverEnd],
    )

    const lineSize = ResizeMouseAreaSize / scale
    const width = props.direction === 'horizontal' ? undefined : lineSize
    const height = props.direction === 'vertical' ? undefined : lineSize
    const offsetLeft = props.direction === 'horizontal' ? `0px` : `${-lineSize / 2}px`
    const offsetTop = props.direction === 'vertical' ? `0px` : `${-lineSize / 2}px`
    return (
      <div
        ref={ref}
        style={{
          position: 'absolute',
          width: width,
          height: height,
          backgroundColor: 'transparent',
          cursor: props.cursor,
          pointerEvents: 'initial',
          transform: `translate(${offsetLeft}, ${offsetTop})`,
        }}
        onMouseDown={onEdgeMouseDown}
        onMouseMove={onMouseMove}
        data-testid={`absolute-resize-${props.position.x}-${props.position.y}`}
      />
    )
  }),
)

function startResizeInteraction(
  event: React.MouseEvent<HTMLDivElement>,
  dispatch: EditorDispatch,
  position: EdgePosition,
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
          {
            type: 'RESIZE_HANDLE',
            edgePosition: position,
          },
        ),
      ),
    ])
  }
}
