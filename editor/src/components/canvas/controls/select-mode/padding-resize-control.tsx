import React from 'react'
import { CanvasVector, windowPoint } from '../../../../core/shared/math-utils'
import { assertNever } from '../../../../core/shared/utils'
import { Modifier } from '../../../../utils/modifiers'
import { useColorTheme } from '../../../../uuiui'
import { EditorDispatch } from '../../../editor/action-types'
import { EditorStorePatched } from '../../../editor/store/editor-state'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import {
  createInteractionViaMouse,
  paddingResizeHandle,
} from '../../canvas-strategies/interaction-state'
import { CSSCursor, EdgePosition } from '../../canvas-types'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'
import { useMaybeHighlightElement } from './select-mode-hooks'

type Orientation = 'vertical' | 'horizontal'

interface ResizeContolProps {
  orientation: Orientation
  color: string
  cursor: CSSCursor
  position: EdgePosition
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

const PaddingResizeControlWidth = 4
const PaddingResizeControlHeight = 24
const PaddingResizeControlBorder = 1
const PaddingResizeControlI = React.memo(
  React.forwardRef<HTMLDivElement, ResizeContolProps>((props, ref) => {
    const scale = useEditorState((store) => store.editor.canvas.scale, 'PaddingResizeControl scale')
    const dispatch = useEditorState((store) => store.dispatch, 'PaddingResizeControl dispatch')
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

    const width = PaddingResizeControlWidth / scale
    const height = PaddingResizeControlHeight / scale
    const borderWidth = PaddingResizeControlBorder / scale
    return (
      <div
        ref={ref}
        data-testid={`absolute-resizepadding-${props.position.x}-${props.position.y}`}
        style={{
          position: 'absolute',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
        }}
      >
        <div
          onMouseDown={onEdgeMouseDown}
          onMouseMove={onMouseMove}
          style={{
            position: 'absolute',
            width: width,
            height: height,
            backgroundColor: props.color,
            cursor: props.cursor,
            pointerEvents: 'initial',
            border: `${borderWidth}px solid rgb(255, 255, 255, 1)`,
            borderRadius: 2,
            transform: `rotate(${transformFromOrientation(props.orientation)})`,
          }}
        ></div>
      </div>
    )
  }),
)

const selectedElementsSelector = (store: EditorStorePatched) => store.editor.selectedViews
export const PaddingResizeControl = React.memo(() => {
  const colorTheme = useColorTheme()

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

  return (
    <CanvasOffsetWrapper>
      <div
        ref={controlRef}
        style={{
          position: 'absolute',
        }}
      >
        <PaddingResizeControlI
          ref={rightRef}
          position={{ x: 1, y: 0.5 }}
          cursor={CSSCursor.ResizeEW}
          orientation='vertical'
          color={colorTheme.brandNeonPink.value}
        />
        <PaddingResizeControlI
          ref={bottomRef}
          position={{ x: 0.5, y: 1 }}
          cursor={CSSCursor.ResizeNS}
          orientation='horizontal'
          color={colorTheme.brandNeonPink.value}
        />
        <PaddingResizeControlI
          ref={leftRef}
          position={{ x: 0, y: 0.5 }}
          cursor={CSSCursor.ResizeEW}
          orientation='vertical'
          color={colorTheme.brandNeonPink.value}
        />
        <PaddingResizeControlI
          ref={topRef}
          position={{ x: 0.5, y: 0 }}
          cursor={CSSCursor.ResizeNS}
          orientation='horizontal'
          color={colorTheme.brandNeonPink.value}
        />
      </div>
    </CanvasOffsetWrapper>
  )
})

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
          paddingResizeHandle(position),
        ),
      ),
    ])
  }
}
