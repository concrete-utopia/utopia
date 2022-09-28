import React from 'react'
import { CanvasVector, windowPoint } from '../../../../core/shared/math-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { Modifier } from '../../../../utils/modifiers'
import { useColorTheme } from '../../../../uuiui'
import { EditorDispatch } from '../../../editor/action-types'
import { EditorStorePatched } from '../../../editor/store/editor-state'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import {
  CanvasControlType,
  createInteractionViaMouse,
  paddingResizeHandle,
} from '../../canvas-strategies/interaction-state'
import { CSSCursor, EdgePiece } from '../../canvas-types'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import { SimpleCSSPadding, simplePaddingFromMetadata } from '../../padding-utils'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'
import { useMaybeHighlightElement } from './select-mode-hooks'

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
    const outlineColor = colorTheme.canvasDragOutlineBlock.value

    const [hidden, setHidden] = React.useState<boolean>(true)
    const [hoverStart, hoverEnd] = useHoverWithDelay(0, (h) => setHidden(!h))

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
    return (
      <div
        onMouseLeave={hoverEnd}
        onMouseEnter={hoverStart}
        ref={ref}
        data-testid={`absolute-resizepadding-${props.edge}`}
        style={{
          position: 'absolute',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          backgroundImage: hidden
            ? undefined
            : `linear-gradient(135deg, ${outlineColor} 2.5%, rgba(255,255,255,0) 2.5%, rgba(255,255,255,0) 50%, ${outlineColor} 50%, ${outlineColor} 52%, rgba(255,255,255,0) 52%, rgba(255,255,255,0) 100%)`,
          backgroundSize: hidden ? undefined : `${20 / scale}px ${20 / scale}px`,
        }}
      >
        {shown && (
          <div
            onMouseDown={onEdgeMouseDown}
            onMouseMove={onMouseMove}
            onMouseUp={onMouseUp}
            style={{
              width: width,
              height: height,
              backgroundColor: colorTheme.brandNeonPink.value,
              cursor: cursor,
              pointerEvents: 'initial',
              border: `${borderWidth}px solid rgb(255, 255, 255, 1)`,
              borderRadius: 2,
              transform: `rotate(${transformFromOrientation(orientation)})`,
            }}
          ></div>
        )}
      </div>
    )
  }),
)

const selectedElementsSelector = (store: EditorStorePatched) => store.editor.selectedViews
export const PaddingResizeControl = React.memo(() => {
  const selectedElements = useEditorState(
    selectedElementsSelector,
    'PaddingResizeControl selectedElements',
  )

  const padding = useElementPadding(selectedElements[0])

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
    ref.current.style.width = padding.paddingLeft + 'px'
  })
  const topRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    ref.current.style.width = boundingBox.width + 'px'
    ref.current.style.height = padding.paddingTop + 'px'
  })
  const rightRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    ref.current.style.left = boundingBox.width - padding.paddingRight + 'px'
    ref.current.style.height = boundingBox.height + 'px'
    ref.current.style.width = padding.paddingRight + 'px'
  })
  const bottomRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    ref.current.style.top = boundingBox.height - padding.paddingBottom + 'px'
    ref.current.style.width = boundingBox.width + 'px'
    ref.current.style.height = padding.paddingBottom + 'px'
  })

  const [hoverHidden, setHoverHidden] = React.useState<boolean>(true)
  const [hoverStart, hoverEnd] = useHoverWithDelay(200, (h) => setHoverHidden(!h))

  return (
    <CanvasOffsetWrapper>
      <div
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

function useElementPadding(elementPath: ElementPath): SimpleCSSPadding {
  const elementMetadata = useEditorState(
    (store) => store.editor.jsxMetadata,
    'metadata for padding',
  )

  return simplePaddingFromMetadata(elementMetadata, elementPath)
}

function useHoverWithDelay(
  delay: number,
  update: (hovered: boolean) => void,
): [React.MouseEventHandler, React.MouseEventHandler] {
  const fadeInTimeout = React.useRef<Timeout | null>(null)

  const onMouseLeave = () => {
    if (fadeInTimeout.current) {
      clearTimeout(fadeInTimeout.current)
    }
    fadeInTimeout.current = null
    update(false)
  }

  const onMouseEnter = () => {
    fadeInTimeout.current = setTimeout(() => update(true), delay)
  }

  return [onMouseEnter, onMouseLeave]
}

function edgePieceDerivedProps(edgePiece: EdgePiece): {
  cursor: CSSCursor
  orientation: Orientation
} {
  switch (edgePiece) {
    case 'right':
    case 'left':
      return { cursor: CSSCursor.ResizeEW, orientation: 'vertical' }
    case 'bottom':
    case 'top':
      return { cursor: CSSCursor.ResizeNS, orientation: 'horizontal' }
    default:
      assertNever(edgePiece)
  }
}
