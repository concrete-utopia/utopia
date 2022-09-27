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
import { paddingForEdge, SimpleCSSPadding, simplePaddingFromMetadata } from '../../padding-utils'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'
import { useMaybeHighlightElement } from './select-mode-hooks'

type Orientation = 'vertical' | 'horizontal'

interface ResizeContolProps {
  orientation: Orientation
  color: string
  cursor: CSSCursor
  edge: EdgePiece
  paddingForEdge: number
  hidden: boolean
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
    const colorTheme = useColorTheme()
    const outlineColor = colorTheme.canvasDragOutlineBlock.value

    const { maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()

    const onEdgeMouseDown = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>) => {
        startResizeInteraction(event, dispatch, props.edge, canvasOffsetRef.current, scale)
      },
      [dispatch, props.edge, canvasOffsetRef, scale],
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
        data-testid={`absolute-resizepadding-${props.edge}`}
        style={{
          position: 'absolute',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          backgroundImage: `linear-gradient(135deg, ${outlineColor} 2.5%, rgba(255,255,255,0) 2.5%, rgba(255,255,255,0) 50%, ${outlineColor} 50%, ${outlineColor} 52%, rgba(255,255,255,0) 52%, rgba(255,255,255,0) 100%)`,
          backgroundSize: `${20 / scale}px ${20 / scale}px`,
          pointerEvents: 'none',
        }}
      >
        <div
          onMouseDown={onEdgeMouseDown}
          onMouseMove={onMouseMove}
          style={{
            visibility: props.hidden ? 'hidden' : 'visible',
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

  const activeControl = useEditorState(
    (store) => store.editor.canvas.interactionSession?.activeControl,
    'PaddingResizeControl interaction',
  )

  const activeEdgePiece = optionalMap(edgePositionFromActiveControl, activeControl)

  const [innerHidden, setInnerHidden] = React.useState<boolean>(true)

  const fadeInTimeout = React.useRef<Timeout | null>(null)

  const onMouseLeave = () => {
    if (fadeInTimeout.current) {
      clearTimeout(fadeInTimeout.current)
    }
    fadeInTimeout.current = null
    setInnerHidden(true)
  }

  const onMouseEnter = () => {
    fadeInTimeout.current = setTimeout(() => {
      setInnerHidden(false)
    }, 200)
  }

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

  return (
    <CanvasOffsetWrapper>
      <div
        onMouseEnter={onMouseEnter}
        onMouseLeave={onMouseLeave}
        ref={controlRef}
        style={{
          position: 'absolute',
        }}
      >
        <PaddingResizeControlI
          ref={rightRef}
          edge={'right'}
          paddingForEdge={paddingForEdge('right', padding)}
          cursor={CSSCursor.ResizeEW}
          orientation='vertical'
          color={colorTheme.brandNeonPink.value}
          hidden={innerHidden || activeEdgePiece === 'right'}
        />
        <PaddingResizeControlI
          ref={bottomRef}
          edge={'bottom'}
          paddingForEdge={paddingForEdge('bottom', padding)}
          cursor={CSSCursor.ResizeNS}
          orientation='horizontal'
          color={colorTheme.brandNeonPink.value}
          hidden={innerHidden || activeEdgePiece === 'bottom'}
        />
        <PaddingResizeControlI
          ref={leftRef}
          edge={'left'}
          paddingForEdge={paddingForEdge('left', padding)}
          cursor={CSSCursor.ResizeEW}
          orientation='vertical'
          color={colorTheme.brandNeonPink.value}
          hidden={innerHidden || activeEdgePiece === 'left'}
        />
        <PaddingResizeControlI
          ref={topRef}
          edge={'top'}
          paddingForEdge={paddingForEdge('top', padding)}
          cursor={CSSCursor.ResizeNS}
          orientation='horizontal'
          color={colorTheme.brandNeonPink.value}
          hidden={innerHidden || activeEdgePiece === 'top'}
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

function useElementPadding(elementPath: ElementPath): SimpleCSSPadding {
  const elementMetadata = useEditorState(
    (store) => store.editor.jsxMetadata,
    'metadata for padding',
  )

  return simplePaddingFromMetadata(elementMetadata, elementPath)
}

function translationForEdge(edgePiece: EdgePiece, delta: number): string {
  switch (edgePiece) {
    case 'top':
      return `${delta / 2}px, 0px`
    case 'bottom':
      return `${-delta / 2}px, 0px`
    case 'right':
      return `${-delta / 2}px, 0px`
    case 'left':
      return `${delta / 2}px, 0px`
    default:
      assertNever(edgePiece)
  }
}

function edgePositionFromActiveControl(activeControl: CanvasControlType): EdgePiece | null {
  if (activeControl.type !== 'PADDING_RESIZE_HANDLE') {
    return null
  }

  return activeControl.edgePiece
}
