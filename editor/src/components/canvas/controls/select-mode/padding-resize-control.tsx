import React from 'react'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { isLeft, right, defaultEither, Either } from '../../../../core/shared/either'
import { isJSXElement } from '../../../../core/shared/element-template'
import { CanvasVector, windowPoint } from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { Modifier } from '../../../../utils/modifiers'
import { useColorTheme } from '../../../../uuiui'
import { EditorDispatch } from '../../../editor/action-types'
import { EditorStorePatched } from '../../../editor/store/editor-state'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { CSSNumber, cssNumber, CSSPadding } from '../../../inspector/common/css-utils'
import CanvasActions from '../../canvas-actions'
import {
  createInteractionViaMouse,
  paddingResizeHandle,
} from '../../canvas-strategies/interaction-state'
import { CSSCursor, EdgePiece } from '../../canvas-types'
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
  edge: EdgePiece
  paddingForEdge: number
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
            transform: `rotate(${transformFromOrientation(
              props.orientation,
            )}) translate(${translationForEdge(props.edge, props.paddingForEdge)})`,
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
          edge={'right'}
          paddingForEdge={paddingForEdge('right', padding)}
          cursor={CSSCursor.ResizeEW}
          orientation='vertical'
          color={colorTheme.brandNeonPink.value}
        />
        <PaddingResizeControlI
          ref={bottomRef}
          edge={'bottom'}
          paddingForEdge={paddingForEdge('bottom', padding)}
          cursor={CSSCursor.ResizeNS}
          orientation='horizontal'
          color={colorTheme.brandNeonPink.value}
        />
        <PaddingResizeControlI
          ref={leftRef}
          edge={'left'}
          paddingForEdge={paddingForEdge('left', padding)}
          cursor={CSSCursor.ResizeEW}
          orientation='vertical'
          color={colorTheme.brandNeonPink.value}
        />
        <PaddingResizeControlI
          ref={topRef}
          edge={'top'}
          paddingForEdge={paddingForEdge('top', padding)}
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

type CSSPaddingKey = keyof CSSPadding
type SimpleCSSPadding = { [key in CSSPaddingKey]: number }

function useElementPadding(elementPath: ElementPath): SimpleCSSPadding {
  const elementMetadata = useEditorState(
    (store) => store.editor.jsxMetadata,
    'metadata for padding',
  )
  const element = MetadataUtils.findElementByElementPath(elementMetadata, elementPath)

  const defaultPadding: SimpleCSSPadding = {
    paddingTop: 0,
    paddingBottom: 0,
    paddingLeft: 0,
    paddingRight: 0,
  }

  if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
    return defaultPadding
  }

  const padding = cssPaddingToSimple(
    getLayoutProperty('padding', right(element.element.value.props), ['style']),
    defaultPadding,
  )

  const paddingTop = pxValueFromEither(
    getLayoutProperty('paddingTop', right(element.element.value.props), ['style']),
    cssNumber(0, 'px'),
  )

  const paddingBottom = pxValueFromEither(
    getLayoutProperty('paddingBottom', right(element.element.value.props), ['style']),
    cssNumber(0, 'px'),
  )

  const paddingLeft = pxValueFromEither(
    getLayoutProperty('paddingLeft', right(element.element.value.props), ['style']),
    cssNumber(0, 'px'),
  )

  const paddingRight = pxValueFromEither(
    getLayoutProperty('paddingRight', right(element.element.value.props), ['style']),
    cssNumber(0, 'px'),
  )

  return cssPaddingWithDefaults(
    { paddingTop, paddingBottom, paddingLeft, paddingRight },
    padding,
    defaultPadding,
  )
}

function cssPaddingWithDefaults(
  parts: Partial<SimpleCSSPadding>,
  whole: Partial<SimpleCSSPadding>,
  defaults: SimpleCSSPadding,
): SimpleCSSPadding {
  return {
    paddingTop: parts.paddingTop ?? whole.paddingTop ?? defaults.paddingTop,
    paddingBottom: parts.paddingBottom ?? whole.paddingBottom ?? defaults.paddingBottom,
    paddingLeft: parts.paddingLeft ?? whole.paddingLeft ?? defaults.paddingLeft,
    paddingRight: parts.paddingRight ?? whole.paddingRight ?? defaults.paddingRight,
  }
}

const pxValue = (number: CSSNumber): number | undefined =>
  number.unit === 'px' || number.unit == null ? number.value : undefined

function cssValueWithDefault<T>(value: Either<string, T | undefined>, defaults: T): T {
  if (isLeft(value) || value.value == null) {
    return defaults
  }
  return value.value
}

function pxValueFromEither(
  value: Either<string, CSSNumber | undefined>,
  defaults: CSSNumber,
): number | undefined {
  return pxValue(cssValueWithDefault(value, defaults))
}

function cssPaddingToSimple(
  p: Either<string, CSSPadding | undefined>,
  padding: SimpleCSSPadding,
): Partial<SimpleCSSPadding> {
  if (isLeft(p) || p.value == null) {
    return padding
  }

  return {
    paddingTop: pxValue(p.value.paddingTop),
    paddingBottom: pxValue(p.value.paddingBottom),
    paddingLeft: pxValue(p.value.paddingLeft),
    paddingRight: pxValue(p.value.paddingRight),
  }
}

function paddingForEdge(edgePiece: EdgePiece, padding: SimpleCSSPadding): number {
  switch (edgePiece) {
    case 'top':
      return padding.paddingTop
    case 'bottom':
      return padding.paddingBottom
    case 'right':
      return padding.paddingRight
    case 'left':
      return padding.paddingLeft
    default:
      assertNever(edgePiece)
  }
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
