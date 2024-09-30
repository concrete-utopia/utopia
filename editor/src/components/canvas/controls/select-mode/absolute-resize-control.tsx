import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import type { CanvasRectangle, CanvasVector } from '../../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  canvasRectangle,
  isInfinityRectangle,
  nullIfInfinity,
  windowPoint,
} from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever, NO_OP } from '../../../../core/shared/utils'
import { Modifier } from '../../../../utils/modifiers'
import { when } from '../../../../utils/react-conditionals'
import { useColorTheme } from '../../../../uuiui'
import type { EditorDispatch } from '../../../editor/action-types'
import { applyCommandsAction } from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { AllElementProps, getMetadata } from '../../../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import type { FixedHugFill } from '../../../inspector/inspector-common'
import {
  detectFillHugFixedState,
  invert,
  resizeToFitCommands,
} from '../../../inspector/inspector-common'
import { setPropHugStrategies } from '../../../inspector/inspector-strategies/inspector-strategies'
import { executeFirstApplicableStrategy } from '../../../inspector/inspector-strategies/inspector-strategy'
import CanvasActions from '../../canvas-actions'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { createInteractionViaMouse } from '../../canvas-strategies/interaction-state'
import type { EdgePosition } from '../../canvas-types'
import { CSSCursor } from '../../canvas-types'
import { getAllTargetsUnderAreaAABB, windowToCanvasCoordinates } from '../../dom-lookup'
import { SmallElementSize, useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'
import { useMaybeHighlightElement } from './select-mode-hooks'
import { isEdgePositionEqualTo } from '../../canvas-utils'
import { treatElementAsGroupLike } from '../../canvas-strategies/strategies/group-helpers'
import { treatElementAsFragmentLike } from '../../canvas-strategies/strategies/fragment-like-helpers'
import { ElementPathTrees } from '../../../../core/shared/element-path-tree'

export const AbsoluteResizeControlTestId = (targets: Array<ElementPath>): string =>
  `${targets.map(EP.toString).sort()}-absolute-resize-control`

interface AbsoluteResizeControlProps {
  targets: Array<ElementPath>
  pathsWereReplaced: boolean
}

export const SizeLabelID = 'SizeLabel'
export const SizeLabelTestId = 'SizeLabelTestId'

function shouldUseSmallElementResizeControl(size: number, scale: number): boolean {
  return size <= SmallElementSize / scale
}

export const AbsoluteResizeControl = controlForStrategyMemoized(
  ({ targets, pathsWereReplaced }: AbsoluteResizeControlProps) => {
    const scale = useEditorState(
      Substores.canvasOffset,
      (store) => store.editor.canvas.scale,
      'AbsoluteResizeControl scale',
    )

    const controlRef = useBoundingBox(targets, (ref, safeGappedBoundingBox, realBoundingBox) => {
      if (isZeroSizedElement(realBoundingBox)) {
        ref.current.style.display = 'none'
      } else {
        ref.current.style.display = 'block'
        ref.current.style.left = safeGappedBoundingBox.x + 'px'
        ref.current.style.top = safeGappedBoundingBox.y + 'px'
        ref.current.style.width = safeGappedBoundingBox.width + 'px'
        ref.current.style.height = safeGappedBoundingBox.height + 'px'
      }
    })

    const leftRef = useBoundingBox(targets, (ref, boundingBox) => {
      const isSmallElement = shouldUseSmallElementResizeControl(boundingBox.width, scale)
      const lineSize = ResizeMouseAreaSize / scale
      const width = isSmallElement ? lineSize / 2 : lineSize
      const offsetLeft = `${-lineSize / 2}px`
      const offsetTop = `0px`

      ref.current.style.width = `${width}px`
      ref.current.style.transform = `translate(${offsetLeft}, ${offsetTop})`
      ref.current.style.height = boundingBox.height + 'px'
    })
    const topRef = useBoundingBox(targets, (ref, boundingBox) => {
      const isSmallElement = shouldUseSmallElementResizeControl(boundingBox.height, scale)
      const lineSize = ResizeMouseAreaSize / scale
      const height = isSmallElement ? lineSize / 2 : lineSize
      const offsetLeft = `0px`
      const offsetTop = `${-lineSize / 2}px`

      ref.current.style.width = boundingBox.width + 'px'
      ref.current.style.height = height + 'px'
      ref.current.style.transform = `translate(${offsetLeft}, ${offsetTop})`
    })
    const rightRef = useBoundingBox(targets, (ref, boundingBox) => {
      const isSmallElement = shouldUseSmallElementResizeControl(boundingBox.width, scale)
      const lineSize = ResizeMouseAreaSize / scale
      const width = isSmallElement ? lineSize / 2 : lineSize
      const offsetLeft = isSmallElement ? `0px` : `${-lineSize / 2}px`
      const offsetTop = `0px`

      ref.current.style.transform = `translate(${offsetLeft}, ${offsetTop})`
      ref.current.style.left = boundingBox.width + 'px'
      ref.current.style.width = width + 'px'
      ref.current.style.height = boundingBox.height + 'px'
    })

    const bottomRef = useBoundingBox(targets, (ref, boundingBox) => {
      const isSmallElement = shouldUseSmallElementResizeControl(boundingBox.height, scale)
      const lineSize = ResizeMouseAreaSize / scale
      const height = isSmallElement ? lineSize / 2 : lineSize
      const offsetLeft = `0px`
      const offsetTop = isSmallElement ? `0px` : `${-lineSize / 2}px`

      ref.current.style.transform = `translate(${offsetLeft}, ${offsetTop})`
      ref.current.style.top = boundingBox.height + 'px'
      ref.current.style.width = boundingBox.width + 'px'
      ref.current.style.height = height + 'px'
    })

    const topLeftRef = useBoundingBox(targets, NO_OP)
    const topRightRef = useBoundingBox(targets, (ref, boundingBox) => {
      ref.current.style.left = boundingBox.width + 'px'
    })
    const bottomLeftRef = useBoundingBox(targets, (ref, boundingBox) => {
      ref.current.style.top = boundingBox.height + 'px'
    })
    const bottomRightRef = useBoundingBox(targets, (ref, boundingBox) => {
      ref.current.style.left = boundingBox.width + 'px'
      ref.current.style.top = boundingBox.height + 'px'
    })

    const resizeRef = useBoundingBox(targets, (ref, boundingBox) => {
      ref.current.style.top = boundingBox.height + 'px'
      ref.current.style.left = 0 + 'px'
      ref.current.style.width = boundingBox.width + 'px'
    })

    return (
      <CanvasOffsetWrapper>
        <div
          data-testid={AbsoluteResizeControlTestId(targets)}
          ref={controlRef}
          style={{
            position: 'absolute',
            pointerEvents: 'none',
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
          <ResizePoint
            ref={bottomLeftRef}
            position={{ x: 0, y: 1 }}
            cursor={CSSCursor.ResizeNESW}
          />
          <ResizePoint
            ref={bottomRightRef}
            position={{ x: 1, y: 1 }}
            cursor={CSSCursor.ResizeNWSE}
          />
          <SizeLabel ref={resizeRef} targets={targets} pathsWereReplaced={pathsWereReplaced} />
        </div>
      </CanvasOffsetWrapper>
    )
  },
)

interface ResizePointProps {
  cursor: CSSCursor
  position: EdgePosition
}

export const ResizePointTestId = (position: EdgePosition): string =>
  `resize-control-${position.x}-${position.y}`
const ResizePointMouseAreaSize = 12
const ResizePointMouseAreaOffset = ResizePointMouseAreaSize / 2
const ResizePointSize = 6
const ResizePointOffset = ResizePointSize / 2
const ResizePoint = React.memo(
  React.forwardRef<HTMLDivElement, ResizePointProps>((props, ref) => {
    const colorTheme = useColorTheme()
    const { maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()
    const scale = useEditorState(
      Substores.canvas,
      (store) => store.editor.canvas.scale,
      'ResizeEdge scale',
    )
    const dispatch = useDispatch()
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

    const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
    const selectedElementsRef = useRefEditorState((store) => store.editor.selectedViews)
    const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
    const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

    const onEdgeDblClick = React.useCallback(() => {
      dispatch([
        applyCommandsAction(
          resizeToFitCommands(
            metadataRef.current,
            selectedElementsRef.current,
            elementPathTreeRef.current,
            allElementPropsRef.current,
          ),
        ),
      ])
    }, [allElementPropsRef, dispatch, metadataRef, elementPathTreeRef, selectedElementsRef])

    const hiddenDuringInteraction = useEditorState(
      Substores.canvas,
      (store) =>
        store.editor.canvas.interactionSession != null &&
        store.editor.canvas.interactionSession.activeControl.type === 'RESIZE_HANDLE' &&
        !isEdgePositionEqualTo(
          props.position,
          store.editor.canvas.interactionSession.activeControl.edgePosition,
        ),
      'ResizePoint hiddenDuringInteraction',
    )

    return (
      <div
        ref={ref}
        style={{
          position: 'absolute',
          pointerEvents: 'none',
          visibility: hiddenDuringInteraction ? 'hidden' : 'visible',
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
            boxShadow: `${colorTheme.canvasControlsSizeBoxShadowColor50.value} 0px 0px
              ${1 / scale}px, ${colorTheme.canvasControlsSizeBoxShadowColor20.value} 0px ${
              1 / scale
            }px ${2 / scale}px ${1 / scale}px`,
          }}
        />
        <div
          onDoubleClick={onEdgeDblClick}
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
          data-testid={ResizePointTestId(props.position)}
        />
      </div>
    )
  }),
)
ResizePoint.displayName = 'ResizePoint'

interface ResizeEdgeProps {
  cursor: CSSCursor
  direction: 'horizontal' | 'vertical'
  position: EdgePosition
}

const ResizeMouseAreaSize = 10
const ResizeEdge = React.memo(
  React.forwardRef<HTMLDivElement, ResizeEdgeProps>((props, ref) => {
    const scale = useEditorState(
      Substores.canvasOffset,
      (store) => store.editor.canvas.scale,
      'ResizeEdge scale',
    )
    const dispatch = useDispatch()
    const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
    const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
    const selectedElementsRef = useRefEditorState((store) => store.editor.selectedViews)
    const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)

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

    const onEdgeDblClick = React.useCallback(() => {
      executeFirstApplicableStrategy(
        dispatch,
        setPropHugStrategies(
          metadataRef.current,
          selectedElementsRef.current,
          elementPathTreeRef.current,
          invert(props.direction),
        ),
      )
    }, [dispatch, metadataRef, props.direction, elementPathTreeRef, selectedElementsRef])

    return (
      <div
        onDoubleClick={onEdgeDblClick}
        ref={ref}
        style={{
          position: 'absolute',
          backgroundColor: 'transparent',
          cursor: props.cursor,
          pointerEvents: 'initial',
        }}
        onMouseDown={onEdgeMouseDown}
        onMouseMove={onMouseMove}
        data-testid={ResizePointTestId(props.position)}
      />
    )
  }),
)

const sizeLabel = (state: FixedHugFill['type'], actualSize: number): string => {
  switch (state) {
    case 'fill':
      return 'Fill'
    case 'hug':
    case 'hug-group':
      return 'Hug'
    case 'squeeze':
      return 'Squeeze'
    case 'collapsed':
      return 'Collapsed'
    case 'fixed':
    case 'scaled':
    case 'detected':
    case 'computed':
      return `${actualSize}`
    case 'stretch':
      return 'Stretch'
    default:
      assertNever(state)
  }
}

export type SizeLabelSize = {
  type: 'SIZE_LABEL_WITH_DIMENSIONS'
  h: string
  v: string
}

function sizeLabelWithDimensions(h: string, v: string): SizeLabelSize {
  return {
    type: 'SIZE_LABEL_WITH_DIMENSIONS',
    h: h,
    v: v,
  }
}

export type SizeLabelGroup = {
  type: 'SIZE_LABEL_GROUP'
}

function sizeLabelGroup(): SizeLabelGroup {
  return {
    type: 'SIZE_LABEL_GROUP',
  }
}

export type SizeLabelChildren = {
  type: 'SIZE_LABEL_CHILDREN'
  h: string
  v: string
}

function sizeLabelChildren(h: string, v: string): SizeLabelChildren {
  return {
    type: 'SIZE_LABEL_CHILDREN',
    h: h,
    v: v,
  }
}

export type SizeLabelContents = SizeLabelSize | SizeLabelGroup | SizeLabelChildren

function sizeLabelContents(
  metadata: ElementInstanceMetadataMap,
  selectedElements: Array<ElementPath>,
  boundingBox: CanvasRectangle | null,
  pathsWereReplaced: boolean,
): SizeLabelContents | null {
  if (selectedElements.length === 0) {
    return null
  }

  if (selectedElements.length === 1) {
    if (treatElementAsGroupLike(metadata, selectedElements[0])) {
      return sizeLabelGroup()
    }

    const globalFrame = MetadataUtils.findElementByElementPath(
      metadata,
      selectedElements[0],
    )?.globalFrame
    if (globalFrame == null || isInfinityRectangle(globalFrame)) {
      return null
    }

    const horizontal =
      detectFillHugFixedState('horizontal', metadata, selectedElements[0]).fixedHugFill?.type ??
      'fixed'
    const vertical =
      detectFillHugFixedState('vertical', metadata, selectedElements[0]).fixedHugFill?.type ??
      'fixed'

    if (pathsWereReplaced) {
      return sizeLabelChildren(
        sizeLabel(horizontal, globalFrame.width),
        sizeLabel(vertical, globalFrame.height),
      )
    }

    return sizeLabelWithDimensions(
      sizeLabel(horizontal, globalFrame.width),
      sizeLabel(vertical, globalFrame.height),
    )
  }

  if (boundingBox != null) {
    return sizeLabelWithDimensions(`${boundingBox.width}`, `${boundingBox.height}`)
  }

  return null
}

interface SizeLabelProps {
  targets: Array<ElementPath>
  pathsWereReplaced: boolean
}

const FontSize = 11
const PaddingV = 0
const PaddingH = 2
const ExplicitHeightHacked = 20
const BorderRadius = 2
const SizeLabelMarginTop = 8

function getLabelText(label: SizeLabelContents | null): string | null {
  if (label == null) {
    return null
  }
  switch (label.type) {
    case 'SIZE_LABEL_GROUP':
      return 'Group'
    case 'SIZE_LABEL_WITH_DIMENSIONS':
      return `${label.h} x ${label.v}`
    case 'SIZE_LABEL_CHILDREN':
      return `(Children) ${label.h} x ${label.v}`
    default:
      assertNever(label)
  }
}

const SizeLabel = React.memo(
  React.forwardRef<HTMLDivElement, SizeLabelProps>(({ targets, pathsWereReplaced }, ref) => {
    const scale = useEditorState(
      Substores.canvas,
      (store) => store.editor.canvas.scale,
      'Resizelabel scale',
    )
    const colorTheme = useColorTheme()
    const metadata = useEditorState(
      Substores.metadata,
      (store) => getMetadata(store.editor),
      'ResizeLabel metadata',
    )

    const boundingBox = boundingRectangleArray(
      targets.map((t) => nullIfInfinity(MetadataUtils.getFrameInCanvasCoords(t, metadata))),
    )

    const label = sizeLabelContents(metadata, targets, boundingBox, pathsWereReplaced)
    const labelText = getLabelText(label)

    const [dimmed, setDimmed] = React.useState(false)

    const editorRef = useRefEditorState((store) => ({
      scale: store.editor.canvas.scale,
      offset: store.editor.canvas.roundedCanvasOffset,
      jsxMetadata: store.editor.jsxMetadata,
      elementPathTree: store.editor.elementPathTree,
      allElementProps: store.editor.allElementProps,
      hiddenInstances: store.editor.hiddenInstances,
    }))

    const onMouseEnter = React.useCallback(() => {
      const distanceBetweenBoxAndLabel = 10 // px
      const labelRect = document.getElementById(SizeLabelID)?.getBoundingClientRect()
      if (boundingBox != null && labelRect != null) {
        const area = canvasRectangle({
          x: boundingBox.x + (boundingBox.width - labelRect.width) / 2,
          y:
            boundingBox.y +
            boundingBox.height +
            distanceBetweenBoxAndLabel / editorRef.current.scale,
          width: labelRect.width,
          height: labelRect.height,
        })
        const elementsUnderLabel = getAllTargetsUnderAreaAABB(
          editorRef.current.jsxMetadata,
          [],
          editorRef.current.hiddenInstances,
          'no-filter',
          area,
          editorRef.current.elementPathTree,
          editorRef.current.allElementProps,
          true,
        )
        setDimmed(elementsUnderLabel.length > 0)
      }
    }, [editorRef, boundingBox])

    const onMouseLeave = React.useCallback(() => {
      setDimmed(false)
    }, [])

    return (
      <div
        ref={ref}
        style={{
          position: 'absolute',
          display: 'flex',
          justifyContent: 'center',
          pointerEvents: 'none',
        }}
        data-testid='parent-resize-label'
      >
        {when(
          labelText != null,
          <div
            id={SizeLabelID}
            data-testid={SizeLabelTestId}
            style={{
              display: 'flex',
              alignItems: 'center',
              marginTop: SizeLabelMarginTop / scale,
              padding: `${PaddingV}px ${PaddingH / scale}px`,
              borderRadius: BorderRadius / scale,
              color: colorTheme.white.value,
              backgroundColor: colorTheme.primary.value,
              fontSize: FontSize / scale,
              height: ExplicitHeightHacked / scale,
              opacity: dimmed ? 0.075 : 1,
              transition: '0.1s',
              pointerEvents: 'initial',
            }}
            onMouseEnter={onMouseEnter}
            onMouseLeave={onMouseLeave}
          >
            {labelText}
          </div>,
        )}
      </div>
    )
  }),
)
SizeLabel.displayName = 'SizeLabel'

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
          'zero-drag-not-permitted',
        ),
      ),
    ])
  }
}
