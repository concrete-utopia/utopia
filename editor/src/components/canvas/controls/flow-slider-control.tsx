import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import {
  CanvasPoint,
  getRectCenter,
  mod,
  point,
  rectanglesEqual,
  windowPoint,
  zeroCanvasRect,
} from '../../../core/shared/math-utils'
import { arrayEquals } from '../../../core/shared/utils'
import { Modifier } from '../../../utils/modifiers'
import { when } from '../../../utils/react-conditionals'
import { Icons, useColorTheme } from '../../../uuiui'
import { CSSCursor } from '../../../uuiui-deps'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { stopPropagation } from '../../inspector/common/inspector-utils'
import CanvasActions from '../canvas-actions'
import { createInteractionViaMouse, flowSlider } from '../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../dom-lookup'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { ElementPath } from '../../../core/shared/project-file-types'
import { IS_TEST_ENVIRONMENT } from '../../../common/env-vars'
import { findNewIndex } from '../canvas-strategies/flow-reorder-helpers'

export const IconSize = 16
const IndicatorSize = (scale: number) => IconSize / scale
const MenuHeight = (scale: number) => 22 / scale
const AnimatedIndicatorOffset = (scale: number) => 2 / scale
const ControlSize = (scale: number) => 10 / scale

export const FlowSliderControl = React.memo(() => {
  const scale = useEditorState((store) => store.editor.canvas.scale, 'FlowSliderControl scale')
  const colorTheme = useColorTheme()
  const isDragging = useEditorState(
    (store) =>
      store.editor.canvas.interactionSession != null &&
      store.editor.canvas.interactionSession.activeControl.type === 'FLOW_SLIDER',
    'FlowSliderControl isDragging',
  )

  const { siblings, latestIndex, startingIndex, startingFrame } = useEditorState(
    (store) => {
      if (store.editor.selectedViews.length === 1) {
        const target = store.editor.selectedViews[0]
        const siblingPaths = MetadataUtils.getSiblingsProjectContentsOrdered(
          store.editor.canvas.interactionSession?.latestMetadata ?? store.editor.jsxMetadata,
          target,
        ).map((sibling) => sibling.elementPath)
        const targetIndex = siblingPaths.findIndex((sibling) => EP.pathsEqual(sibling, target))
        const latestFrame =
          MetadataUtils.getFrameInCanvasCoords(target, store.editor.jsxMetadata) ?? zeroCanvasRect

        if (isDragging) {
          const startingSiblingsMetadata = MetadataUtils.getSiblings(
            store.strategyState.startingMetadata,
            target,
          ).map((sibling) => sibling.elementPath)
          return {
            siblings: siblingPaths,
            latestIndex: targetIndex,
            startingIndex: startingSiblingsMetadata.findIndex((sibling) =>
              EP.pathsEqual(sibling, target),
            ),
            startingFrame:
              MetadataUtils.getFrameInCanvasCoords(target, store.strategyState.startingMetadata) ??
              zeroCanvasRect,
          }
        } else {
          return {
            siblings: siblingPaths,
            latestIndex: targetIndex,
            startingIndex: targetIndex,
            startingFrame: latestFrame,
          }
        }
      } else {
        return { siblings: [], latestIndex: -1, startingIndex: -1, startingFrame: zeroCanvasRect }
      }
    },
    'FlowSliderControl',
    (oldValue, newValue) =>
      arrayEquals(oldValue.siblings, newValue.siblings, EP.pathsEqual) &&
      oldValue.latestIndex === newValue.latestIndex &&
      oldValue.startingIndex === newValue.startingIndex &&
      rectanglesEqual(oldValue.startingFrame, newValue.startingFrame),
  )

  const controlAreaTopLeft = React.useMemo(() => {
    const centerPoint = getRectCenter(startingFrame ?? zeroCanvasRect)
    return {
      x: centerPoint.x - IndicatorSize(scale) / 2 - startingIndex * IndicatorSize(scale),
      y: centerPoint.y - MenuHeight(scale) / 2,
    } as CanvasPoint
  }, [startingFrame, startingIndex, scale])

  const controlTopLeft = React.useMemo(() => {
    return {
      x: controlAreaTopLeft.x + latestIndex * IndicatorSize(scale) + AnimatedIndicatorOffset(scale),
      y: getRectCenter(startingFrame ?? zeroCanvasRect).y - ControlSize(scale) / 2,
    } as CanvasPoint
  }, [startingFrame, controlAreaTopLeft, latestIndex, scale])

  if (siblings.length > 1) {
    return (
      <CanvasOffsetWrapper>
        {when(
          isDragging,
          <div
            style={{
              position: 'absolute',
              top: controlAreaTopLeft.y,
              left: controlAreaTopLeft.x,
              width: siblings.length * IndicatorSize(scale),
              height: MenuHeight(scale),
              borderRadius: 4 / scale,
              opacity: '50%',
              background: colorTheme.bg0.value,
              boxShadow: `inset 0px 0px 0px ${0.5 / scale}px ${colorTheme.border3.value} , 0px ${
                2 / scale
              }px ${4 / scale}px 0px ${colorTheme.fg6.o(50).value}`,
              cursor: CSSCursor.ResizeEW,
              display: 'flex',
              alignItems: 'center',
              boxSizing: 'content-box',
            }}
          >
            {siblings.map((s) => {
              return (
                <div key={EP.toString(s)} style={{ zoom: 1 / scale }}>
                  <Icons.Dot width={IconSize} height={IconSize} />
                </div>
              )
            })}
          </div>,
        )}
        {when(
          isDragging,
          <AnimatedReorderIndicator
            startingIndex={startingIndex}
            controlAreaTopLeft={controlAreaTopLeft}
            siblings={siblings}
          />,
        )}
        <FlowReorderControl controlPosition={controlTopLeft} />
      </CanvasOffsetWrapper>
    )
  } else {
    return null
  }
})

interface AnimatedReorderIndicatorProps {
  controlAreaTopLeft: CanvasPoint
  startingIndex: number
  siblings: Array<ElementPath>
}

const AnimatedReorderIndicator = React.memo((props: AnimatedReorderIndicatorProps) => {
  const colorTheme = useColorTheme()
  const scale = useEditorState(
    (store) => store.editor.canvas.scale,
    'AnimatedReorderIndicator scale',
  )
  const { controlAreaTopLeft, startingIndex, siblings } = props
  const indicatorOffset = useEditorState((store) => {
    if (
      store.editor.canvas.interactionSession != null &&
      store.editor.canvas.interactionSession.activeControl.type === 'FLOW_SLIDER' &&
      store.editor.canvas.interactionSession.interactionData.type === 'DRAG'
    ) {
      const dragDelta = store.editor.canvas.interactionSession.interactionData.accumulatedMovement
      return findNewIndex(startingIndex, dragDelta, siblings, 'raw-value')
    } else {
      return startingIndex
    }
  }, 'FlowSliderControl indicatorOffset')

  return (
    <div
      style={{
        position: 'absolute',
        top: controlAreaTopLeft.y + AnimatedIndicatorOffset(scale),
        left: controlAreaTopLeft.x + indicatorOffset * IndicatorSize(scale),
        width: IndicatorSize(scale) - AnimatedIndicatorOffset(scale),
        height: MenuHeight(scale) - AnimatedIndicatorOffset(scale) * 2,
        borderRadius: 4 / scale,
        background: colorTheme.primary.value,
        transition: 'left 0.05s linear',
        opacity: 0.6,
      }}
    ></div>
  )
})

const FlowReorderControl = React.memo(({ controlPosition }: { controlPosition: CanvasPoint }) => {
  const colorTheme = useColorTheme()
  const scale = useEditorState((store) => store.editor.canvas.scale, 'FlowReorderControl scale')
  const ref = React.useRef<HTMLDivElement>(null)
  const ClickAreaSize = ControlSize(scale) + 6 / scale

  const dispatch = useRefEditorState((store) => store.dispatch)
  const scaleRef = useRefEditorState((store) => store.editor.canvas.scale)
  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.realCanvasOffset)

  const onMouseDown = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      stopPropagation(event)
      const startPoint = windowToCanvasCoordinates(
        scaleRef.current,
        canvasOffsetRef.current,
        windowPoint(point(event.clientX, event.clientY)),
      ).canvasPositionRounded
      if (ref.current != null && !IS_TEST_ENVIRONMENT) {
        // real user input is needed for enabling pointerLock
        ref.current.requestPointerLock()
      }
      if (event.button !== 2) {
        dispatch.current(
          [
            CanvasActions.createInteractionSession(
              createInteractionViaMouse(
                startPoint,
                Modifier.modifiersForEvent(event),
                flowSlider(),
              ),
            ),
          ],
          'everyone',
        )
      }
    },
    [dispatch, canvasOffsetRef, scaleRef],
  )

  return (
    <React.Fragment>
      <div
        style={{
          position: 'absolute',
          top: controlPosition.y,
          left: controlPosition.x,
          width: ControlSize(scale),
          height: ControlSize(scale),
          borderRadius: '50%',
          background: colorTheme.bg0.value,
          boxShadow: `inset 0px 0px 0px ${0.5 / scale}px ${colorTheme.border3.value} , 0px ${
            2 / scale
          }px ${4 / scale}px 0px ${colorTheme.fg6.o(50).value}`,
        }}
      ></div>
      <div
        ref={ref}
        data-testid='flow-reorder-slider-control'
        style={{
          position: 'absolute',
          top: controlPosition.y - (ClickAreaSize - ControlSize(scale)) / 2,
          left: controlPosition.x - (ClickAreaSize - ControlSize(scale)) / 2,
          width: ClickAreaSize,
          height: ClickAreaSize,
          cursor: CSSCursor.ResizeEW,
        }}
        onMouseDown={onMouseDown}
      ></div>
    </React.Fragment>
  )
})
