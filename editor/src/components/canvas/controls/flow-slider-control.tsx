/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { css, jsx } from '@emotion/react'
import { useSpring, animated } from 'react-spring'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import {
  CanvasPoint,
  easeOutCubic,
  getRectCenter,
  point,
  rectanglesEqual,
  windowPoint,
  zeroCanvasRect,
} from '../../../core/shared/math-utils'
import { arrayEquals } from '../../../core/shared/utils'
import { Modifier } from '../../../utils/modifiers'
import { when } from '../../../utils/react-conditionals'
import { Icons, useColorTheme, UtopiaStyles } from '../../../uuiui'
import { CSSCursor } from '../../../uuiui-deps'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { stopPropagation } from '../../inspector/common/inspector-utils'
import CanvasActions from '../canvas-actions'
import { createInteractionViaMouse, flowSlider } from '../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../dom-lookup'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

const IndicatorSize = 16
const MenuHeight = 22
const AnimatedIndicatorOffset = 2
const ControlSize = 10
const ClickAreaSize = ControlSize + 6

export const FlowSliderControl = React.memo(() => {
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
      x: centerPoint.x - IndicatorSize / 2 - startingIndex * IndicatorSize,
      y: centerPoint.y - MenuHeight / 2,
    } as CanvasPoint
  }, [startingFrame, startingIndex])

  const controlTopLeft = React.useMemo(() => {
    return {
      x: controlAreaTopLeft.x + latestIndex * IndicatorSize + AnimatedIndicatorOffset,
      y: getRectCenter(startingFrame ?? zeroCanvasRect).y - ControlSize / 2,
    } as CanvasPoint
  }, [startingFrame, controlAreaTopLeft, latestIndex])

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
              width: siblings.length * IndicatorSize,
              height: MenuHeight,
              borderRadius: 4,
              opacity: '50%',
              background: colorTheme.bg0.value,
              boxShadow: UtopiaStyles.popup.boxShadow,
              cursor: CSSCursor.ResizeEW,
              display: 'flex',
              alignItems: 'center',
            }}
          >
            {siblings.map((s) => {
              return <Icons.Dot key={EP.toString(s)} width={IndicatorSize} height={IndicatorSize} />
            })}
          </div>,
        )}
        {when(
          isDragging,
          <AnimatedReorderIndicator
            latestIndex={startingIndex}
            controlAreaTopLeft={controlAreaTopLeft}
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
  latestIndex: number
}

const AnimatedReorderIndicator = React.memo((props: AnimatedReorderIndicatorProps) => {
  const colorTheme = useColorTheme()
  const { controlAreaTopLeft, latestIndex } = props

  // const indicatorOffset = useEditorState((store) => {
  //   const indexPositionBetweenElements = store.editor.canvas.controls.flowReorderIndexPosition
  //   if (indexPositionBetweenElements != null) {
  //     // return easeOutCubic()
  //     return indexPositionBetweenElements
  //   } else {
  //     return 0
  //   }
  // }, 'FlowSliderControl indicatorOffset')
  const indicatorOffset = useEditorState((store) => {
    if (
      store.editor.canvas.interactionSession != null &&
      store.editor.canvas.interactionSession.activeControl.type === 'FLOW_SLIDER' &&
      store.editor.canvas.interactionSession.interactionData.type === 'DRAG' &&
      store.editor.canvas.interactionSession.interactionData.drag != null
    ) {
      return store.editor.canvas.interactionSession.interactionData.drag.x / 16
    } else {
      return 0
    }
  }, 'FlowSliderControl indicatorOffset')

  // const styles = useSpring({
  //   left: controlAreaTopLeft.x + latestIndex * IndicatorSize + indicatorOffset,
  //   config: { mass: 1, tension: 170, friction: 26 },
  // })

  return (
    <div
      style={{
        position: 'absolute',
        top: controlAreaTopLeft.y + AnimatedIndicatorOffset,
        // left: styles.left,
        left: controlAreaTopLeft.x + latestIndex * IndicatorSize + indicatorOffset * IndicatorSize,
        width: IndicatorSize - AnimatedIndicatorOffset,
        height: MenuHeight - AnimatedIndicatorOffset * 2,
        borderRadius: 4,
        background: colorTheme.primary.value,
        transition: 'left 0.2s ease',
      }}
      css={{
        opacity: 0.6,
      }}
    ></div>
  )
})

const FlowReorderControl = React.memo(({ controlPosition }: { controlPosition: CanvasPoint }) => {
  const colorTheme = useColorTheme()
  const ref = React.useRef<HTMLDivElement>(null)

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
      if (ref.current != null) {
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
          width: ControlSize,
          height: ControlSize,
          borderRadius: '50%',
          background: colorTheme.bg0.value,
          boxShadow: UtopiaStyles.popup.boxShadow,
        }}
      ></div>
      <div
        ref={ref}
        style={{
          position: 'absolute',
          top: controlPosition.y - (ClickAreaSize - ControlSize) / 2,
          left: controlPosition.x - (ClickAreaSize - ControlSize) / 2,
          width: ClickAreaSize,
          height: ClickAreaSize,
          cursor: CSSCursor.ResizeEW,
        }}
        onMouseDown={onMouseDown}
      ></div>
    </React.Fragment>
  )
})
