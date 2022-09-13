import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { point, windowPoint } from '../../../core/shared/math-utils'
import { Modifier } from '../../../utils/modifiers'
import { Icons, useColorTheme, UtopiaStyles } from '../../../uuiui'
import { CSSCursor, getControlStyles, SliderControl } from '../../../uuiui-deps'
import { setCursorOverlay } from '../../editor/actions/action-creators'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { stopPropagation } from '../../inspector/common/inspector-utils'
import CanvasActions from '../canvas-actions'
import { createInteractionViaMouse, flowSlider } from '../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../dom-lookup'

export const FlowSliderControl = React.memo(() => {
  const colorTheme = useColorTheme()
  const ref = React.useRef<HTMLDivElement | null>(null)
  const dispatch = useRefEditorState((store) => store.dispatch)
  const isTargetFlowWithSiblings = useEditorState((store) => {
    if (store.editor.selectedViews.length === 1) {
      const target = store.editor.selectedViews[0]
      const elementMetadata = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        target,
      )
      if (MetadataUtils.isPositionedByFlow(elementMetadata)) {
        const siblings = MetadataUtils.getSiblings(store.editor.jsxMetadata, target)
        if (
          siblings.length > 1 &&
          siblings.every((sibling) => MetadataUtils.isPositionedByFlow(sibling))
        ) {
          return true
        } else {
          return false
        }
      } else {
        return false
      }
    } else {
      return false
    }
  }, 'isTargetFlowWithSiblings')

  const { siblings, currentIndex } = useEditorState((store) => {
    if (store.editor.selectedViews.length === 1) {
      const target = store.editor.selectedViews[0]
      const siblingsMetadata = MetadataUtils.getSiblings(
        store.editor.canvas.interactionSession?.latestMetadata ?? store.editor.jsxMetadata,
        target,
      ).map((sibling) => sibling.elementPath)
      return {
        siblings: siblingsMetadata,
        currentIndex: siblingsMetadata.findIndex((sibling) => EP.pathsEqual(sibling, target)),
      }
    } else {
      return { siblings: [], currentIndex: -1 }
    }
  }, 'current index')

  // const onChange = React.useCallback((value: number, transient?: boolean | undefined) => {
  //   console.log('i')
  // }, [])

  // const onMouseDown = React.useCallback(
  //   (e: React.MouseEvent<HTMLDivElement>) => {
  //     stopPropagation(e)
  //     ref.current?.requestPointerLock()
  //     window.addEventListener('mousemove', onMouseMove)
  //     window.addEventListener('mouseup', onMouseUp)
  //   },
  //   [ref],
  // )
  // const onMouseMove = (e: MouseEvent) => {
  //   console.log('mouse move event', e)
  // }
  // const onMouseUp = (e: MouseEvent) => {
  //   document.exitPointerLock()
  //   window.removeEventListener('mousemove', onMouseMove)
  //   window.removeEventListener('mouseup', onMouseUp)
  // }

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

  if (isTargetFlowWithSiblings && siblings.length > 1) {
    return (
      <div
        ref={ref}
        style={{
          position: 'absolute',
          right: 125,
          top: 4,
          minWidth: 144,
          height: 22,
          borderRadius: 4,
          background: colorTheme.bg0.value,
          boxShadow: UtopiaStyles.popup.boxShadow,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'space-between',
          cursor: CSSCursor.ResizeEW,
        }}
        onMouseDown={onMouseDown}
        onMouseUp={stopPropagation}
      >
        {siblings.map((s, i) => {
          return (
            <div
              key={EP.toString(s)}
              style={{
                display: 'inline-block',
                transform: `scale(${i === currentIndex ? 2 : 1})`,
                transition: 'transform .1s ease-in-out',
              }}
            >
              <Icons.Dot color={i === currentIndex ? 'primary' : 'main'} />
            </div>
          )
        })}
        {/* <SliderControl
          key='flow-slider'
          id='flow-slider'
          testId='flow-slider'
          value={currentIndex}
          onSubmitValue={onChange}
          controlStatus={'simple'}
          controlStyles={getControlStyles('simple')}
          DEPRECATED_controlOptions={{
            minimum: 0,
            maximum: siblings.length,
            // filled: true,
            stepSize: 1,
            // origin: number
          }}
        /> */}
      </div>
    )
  } else {
    return null
  }
})
