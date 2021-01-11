import * as React from 'react'
import * as ReactDOM from 'react-dom'
import CanvasActions from '../../components/canvas/canvas-actions'
import { DebugDispatch } from '../../components/editor/action-types'
import { clearSelection, selectComponents } from '../../components/editor/actions/action-creators'
import { useEditorState } from '../../components/editor/store/store-hook'
import {
  canvasPoint,
  CanvasRectangle,
  CanvasVector,
  zeroPoint,
  zeroRectangle,
} from '../shared/math-utils'
import { resizeDragState } from '../../components/canvas/canvas-types'
import { MetadataUtils } from './element-metadata-utils'
import { InstancePath } from '../shared/project-file-types'
import { getOriginalFrames } from '../../components/canvas/canvas-utils'
import * as TP from '../../core/shared/template-path'

export function useTriggerScrollPerformanceTest(): () => void {
  const dispatch = useEditorState(
    (store) => store.dispatch as DebugDispatch,
    'useTriggerScrollPerformanceTest dispatch',
  )
  const trigger = React.useCallback(async () => {
    let framesPassed = 0
    async function step() {
      performance.mark(`scroll_step_${framesPassed}`)
      await dispatch([CanvasActions.scrollCanvas(canvasPoint({ x: -5, y: -1 }))])
        .entireUpdateFinished
      performance.mark(`scroll_dispatch_finished_${framesPassed}`)
      performance.measure(
        `scroll_frame_${framesPassed}`,
        `scroll_step_${framesPassed}`,
        `scroll_dispatch_finished_${framesPassed}`,
      )
      framesPassed++
      if (framesPassed < 600) {
        requestAnimationFrame(step)
      } else {
        console.info('SCROLL_TEST_FINISHED')
      }
    }
    requestAnimationFrame(step)
  }, [dispatch])
  return trigger
}

export function useTriggerResizePerformanceTest(): () => void {
  const dispatch = useEditorState(
    (store) => store.dispatch as DebugDispatch,
    'useTriggerResizePerformanceTest dispatch',
  )
  const metadata = useEditorState(
    (store) => store.editor.jsxMetadataKILLME,
    'useTriggerResizePerformanceTest metadata',
  )
  const selectedViews = useEditorState(
    (store) => store.editor.selectedViews,
    'useTriggerResizePerformanceTest selectedViews',
  )
  const trigger = React.useCallback(async () => {
    if (selectedViews.length === 0) {
      console.info('RESIZE_TEST_MISSING_SELECTEDVIEW')
      return
    }

    const target = selectedViews[0]
    const targetFrame = MetadataUtils.getElementByInstancePathMaybe(
      metadata.elements,
      target as InstancePath,
    )?.globalFrame
    const targetStartPoint =
      targetFrame != null
        ? ({
            x: targetFrame.x + targetFrame.width,
            y: targetFrame.y + targetFrame.height,
          } as CanvasVector)
        : (zeroPoint as CanvasVector)
    const originalFrames = getOriginalFrames(selectedViews, metadata)

    let framesPassed = 0
    async function step() {
      performance.mark(`resize_step_${framesPassed}`)
      const dragState = resizeDragState(
        targetStartPoint,
        { x: framesPassed / 10, y: framesPassed / 10 } as CanvasVector,
        true,
        false,
        false,
        targetFrame || (zeroRectangle as CanvasRectangle),
        originalFrames,
        { x: 1, y: 1 },
        { x: 1, y: 1 },
        metadata,
        [target],
        false,
      )
      await dispatch([CanvasActions.createDragState(dragState)]).entireUpdateFinished
      performance.mark(`resize_dispatch_finished_${framesPassed}`)
      performance.measure(
        `resize_frame_${framesPassed}`,
        `resize_step_${framesPassed}`,
        `resize_dispatch_finished_${framesPassed}`,
      )
      framesPassed++
      if (framesPassed < 600) {
        requestAnimationFrame(step)
      } else {
        await dispatch([CanvasActions.clearDragState(true)]).entireUpdateFinished
        console.info('RESIZE_TEST_FINISHED')
      }
    }
    requestAnimationFrame(step)
  }, [dispatch, metadata, selectedViews])
  return trigger
}

export function useTriggerSelectionPerformanceTest(): () => void {
  const dispatch = useEditorState(
    (store) => store.dispatch as DebugDispatch,
    'useTriggerSelectionPerformanceTest dispatch',
  )
  const allPaths = useEditorState(
    (store) => store.derived.navigatorTargets,
    'useTriggerResizePerformanceTest navigatorTargets',
  )
  const trigger = React.useCallback(async () => {
    if (allPaths.length === 0) {
      console.info('SELECT_TEST_MISSING_ELEMENTS')
      return
    }

    const targetPath = [...allPaths].sort(
      (a, b) => TP.toString(b).length - TP.toString(a).length,
    )[0]
    let framesPassed = 0
    async function step() {
      performance.mark(`select_step_${framesPassed}`)
      await dispatch([selectComponents([targetPath!], false)]).entireUpdateFinished
      performance.mark(`select_dispatch_finished_${framesPassed}`)
      performance.measure(
        `select_frame_${framesPassed}`,
        `select_step_${framesPassed}`,
        `select_dispatch_finished_${framesPassed}`,
      )

      performance.mark(`select_deselect_step_${framesPassed}`)
      await dispatch([clearSelection()]).entireUpdateFinished
      performance.mark(`select_deselect_dispatch_finished_${framesPassed}`)
      performance.measure(
        `select_deselect_frame_${framesPassed}`,
        `select_deselect_step_${framesPassed}`,
        `select_deselect_dispatch_finished_${framesPassed}`,
      )

      framesPassed++
      if (framesPassed < 5) {
        requestAnimationFrame(step)
      } else {
        console.info('SELECT_TEST_FINISHED')
      }
    }
    requestAnimationFrame(step)
  }, [dispatch, allPaths])
  return trigger
}
