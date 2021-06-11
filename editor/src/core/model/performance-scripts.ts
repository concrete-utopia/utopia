import * as React from 'react'
import * as ReactDOM from 'react-dom'
import CanvasActions from '../../components/canvas/canvas-actions'
import { DebugDispatch } from '../../components/editor/action-types'
import {
  clearSelection,
  selectComponents,
  switchEditorMode,
} from '../../components/editor/actions/action-creators'
import { useEditorState, useRefEditorState } from '../../components/editor/store/store-hook'
import {
  canvasPoint,
  CanvasRectangle,
  CanvasVector,
  zeroPoint,
  zeroRectangle,
} from '../shared/math-utils'
import { resizeDragState } from '../../components/canvas/canvas-types'
import { MetadataUtils } from './element-metadata-utils'
import { getOriginalFrames } from '../../components/canvas/canvas-utils'
import * as EP from '../shared/element-path'
import { EditorModes } from '../../components/editor/editor-modes'

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
      if (framesPassed < 100) {
        framesPassed++
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
  const metadata = useRefEditorState((store) => store.editor.jsxMetadata)
  const selectedViews = useRefEditorState((store) => store.editor.selectedViews)
  const trigger = React.useCallback(async () => {
    if (selectedViews.current.length === 0) {
      console.info('RESIZE_TEST_MISSING_SELECTEDVIEW')
      return
    }
    await dispatch([switchEditorMode(EditorModes.selectMode())]).entireUpdateFinished

    const target = selectedViews.current[0]
    const targetFrame = MetadataUtils.findElementByElementPath(metadata.current, target)
      ?.globalFrame
    const targetStartPoint =
      targetFrame != null
        ? ({
            x: targetFrame.x + targetFrame.width,
            y: targetFrame.y + targetFrame.height,
          } as CanvasVector)
        : (zeroPoint as CanvasVector)
    const originalFrames = getOriginalFrames(selectedViews.current, metadata.current)

    let framesPassed = 0
    async function step() {
      performance.mark(`resize_step_${framesPassed}`)
      const dragState = resizeDragState(
        targetStartPoint,
        { x: framesPassed % 100, y: framesPassed % 100 } as CanvasVector,
        true,
        false,
        false,
        targetFrame ?? (zeroRectangle as CanvasRectangle),
        originalFrames,
        { x: 1, y: 1 },
        { x: 1, y: 1 },
        metadata.current,
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
      if (framesPassed < 100) {
        framesPassed++
        requestAnimationFrame(step)
      } else {
        await dispatch([CanvasActions.clearDragState(false)]).entireUpdateFinished
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
  const allPaths = useRefEditorState((store) => store.derived.navigatorTargets)
  const trigger = React.useCallback(async () => {
    if (allPaths.current.length === 0) {
      console.info('SELECT_TEST_ERROR')
      return
    }

    const targetPath = [...allPaths.current].sort(
      (a, b) => EP.toString(b).length - EP.toString(a).length,
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

      if (framesPassed < 100) {
        framesPassed++
        requestAnimationFrame(step)
      } else {
        console.info('SELECT_TEST_FINISHED')
      }
    }
    requestAnimationFrame(step)
  }, [dispatch, allPaths])
  return trigger
}

export function useTriggerBaselinePerformanceTest(): () => void {
  const dispatch = useEditorState(
    (store) => store.dispatch as DebugDispatch,
    'useTriggerSelectionPerformanceTest dispatch',
  )

  const trigger = React.useCallback(async () => {
    let framesPassed = 0
    async function step() {
      performance.mark(`baseline_step_${framesPassed}`)
      for (let i = 0; i < 3000; i++) {
        await dispatch([]).entireUpdateFinished
      }
      performance.mark(`baseline_dispatch_finished_${framesPassed}`)
      performance.measure(
        `baseline_frame_${framesPassed}`,
        `baseline_step_${framesPassed}`,
        `baseline_dispatch_finished_${framesPassed}`,
      )

      if (framesPassed < 100) {
        framesPassed++
        requestAnimationFrame(step)
      } else {
        requestAnimationFrame(() => console.info('BASELINE_TEST_FINISHED'))
      }
    }
    requestAnimationFrame(step)
  }, [dispatch])

  return trigger
}
