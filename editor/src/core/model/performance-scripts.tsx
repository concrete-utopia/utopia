import React from 'react'
import * as ReactDOM from 'react-dom'
import CanvasActions from '../../components/canvas/canvas-actions'
import { DebugDispatch, DispatchPriority, EditorAction } from '../../components/editor/action-types'
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
  windowPoint,
  zeroPoint,
  zeroRectangle,
} from '../shared/math-utils'
import {
  CanvasContainerID,
  resizeDragState,
  updateResizeDragState,
} from '../../components/canvas/canvas-types'
import { MetadataUtils } from './element-metadata-utils'
import { getOriginalFrames } from '../../components/canvas/canvas-utils'
import * as EP from '../shared/element-path'
import { EditorModes } from '../../components/editor/editor-modes'
import {
  useCalculateHighlightedViews,
  useGetSelectableViewsForSelectMode,
} from '../../components/canvas/controls/select-mode/select-mode-hooks'
import { CanvasControlsContainerID } from '../../components/canvas/controls/new-canvas-controls'
import { forceNotNull } from '../shared/optional-utils'
import { ElementPathArrayKeepDeepEquality } from '../../utils/deep-equality-instances'

export function wait(timeout: number): Promise<void> {
  return new Promise((resolve) => {
    setTimeout(resolve, timeout)
  })
}

const NumberOfIterations = 100

export function useTriggerScrollPerformanceTest(): () => void {
  const dispatch = useEditorState(
    (store) => store.dispatch as DebugDispatch,
    'useTriggerScrollPerformanceTest dispatch',
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

    await dispatch([selectComponents([targetPath!], false)]).entireUpdateFinished

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
      if (framesPassed < NumberOfIterations) {
        framesPassed++
        requestAnimationFrame(step)
      } else {
        console.info('SCROLL_TEST_FINISHED')
      }
    }
    requestAnimationFrame(step)
  }, [dispatch, allPaths])
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
      const dragState = updateResizeDragState(
        resizeDragState(
          targetFrame ?? (zeroRectangle as CanvasRectangle),
          originalFrames,
          { x: 1, y: 1 },
          { x: 1, y: 1 },
          metadata.current,
          [target],
          false,
          [],
        ),
        targetStartPoint,
        { x: framesPassed % 100, y: framesPassed % 100 } as CanvasVector,
        'width',
        true,
        false,
        false,
      )
      await dispatch([CanvasActions.createDragState(dragState)]).entireUpdateFinished
      performance.mark(`resize_dispatch_finished_${framesPassed}`)
      performance.measure(
        `resize_frame_${framesPassed}`,
        `resize_step_${framesPassed}`,
        `resize_dispatch_finished_${framesPassed}`,
      )
      if (framesPassed < NumberOfIterations) {
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

function useTriggerHighlightPerformanceTest(key: 'regular' | 'all-elements'): () => void {
  const allPaths = useRefEditorState((store) => store.derived.navigatorTargets)
  const getHighlightableViews = useGetSelectableViewsForSelectMode()
  const calculateHighlightedViews = useCalculateHighlightedViews(true, getHighlightableViews)
  const trigger = React.useCallback(async () => {
    const allCapsKey = key.toLocaleUpperCase()
    if (allPaths.current.length === 0) {
      console.info(`HIGHLIGHT_${allCapsKey}_TEST_ERROR_NO_PATHS`)
      return
    }

    const targetPath = allPaths.current[0]

    const htmlElement = document.querySelector(`*[data-paths~="${EP.toString(targetPath)}"]`)
    if (htmlElement == null) {
      console.info(`HIGHLIGHT_${allCapsKey}_TEST_ERROR_NO_ELEMENT`)
      return
    }

    const elementBounds = htmlElement.getBoundingClientRect()

    let framesPassed = 0
    async function step() {
      performance.mark(`highlight_${key}_step_${framesPassed}`)

      calculateHighlightedViews(
        windowPoint({ x: elementBounds.left + 10, y: elementBounds.top + 10 }),
        key === 'all-elements',
      )

      // Clear the highlight before the next run
      calculateHighlightedViews(
        windowPoint({ x: elementBounds.left - 100, y: elementBounds.top - 100 }),
        key === 'all-elements',
      )

      performance.mark(`highlight_${key}_dispatch_finished_${framesPassed}`)
      performance.measure(
        `highlight_${key}_frame_${framesPassed}`,
        `highlight_${key}_step_${framesPassed}`,
        `highlight_${key}_dispatch_finished_${framesPassed}`,
      )

      if (framesPassed < NumberOfIterations) {
        framesPassed++
        requestAnimationFrame(step)
      } else {
        console.info(`HIGHLIGHT_${allCapsKey}_TEST_FINISHED`)
      }
    }
    requestAnimationFrame(step)
  }, [allPaths, calculateHighlightedViews, key])

  return trigger
}

export const useTriggerRegularHighlightPerformanceTest = () =>
  useTriggerHighlightPerformanceTest('regular')

export const useTriggerAllElementsHighlightPerformanceTest = () =>
  useTriggerHighlightPerformanceTest('all-elements')

export function useTriggerSelectionPerformanceTest(): () => void {
  const dispatch = useEditorState(
    (store) => store.dispatch as DebugDispatch,
    'useTriggerSelectionPerformanceTest dispatch',
  )
  const allPaths = useRefEditorState((store) => store.derived.navigatorTargets)
  const selectedViews = useRefEditorState((store) => store.editor.selectedViews)
  const trigger = React.useCallback(async () => {
    const targetPath = [...allPaths.current].sort(
      (a, b) => EP.toString(b).length - EP.toString(a).length,
    )[0]
    // Determine where the events should be fired.
    const controlsContainerElement = forceNotNull(
      'Container controls element should exist.',
      document.getElementById(CanvasControlsContainerID),
    )

    const targetElement = forceNotNull(
      'Target element should exist.',
      document.querySelector(`*[data-paths~="${EP.toString(targetPath)}"]`),
    )
    const targetBounds = targetElement.getBoundingClientRect()
    await dispatch([CanvasActions.positionCanvas(canvasPoint({ x: -1900, y: -1100 }))], 'everyone')
      .entireUpdateFinished
    if (allPaths.current.length === 0) {
      console.info('SELECT_TEST_ERROR')
      return
    }

    let framesPassed = 0
    async function step() {
      performance.mark(`select_step_${framesPassed}`)
      controlsContainerElement.dispatchEvent(
        new MouseEvent('mousedown', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          metaKey: true,
          clientX: targetBounds.left + 5,
          clientY: targetBounds.top + 5,
          buttons: 1,
        }),
      )
      function isTargetSelected(): boolean {
        return ElementPathArrayKeepDeepEquality([targetPath], selectedViews.current).areEqual
      }
      const startingTime = Date.now()
      while (!isTargetSelected() && Date.now() < startingTime + 3000) {
        await wait(50)
      }
      if (!isTargetSelected()) {
        throw new Error(`Element never ended up being selected.`)
      }
      controlsContainerElement.dispatchEvent(
        new MouseEvent('pointerup', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          metaKey: true,
          clientX: targetBounds.left + 5,
          clientY: targetBounds.top + 5,
          buttons: 1,
        }),
      )
      controlsContainerElement.dispatchEvent(
        new MouseEvent('mouseup', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          metaKey: true,
          clientX: targetBounds.left + 5,
          clientY: targetBounds.top + 5,
          buttons: 1,
        }),
      )
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

      if (framesPassed < NumberOfIterations) {
        framesPassed++
        requestAnimationFrame(step)
      } else {
        console.info('SELECT_TEST_FINISHED')
      }
    }
    requestAnimationFrame(step)
  }, [dispatch, allPaths, selectedViews])
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

      if (framesPassed < NumberOfIterations) {
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
