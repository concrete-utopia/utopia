import * as React from 'react'
import * as ReactDOM from 'react-dom'
import CanvasActions from '../../components/canvas/canvas-actions'
import { DebugDispatch } from '../../components/editor/action-types'
import { useEditorState } from '../../components/editor/store/store-hook'
import { canvasPoint } from '../shared/math-utils'

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
