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
      framesPassed++
      await dispatch([CanvasActions.scrollCanvas(canvasPoint({ x: -5, y: -1 }))])
        .entireUpdateFinished
      if (framesPassed < 100) {
        requestAnimationFrame(step)
      }
    }
    requestAnimationFrame(step)
  }, [dispatch])
  return trigger
}
