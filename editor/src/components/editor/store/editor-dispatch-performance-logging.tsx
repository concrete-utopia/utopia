import { canMeasurePerformance } from '../../../core/performance/performance-utils'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import type { EditorAction } from '../action-types'
import { simpleStringifyActions } from '../actions/action-utils'

export function createPerformanceMeasure() {
  const MeasureSelectorsEnabled = isFeatureEnabled('Debug – Measure Selectors')
  const PerformanceMarks = canMeasurePerformance()

  let stringifiedActions = ''

  let tasks: {
    [name: string]: { timingInfo: string; startMarkName: string; endMarkName: string }
  } = {}
  return {
    logActions(actions: ReadonlyArray<EditorAction>): void {
      stringifiedActions = simpleStringifyActions(actions)
    },
    taskTime<T>(name: string, task: () => T): T {
      const startMarkName = `${name}-start`
      const endMarkName = `${name}-end`

      const before = MeasureSelectorsEnabled ? performance.now() : 0
      if (PerformanceMarks) {
        performance.mark(startMarkName)
      }

      // run the actual task
      const result = task()

      const after = MeasureSelectorsEnabled ? performance.now() : 0
      if (PerformanceMarks) {
        performance.mark(endMarkName)
      }

      tasks[name] = {
        timingInfo: `${after - before}ms`,
        startMarkName: startMarkName,
        endMarkName: endMarkName,
      }
      return result
    },
    printMeasurements() {
      if (MeasureSelectorsEnabled) {
        console.info('------------------')
        console.info(`dispatched actions: ${stringifiedActions}`)
        console.info(
          Object.entries(tasks)
            .map(([task, { timingInfo }]) => `${task}: ${timingInfo}`)
            .join('\n'),
        )
      }
      if (PerformanceMarks) {
        Object.entries(tasks).forEach(([task, { startMarkName, endMarkName }]) => {
          performance.measure(task, startMarkName, endMarkName)
        })
      }
    },
  }
}
