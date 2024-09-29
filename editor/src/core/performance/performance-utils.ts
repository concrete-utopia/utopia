import { PERFORMANCE_MARKS_ALLOWED } from '../../common/env-vars'
import { isFeatureEnabled } from '../../utils/feature-switches'

export function timeFunction(fnName: string, fn: () => any, iterations: number = 100) {
  const start = Date.now()
  for (var i = 0; i < iterations; i++) {
    fn()
  }
  const end = Date.now()
  const timeTaken = (end - start) / iterations
  // eslint-disable-next-line no-console
  console.log(`${fnName} took ${timeTaken}ms`)
}

export function canMeasurePerformance(): boolean {
  return (
    (isFeatureEnabled('Debug – Performance Marks (Fast)') ||
      isFeatureEnabled('Debug – Performance Marks (Slow)')) &&
    PERFORMANCE_MARKS_ALLOWED
  )
}

export function startPerformanceMeasure(
  measureName: string,
  { uniqueId }: { uniqueId?: boolean } = {},
): { id: string; endMeasure: () => number } {
  const id = uniqueId ? `${measureName}-${Math.random()}` : measureName
  if (PERFORMANCE_MARKS_ALLOWED) {
    performance.mark(`${id}-start`)
  }
  return {
    id: id,
    endMeasure: () => endPerformanceMeasure(id),
  }
}

export function endPerformanceMeasure(id: string): number {
  if (PERFORMANCE_MARKS_ALLOWED) {
    performance.mark(`${id}-end`)
    performance.measure(`${id}-duration`, `${id}-start`, `${id}-end`)
    const measurements = performance.getEntriesByName(`${id}-duration`)
    const latestMeasurement = measurements[measurements.length - 1]
    if (latestMeasurement != null) {
      return latestMeasurement.duration
    }
  }
  return 0
}
