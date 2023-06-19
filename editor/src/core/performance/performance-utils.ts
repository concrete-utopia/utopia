import { PRODUCTION_CONFIG } from '../../common/env-vars'
import { roundToNearestWhole } from '../shared/math-utils'

export function timeBlockIfNotInProd<T>(fn: () => T): { result: T; timeTaken: number } {
  if (PRODUCTION_CONFIG) {
    return {
      result: fn(),
      timeTaken: 0,
    }
  }

  const startTime = performance.now()
  const result = fn()
  const timeTaken = roundToNearestWhole(performance.now() - startTime)

  return {
    result: result,
    timeTaken: timeTaken,
  }
}
