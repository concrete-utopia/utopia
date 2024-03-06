import type { Either } from '../core/shared/either'
import { left, right } from '../core/shared/either'

export function resultOrError<T>(toRun: () => T): Either<unknown, T> {
  try {
    return right(toRun())
  } catch (error) {
    return left(error)
  }
}
