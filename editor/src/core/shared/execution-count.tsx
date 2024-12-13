export type LimitExecutionCountReset = () => void

export interface LimitExecutionCountOptions {
  maximumExecutionCount: number
  addToResetArray: Array<LimitExecutionCountReset> | null
}

export interface LimitExecutionCountState {
  executionCount: number
}

export interface LimitExecutionCountResult<Args extends ReadonlyArray<unknown>, Result> {
  wrappedFunction: (...args: Args) => Result
  resetCount: LimitExecutionCountReset
}

const defaultOptions: LimitExecutionCountOptions = {
  maximumExecutionCount: 1,
  addToResetArray: null,
}

export function limitExecutionCount<Args extends ReadonlyArray<unknown>, Result>(
  options: Partial<LimitExecutionCountOptions>,
  fn: (...args: Args) => Result,
): LimitExecutionCountResult<Args, Result> {
  // Setup the default and specified options and state.
  const fullySpecifiedOptions: LimitExecutionCountOptions = {
    ...defaultOptions,
    ...options,
  }
  let state: LimitExecutionCountState = {
    executionCount: 0,
  }

  // Wrap the function supplied by the user.
  const wrappedFunction = (...args: Args): Result => {
    if (state.executionCount >= fullySpecifiedOptions.maximumExecutionCount) {
      throw new Error(
        `Function exceeded maximum execution count of ${fullySpecifiedOptions.maximumExecutionCount}.`,
      )
    }

    state.executionCount += 1
    return fn(...args)
  }

  // Create the function for resetting the count.
  const resetCount = (): void => {
    state.executionCount = 0
  }

  // Add into the reset array.
  if (options.addToResetArray != null) {
    options.addToResetArray.push(resetCount)
  }

  // Build the result.
  return {
    wrappedFunction: wrappedFunction,
    resetCount: resetCount,
  }
}
