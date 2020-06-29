// Compose two functions f and g as f.g, or g(f())
export function compose<FResult, GResult>(
  f: () => FResult,
  g: (input: FResult) => GResult,
): () => GResult
export function compose<T1, FResult, GResult>(
  f: (t1: T1) => FResult,
  g: (input: FResult) => GResult,
): (t1: T1) => GResult
export function compose<T1, T2, FResult, GResult>(
  f: (t1: T1, t2: T2) => FResult,
  g: (input: FResult) => GResult,
): (t1: T1, t2: T2) => GResult
export function compose<T1, T2, T3, FResult, GResult>(
  f: (t1: T1, t2: T2, t3: T3) => FResult,
  g: (input: FResult) => GResult,
): (t1: T1, t2: T2, t3: T3) => GResult
export function compose<T1, T2, T3, T4, FResult, GResult>(
  f: (t1: T1, t2: T2, t3: T3, t4: T4) => FResult,
  g: (input: FResult) => GResult,
): (t1: T1, t2: T2, t3: T3, t4: T4) => GResult
export function compose<T1, T2, T3, T4, T5, FResult, GResult>(
  f: (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) => FResult,
  g: (input: FResult) => GResult,
): (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) => GResult

export function compose<FResult, GResult>(
  f: (...args: any[]) => FResult,
  g: (input: FResult) => GResult,
): (...args: any[]) => GResult {
  return (...args: any[]) => {
    return g(f(...args))
  }
}
