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
