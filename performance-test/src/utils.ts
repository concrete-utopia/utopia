export function timeLimitPromise<T>(
  promise: Promise<T>,
  limitms: number,
  message: string,
): Promise<T> {
  const timeoutPromise: Promise<any> = new Promise((resolve, reject) => {
    const timeoutID = setTimeout(() => {
      clearTimeout(timeoutID)
      reject(message)
    }, limitms)
  })
  return Promise.race([promise, timeoutPromise])
}
