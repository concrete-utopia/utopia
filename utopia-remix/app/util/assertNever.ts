import { ApiError } from './errors'

export function assertNever(n: never): never {
  throw new Error(`Expected \`never\`, got ${JSON.stringify(n)}`)
}

export function assertNeverApiError(n: never, message: string, code: number): never {
  throw new ApiError(message, code)
}
