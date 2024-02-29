import { getStatusName } from './statusCodes'

export class ApiError extends Error {
  status: number
  statusText: string
  data: string // mimicing the Remix errors
  constructor(message: string, code: number) {
    super(message)
    this.statusText = getStatusName(code)
    this.status = code
    this.data = message
  }
}

export interface ErrorWithStatus {
  status: number
  statusText: string
  data: string
}

export function isErrorWithStatus(u: unknown): u is ErrorWithStatus {
  const maybe = u as ErrorWithStatus
  return (
    typeof u === 'object' &&
    u != null &&
    maybe.status != null &&
    maybe.statusText != null &&
    maybe.data != null
  )
}

export function isLikeApiError(u: unknown): u is ApiError {
  const maybe = u as ApiError
  return typeof u === 'object' && u != null && maybe.status != null && maybe.message != null
}
