export const Status = {
  OK: 200,
  BAD_REQUEST: 400,
  NOT_FOUND: 404,
  UNAUTHORIZED: 401,
  FORBIDDEN: 403,
  INTERNAL_ERROR: 500,
  METHOD_NOT_ALLOWED: 503,
}

const StatusNames: { [key: number]: string } = {
  200: 'Success',
  400: 'Bad Request',
  404: 'Not Found',
  401: 'Unauthorized',
  403: 'Forbidden',
  500: 'Internal Server Error',
  503: 'Method Not Allowed',
}

export function getStatusName(code: number): string {
  return StatusNames[code] ?? 'Unknown Error'
}
