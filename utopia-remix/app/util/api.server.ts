import { TypedResponse, json } from '@remix-run/node'
import invariant from 'tiny-invariant'
import { ServerEnvironment } from '../env.server'
import { Status, getStatusName } from './statusCodes.server'
import { Method } from './methods.server'
import { UserDetails } from 'prisma-client'
import { getUserFromSession } from '../models/session.server'
import * as cookie from 'cookie'
import { Params } from '@remix-run/react'
import { PrismaClientKnownRequestError } from 'prisma-client/runtime/library.js'
import { AxiosResponse } from 'axios'

interface ErrorResponse {
  error: string
}

type EmptyResponse = Record<string, never>

export type ApiResponse<T> = TypedResponse<T | ErrorResponse | EmptyResponse>

const responseHeaders: HeadersInit = {
  'Access-Control-Allow-Origin': ServerEnvironment.CORSOrigin,
  'Access-Control-Allow-Credentials': 'true',
  'Access-Control-Allow-Headers': 'content-type, origin, cookie',
  'Access-Control-Allow-Methods': 'GET,POST,PUT,DELETE,OPTIONS',
}

export async function handleOptions(): Promise<TypedResponse<EmptyResponse>> {
  return json({}, { headers: responseHeaders })
}

interface HandleRequest {
  request: Request
  params: Params<string>
}

export function handle(
  { request, params }: HandleRequest,
  handlers: {
    [method in Method]?: (request: Request, params: Params<string>) => Promise<unknown>
  },
): Promise<unknown> {
  const handler = handlers[request.method as Method]
  if (handler == null) {
    throw new ApiError('invalid method', Status.METHOD_NOT_ALLOWED)
  }
  return handleMethod(request, params, handler)
}

async function handleMethod<T>(
  request: Request,
  params: Params<string>,
  fn: (request: Request, params: Params<string>) => Promise<T>,
): Promise<ApiResponse<T> | unknown> {
  try {
    const resp = await fn(request, params)
    if (resp instanceof Response) {
      return new Response(resp.body, {
        headers: {
          ...resp.headers,
          ...responseHeaders,
        },
      })
    }
    return json(resp, { headers: responseHeaders })
  } catch (err) {
    const { message, status, name } = getErrorData(err)

    console.error(`${request.method} ${request.url}: ${message}`)

    return json(
      { error: name, status: status, message: message },
      { headers: responseHeaders, status: status },
    )
  }
}

function getErrorData(err: unknown): { message: string; status: number; name: string } {
  if (err instanceof ApiError) {
    return {
      message: err.message,
      status: err.status,
      name: err.name,
    }
  } else if (err instanceof PrismaClientKnownRequestError) {
    return {
      message: (err.meta?.cause as string) ?? err.message,
      status: Status.INTERNAL_ERROR,
      name: (err.meta?.modelName as string) ?? err.name,
    }
  } else {
    return {
      message: `${err}`,
      status: Status.INTERNAL_ERROR,
      name: 'Error',
    }
  }
}

export class ApiError extends Error {
  status: number
  constructor(message: string, code: number) {
    super(message)
    this.name = getStatusName(code)
    this.status = code
  }
}

export function ensure(condition: unknown, message: string, status: number): asserts condition {
  try {
    invariant(condition)
  } catch (err) {
    throw new ApiError(message, status)
  }
}

export async function proxiedResponse(response: AxiosResponse): Promise<unknown> {
  if (response.status !== Status.OK) {
    let text = await response.data
    if (text.length === 0) {
      text = response.statusText
    }
    throw new ApiError(text, response.status)
  }
  return response.data
}

export const SESSION_COOKIE_NAME = 'JSESSIONID'

export async function requireUser(request: Request): Promise<UserDetails> {
  const cookieHeader = request.headers.get('cookie') ?? ''
  const cookies = cookie.parse(cookieHeader)
  const sessionId = cookies[SESSION_COOKIE_NAME] ?? null
  ensure(sessionId != null, 'missing session cookie', Status.UNAUTHORIZED)
  const user = await getUserFromSession({ key: sessionId })
  ensure(user != null, 'user not found', Status.UNAUTHORIZED)
  return user
}
