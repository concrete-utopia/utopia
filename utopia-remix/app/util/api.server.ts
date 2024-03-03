import { TypedResponse, json, redirect } from '@remix-run/node'
import { Params } from '@remix-run/react'
import * as cookie from 'cookie'
import { UserDetails } from 'prisma-client'
import { PrismaClientKnownRequestError } from 'prisma-client/runtime/library.js'
import invariant from 'tiny-invariant'
import { ServerEnvironment } from '../env.server'
import { getUserFromSession } from '../models/session.server'
import { ApiError } from './errors'
import { Method } from './methods.server'
import { Status } from './statusCodes'

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
  'Cache-control': 'no-cache',
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

export function ensure(condition: unknown, message: string, status: number): asserts condition {
  try {
    invariant(condition)
  } catch (error) {
    throw new ApiError(message, status)
  }
}

export async function proxiedResponse(response: Response): Promise<unknown> {
  if (response.status !== Status.OK) {
    let text = await response.text()
    if (text.length === 0) {
      text = response.statusText
    }
    throw new ApiError(text, response.status)
  }
  return response.json()
}

export const SESSION_COOKIE_NAME = 'JSESSIONID'

export async function requireUser(
  request: Request,
  options?: { redirect?: string },
): Promise<UserDetails> {
  const cookieHeader = request.headers.get('cookie') ?? ''
  const cookies = cookie.parse(cookieHeader)
  const sessionId = cookies[SESSION_COOKIE_NAME] ?? null
  try {
    ensure(sessionId != null, 'missing session cookie', Status.UNAUTHORIZED)
    const user = await getUserFromSession({ key: sessionId })
    ensure(user != null, 'user not found', Status.UNAUTHORIZED)
    return user
  } catch (error) {
    if (error instanceof ApiError && error.status === Status.UNAUTHORIZED) {
      if (options?.redirect != null) {
        throw redirect(options.redirect)
      }
    }
    throw error
  }
}

export async function getUser(request: Request): Promise<UserDetails | null> {
  const cookieHeader = request.headers.get('cookie') ?? ''
  const cookies = cookie.parse(cookieHeader)
  const sessionId = cookies[SESSION_COOKIE_NAME] ?? null
  if (sessionId == null) {
    return null
  }
  return getUserFromSession({ key: sessionId })
}
