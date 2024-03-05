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

const defaultResponseHeaders = new Headers({
  'Access-Control-Allow-Origin': ServerEnvironment.CORSOrigin,
  'Access-Control-Allow-Credentials': 'true',
  'Access-Control-Allow-Headers': 'content-type, origin, cookie',
  'Access-Control-Allow-Methods': 'GET,POST,PUT,DELETE,OPTIONS',
  'Cache-control': 'no-cache',
})

export async function handleOptions(): Promise<TypedResponse<EmptyResponse>> {
  return json({}, { headers: defaultResponseHeaders })
}

interface HandleRequest {
  request: Request
  params: Params<string>
}

/**
 * A Chain is an ordered sequence of handlers that are meant to run one after the other.
 * If a handler throws an error during the chain, the chain is stopped.
 * The final result of the last handler is returned to the client.
 * Handlers can share state using the specified context type.
 */
type Chain<T> = {
  handlers: Handler<T>[]
  context: T | null
}

/**
 * Create a new chain for the given (ordered!) handlers and context, in its initial value.
 */
export function chain<T = unknown>(handlers: Handler<T>[], contextInitialValue?: T): Chain<T> {
  return {
    handlers: handlers,
    context: contextInitialValue ?? null,
  }
}

type Handler<T> = (request: Request, params: Params<string>, context: T | null) => Promise<unknown>

/**
 * Process the Chain against the given request, returning the last result.
 */
async function handleChain<T>(
  request: Request,
  params: Params<string>,
  chain: Chain<T>,
): Promise<unknown> {
  try {
    let result: unknown = null
    for (const handler of chain.handlers) {
      result = await handleMethod(chain.context, request, params, handler)
    }
    return result
  } catch (err) {
    const { message, status, name } = getErrorData(err)

    console.error(`${request.method} ${request.url}: ${message}`)

    return json(
      { error: name, status: status, message: message },
      { headers: defaultResponseHeaders, status: status },
    )
  }
}

export function handle<T>(
  { request, params }: HandleRequest,
  handlers: {
    [method in Method]?: Chain<T>
  },
): Promise<unknown> {
  const chain = handlers[request.method as Method]
  if (chain == null) {
    throw new ApiError('invalid method', Status.METHOD_NOT_ALLOWED)
  }
  return handleChain(request, params, chain)
}

async function handleMethod<T>(
  context: T | null,
  request: Request,
  params: Params<string>,
  fn: Handler<T>,
): Promise<ApiResponse<T> | unknown> {
  const resp = await fn(request, params, context)
  if (resp instanceof Response) {
    const mergedHeaders = new Headers(defaultResponseHeaders)
    resp.headers.forEach((value, key) => {
      mergedHeaders.set(key, value)
    })
    return new Response(resp.body, {
      status: resp.status,
      headers: mergedHeaders,
    })
  }
  return json(resp, { headers: defaultResponseHeaders })
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
