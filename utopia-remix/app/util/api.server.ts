import { TypedResponse, json } from "@remix-run/node";
import invariant from "tiny-invariant";
import { ServerEnvironment } from "../env.server";
import { Status } from "./statusCodes.server";
import { Method } from "./methods.server";
import { UserDetails } from "prisma-client";
import { getUserFromSession } from "../models/session.server";
import * as cookie from "cookie";

interface ErrorResponse {
  error: string;
}

type EmptyResponse = Record<string, never>;

export type ApiResponse<T> = TypedResponse<T | ErrorResponse | EmptyResponse>;

const responseHeaders: HeadersInit = {
  "Access-Control-Allow-Origin": ServerEnvironment.CORSOrigin,
  "Access-Control-Allow-Credentials": "true",
  "Access-Control-Allow-Headers": "content-type, origin, cookie",
  "Access-Control-Allow-Methods": "GET,POST,PUT,DELETE,OPTIONS",
};

export async function handleOptions(): Promise<TypedResponse<EmptyResponse>> {
  return json({}, { headers: responseHeaders });
}

export function handle(
  request: Request,
  handlers: {
    [method in Method]?: (request: Request) => Promise<unknown>;
  },
): Promise<unknown> {
  const handler = handlers[request.method];
  if (handler == null) {
    throw new ApiError("invalid method", Status.METHOD_NOT_ALLOWED);
  }
  return handleMethod(request, handler);
}

async function handleMethod<T>(
  request: Request,
  fn: (request: Request) => Promise<T>,
): Promise<ApiResponse<T> | unknown> {
  try {
    const resp = await fn(request);
    if (resp instanceof Response) {
      return new Response(resp.body, {
        headers: {
          ...resp.headers,
          ...responseHeaders,
        },
      });
    }
    return json(resp, { headers: responseHeaders });
  } catch (err) {
    const isApiError = err instanceof ApiError;
    const message = isApiError ? err.message : `${err}`;
    const status = isApiError ? err.status : 500;

    console.error(`${request.method} ${request.url}: ${message}`);

    return json(
      { error: message },
      { headers: responseHeaders, status: status },
    );
  }
}

export class ApiError extends Error {
  status: number;
  constructor(message: string, code: number) {
    super(message);
    this.name = "InvariantError";
    this.status = code;
  }
}

export function ensure(
  condition: unknown,
  message: string,
  status: number,
): asserts condition {
  try {
    invariant(condition);
  } catch (err) {
    throw new ApiError(message, status);
  }
}

export async function proxiedResponse(response: Response): Promise<unknown> {
  if (response.status !== Status.OK) {
    let text = await response.text();
    if (text.length === 0) {
      text = response.statusText;
    }
    throw new ApiError(text, response.status);
  }
  return response.json();
}

const SESSION_COOKIE_NAME = "JSESSIONID";

export async function requireUser(request: Request): Promise<UserDetails> {
  const cookieHeader = request.headers.get("cookie") ?? "";
  const cookies = cookie.parse(cookieHeader);
  const sessionId = cookies[SESSION_COOKIE_NAME] ?? null;
  ensure(sessionId != null, "missing session cookie", Status.UNAUTHORIZED);
  const user = await getUserFromSession({ key: sessionId });
  ensure(user != null, "user not found", Status.UNAUTHORIZED);
  return user;
}
