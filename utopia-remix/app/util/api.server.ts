import { TypedResponse, json } from "@remix-run/node";
import invariant from "tiny-invariant";
import { Env } from "../env.server";
import { Status } from "./statusCodes.server";

interface ErrorResponse {
  error: string;
}

type EmptyResponse = Record<string, never>;

export type ApiResponse<T> = TypedResponse<T | ErrorResponse | EmptyResponse>;

const responseHeaders: HeadersInit = {
  "Access-Control-Allow-Origin": Env.CORSOrigin,
  "Access-Control-Allow-Credentials": "true",
  "Access-Control-Allow-Headers": "content-type, origin",
  "Access-Control-Allow-Methods": "GET,POST,PUT,DELETE,OPTIONS",
};

export async function api<T>(
  request: Request,
  fn: () => Promise<T>,
): Promise<ApiResponse<T>> {
  if (request.method === "OPTIONS") {
    return json({}, { headers: responseHeaders });
  }

  try {
    const resp = await fn();
    return json(resp, { headers: responseHeaders });
  } catch (err) {
    const isApiError = err instanceof ApiError;
    const message = isApiError ? err.message : `${err}`;
    const status = isApiError ? err.status : 500;
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

export async function proxiedResponse(response: Response) {
  if (response.status !== Status.OK) {
    let text = await response.text();
    if (text.length === 0) {
      text = response.statusText;
    }
    throw new ApiError(text, response.status);
  }
  return response.json();
}
