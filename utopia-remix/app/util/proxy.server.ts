import urljoin from "url-join";
import { Env } from "../env.server";
import { proxiedResponse } from "./api.server";

const BASE_URL = Env.BackendURL;

function buildProxyUrl(url: URL, path: string | null): string {
  const { pathname, search } = url;

  if (path != null) {
    return urljoin(BASE_URL, path);
  }

  return urljoin(BASE_URL, `${pathname}${search}`);
}

export async function proxy(
  req: Request,
  options?: { rawOutput?: boolean; path?: string },
) {
  const url = buildProxyUrl(new URL(req.url), options?.path ?? null);
  const response = await fetch(url, {
    credentials: "include",
    method: req.method,
    body: req.body,
    headers: req.headers,
  });
  if (options?.rawOutput) {
    return response;
  }
  return proxiedResponse(response);
}
