import urljoin from "url-join";
import { Env } from "../env.server";
import { proxiedResponse } from "./api.server";

const BASE_URL = Env.BackendURL;

export async function proxy(req: Request, path: string) {
  const response = await fetch(urljoin(BASE_URL, path), {
    credentials: "include",
    method: req.method,
    body: req.body,
  });
  if (response instanceof Response) {
    return proxiedResponse(response);
  }
  return response;
}
