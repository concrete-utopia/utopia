import { LoaderFunctionArgs } from "@remix-run/node";
import { proxy } from "../util/proxy.server";
import { handler } from "../util/api.server";

export async function loader(args: LoaderFunctionArgs) {
  return handler(["OPTIONS", "GET"], args.request, proxy);
}
