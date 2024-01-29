import { LoaderFunctionArgs } from "@remix-run/node";
import { handler } from "../util/api.server";
import { proxy } from "../util/proxy.server";

export async function loader(args: LoaderFunctionArgs) {
  return handler(["OPTIONS", "GET"], args.request, proxy);
}
