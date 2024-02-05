import { ActionFunctionArgs, LoaderFunctionArgs } from "@remix-run/node";
import { handle, handleOptions } from "../util/api.server";
import { proxy } from "../util/proxy.server";

export async function loader(args: LoaderFunctionArgs) {
  return handle(args.request, {
    OPTIONS: handleOptions,
  });
}

export async function action(args: ActionFunctionArgs) {
  return handle(args.request, {
    POST: (req) => proxy(req, { rawOutput: true }),
  });
}
