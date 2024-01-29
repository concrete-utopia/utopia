import { ActionFunctionArgs, LoaderFunctionArgs } from "@remix-run/node";
import { handler } from "../util/api.server";
import { proxy } from "../util/proxy.server";

export async function loader(args: LoaderFunctionArgs) {
  return handler(["OPTIONS"], args.request, proxy);
}

export async function action(args: ActionFunctionArgs) {
  return handler(["PUT"], args.request, proxy);
}
