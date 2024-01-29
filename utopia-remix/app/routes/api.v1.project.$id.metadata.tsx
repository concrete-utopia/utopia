import { LoaderFunctionArgs } from "@remix-run/node";
import { api, ensure } from "../util/api.server";
import { proxy } from "../util/proxy.server";
import { Status } from "../util/statusCodes.server";

export async function loader(args: LoaderFunctionArgs) {
  return api(args.request, async () => {
    const { id } = args.params;
    ensure(id != null, "id is null", Status.BAD_REQUEST);

    return proxy(args.request, `/project/${id}/metadata`);
  });
}
