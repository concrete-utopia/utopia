import type { AppLoadContext } from '@remix-run/server-runtime'
import type { DataRouteObject, RouteObject } from 'react-router'

// adopted from https://github.com/remix-run/remix/blob/4a8f558d16b8739d7f70903958ac5792e050f3e9/packages/remix-testing/create-remix-stub.tsx#L55
// replaced the types with the vanilla types from react-router
export function patchRoutesWithContext(
  routes: Array<RouteObject | DataRouteObject>,
  getLoadContext?: (request: Request) => Promise<AppLoadContext> | AppLoadContext, // the async getLoadContext has been lifted from the server adapter https://remix.run/docs/en/main/route/loader#context
): (RouteObject | DataRouteObject)[] {
  if (getLoadContext == null) {
    // no context to patch with
    return routes
  }

  return routes.map((route) => {
    if (route.loader) {
      let loader = route.loader
      route.loader = async (args) => {
        const context = await getLoadContext(args.request)
        return loader({ ...args, context: context })
      }
    }

    if (route.action) {
      let action = route.action
      route.action = async (args) => {
        const context = await getLoadContext(args.request)
        return action({ ...args, context: context })
      }
    }

    if (route.children) {
      return {
        ...route,
        children: patchRoutesWithContext(route.children, getLoadContext),
      }
    }

    return route as RouteObject | DataRouteObject
  }) as (RouteObject | DataRouteObject)[]
}
