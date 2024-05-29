import urlJoin from 'url-join'
import { BrowserEnvironment } from '../env.server'
import { allowedAssetExtensions } from '../handlers/splatLoad'
import { canAccessProject } from '../handlers/validators'
import { UserProjectPermission } from '../types'

export async function getProxyAssetPath(req: Request): Promise<string | null> {
  const url = new URL(req.url)

  // the extension must be allowed
  if (!allowedAssetExtensions.some((extension) => url.pathname.endsWith(extension))) {
    return null
  }

  // the request referer must contain the project id
  const maybeProjectId = getProjectIdFromReferer(req)
  if (maybeProjectId == null) {
    return null
  }

  // validate access to the project
  const { ok } = await canAccessProject({
    projectId: maybeProjectId,
    permission: UserProjectPermission.CAN_VIEW_PROJECT,
    request: req,
  })
  if (!ok) {
    return null
  }

  return '/' + urlJoin('p', maybeProjectId, url.pathname)
}

export function getProjectIdFromReferer(req: Request): string | null {
  const referer = req.headers.get('referer')
  if (referer == null) {
    return null
  }

  const refererURL = new URL(referer)
  const isMaybeProjectReferer =
    refererURL.origin === BrowserEnvironment.EDITOR_URL &&
    (refererURL.pathname.startsWith('/p/') || refererURL.pathname.startsWith('/project/'))
  if (!isMaybeProjectReferer) {
    return null
  }

  const maybeProjectId = refererURL.pathname
    .replace(/^\/p(roject)?\//, '')
    .split('/')[0]
    .split('-')[0]
  if (maybeProjectId.length === 0) {
    return null
  }

  return maybeProjectId
}
