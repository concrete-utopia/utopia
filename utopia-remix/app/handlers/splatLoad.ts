import path from 'path'
import { ensure } from '../util/api.server'
import { proxy } from '../util/proxy.server'
import { Status } from '../util/statusCodes'

export const allowedAssetExtensions = [
  // images
  '.bmp',
  '.gif',
  '.ico',
  '.jpeg',
  '.jpg',
  '.png',
  '.svg',
  '.webm',
  '.webp',
  // fonts
  '.eot',
  '.ttf',
  '.woff',
  '.woff2',
  // other
  '.css',
  '.json',
  '.txt',
  '.xml',
]

export async function handleSplatLoad(req: Request) {
  const url = new URL(req.url)

  const extension = path.extname(url.pathname).toLowerCase()
  ensure(allowedAssetExtensions.includes(extension), 'invalid extension', Status.BAD_REQUEST)

  return proxy(req, { rawOutput: true })
}
