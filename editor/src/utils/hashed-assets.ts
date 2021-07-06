import { HEADERS, MODE } from '../common/server'
import { BASE_URL, STATIC_BASE_URL } from '../common/env-vars'
import { isBrowserEnvironment } from '../core/shared/utils'
import { cachedPromise } from '../core/shared/promise-utils'
const urljoin = require('url-join')

const HASHED_ASSETS_ENDPOINT = BASE_URL + 'hashed-assets.json'

export let HASHED_ASSETS_MAPPINGS: { [key: string]: string } = {}

export function triggerHashedAssetsUpdate(): Promise<void> {
  return cachedPromise('triggerHashedAssetsUpdate', async () => {
    if (isBrowserEnvironment) {
      const response = await fetch(HASHED_ASSETS_ENDPOINT, {
        method: 'GET',
        credentials: 'include',
        headers: HEADERS,
        mode: MODE,
      })
      const mappingsJSON = await response.json()
      HASHED_ASSETS_MAPPINGS = mappingsJSON
    } else {
      return Promise.resolve()
    }
  })
}

function getPossiblyHashedURLInner(url: string): string {
  if (url in HASHED_ASSETS_MAPPINGS) {
    return HASHED_ASSETS_MAPPINGS[url]
  } else {
    return url
  }
}

export function getPossiblyHashedURL(url: string): string {
  const relativeURL = getPossiblyHashedURLInner(url)
  return urljoin(STATIC_BASE_URL, relativeURL)
}

const prioritisedAssets = [
  '/editor/icons/light/semantic/hamburgermenu-black-24x24@2x.png',
  '/editor/icons/light/semantic/closedcube-black-24x24@2x.png',
  '/editor/icons/light/semantic/closedcube-white-24x24@2x.png',
  '/editor/icons/light/filetype/ui-darkgray-18x18@2x.png',
  '/editor/icons/light/filetype/js-darkgray-18x18@2x.png',
  '/editor/icons/light/semantic/externallink-large-black-24x24@2x.png',
  '/editor/icons/light/semantic/cross-small-gray-16x16@2x.png',
]

export function preloadPrioritizedAssets() {
  if (isBrowserEnvironment) {
    prioritisedAssets.forEach((asset) => {
      const url = getPossiblyHashedURL(asset)
      fetch(url)
    })
  }
}
