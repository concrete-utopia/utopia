import { HEADERS, MODE } from '../common/server'
import { BASE_URL, STATIC_BASE_URL } from '../common/env-vars'
import { isBrowserEnvironment } from '../core/shared/utils'
import { cachedPromise } from '../core/shared/promise-utils'
import { appendToPath } from './path-utils'

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
    // Provably exists because of the `in` check.
    return HASHED_ASSETS_MAPPINGS[url]!
  } else {
    return url
  }
}

export function getPossiblyHashedURL(url: string): string {
  const relativeURL = getPossiblyHashedURLInner(url)
  return appendToPath(STATIC_BASE_URL, relativeURL)
}

// prioritise the toolbar assets for now, so first click shows them immediately
const prioritisedAssets = [
  '/editor/icons/tools/comment-white-18x18@2x.png',
  // next line is for the pointer when it's not selected (by default it is)
  '/editor/icons/tools/pointer-black-18x18@2x.png',
  '/editor/icons/tools/play-white-18x18@2x.png',
  '/editor/icons/tools/text-white-18x18@2x.png',
  '/editor/icons/tools/panels-white-18x18@2x.png',
]

export function preloadPrioritizedAssets() {
  if (isBrowserEnvironment) {
    prioritisedAssets.forEach((asset) => {
      const url = getPossiblyHashedURL(asset)
      void fetch(url)
    })
  }
}
