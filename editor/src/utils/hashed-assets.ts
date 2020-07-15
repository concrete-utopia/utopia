import { HEADERS, MODE } from '../common/server'
import { BASE_URL, STATIC_BASE_URL } from '../common/env-vars'
import { isBrowserEnvironment } from '../core/shared/utils'
import { cachedPromise } from '../core/shared/promise-utils'
import urljoin = require('url-join')

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
      response.json().then((mappingsJSON) => {
        HASHED_ASSETS_MAPPINGS = mappingsJSON
      })
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
