import { HEADERS, MODE } from '../common/server'
import { BASE_URL } from '../common/env-vars'
import { isBrowserEnvironment } from '../core/shared/utils'

const HASHED_ASSETS_ENDPOINT = BASE_URL + 'hashed-assets.json'

export let HASHED_ASSETS_MAPPINGS: { [key: string]: string } = {}

export function triggerHashedAssetsUpdate(): Promise<void> {
  if (isBrowserEnvironment) {
    return fetch(HASHED_ASSETS_ENDPOINT, {
      method: 'GET',
      credentials: 'include',
      headers: HEADERS,
      mode: MODE,
    }).then((response) => {
      response.json().then((mappingsJSON) => {
        HASHED_ASSETS_MAPPINGS = mappingsJSON
      })
    })
  } else {
    return Promise.resolve()
  }
}

export function getPossiblyHashedURL(url: string): string {
  if (url in HASHED_ASSETS_MAPPINGS) {
    return HASHED_ASSETS_MAPPINGS[url]
  } else {
    return url
  }
}
