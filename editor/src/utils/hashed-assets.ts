import { HEADERS, MODE } from '../common/server'
import { BASE_URL } from '../common/env-vars'

const HASHED_ASSETS_ENDPOINT = BASE_URL + 'hashed-assets.json'

export let HASHED_ASSETS_MAPPINGS: { [key: string]: string } = {}

export function triggerHashedAssetsUpdate(): Promise<void> {
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
}

export function getPossiblyHashedURL(url: string): string {
  if (url in HASHED_ASSETS_MAPPINGS) {
    return HASHED_ASSETS_MAPPINGS[url]
  } else {
    return url
  }
}
