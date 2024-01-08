import { assertNever } from '../core/shared/utils'

const RETRY_TIMEOUT = 1000 // ms
const MAX_RETRIES = 3 // how many retries per image before giving up

type ImageCacheEntry =
  | { status: 'not-loaded'; retry: number }
  | { status: 'loading' }
  | { status: 'loaded'; data: ArrayBuffer }

const imageCache: {
  [key: string]: ImageCacheEntry
} = {}

export async function loadAvatarImage(url: string, onLoaded: (data: ArrayBuffer) => void) {
  const cacheEntry = imageCache[url] ?? {
    status: 'not-loaded',
    retry: 0,
  }

  switch (cacheEntry.status) {
    case 'loaded':
      // the data is already there, so run the callback
      onLoaded(cacheEntry.data)
      break

    case 'loading':
      // the data is being loaded, try again in a bit
      setTimeout(() => {
        void loadAvatarImage(url, onLoaded)
      }, RETRY_TIMEOUT)
      break

    case 'not-loaded':
      // give up after too many retries
      if (cacheEntry.retry > MAX_RETRIES) {
        break
      }

      // set the image to loading, just once
      imageCache[url] = {
        status: 'loading',
      }

      // fetch the image data
      try {
        const imageData = await fetch(url)
        const data = await imageData.arrayBuffer()

        // all is fine, store it and run the callback
        imageCache[url] = {
          status: 'loaded',
          data: data,
        }
        onLoaded(data)
      } catch (error) {
        console.warn('cannot get picture', url, error)

        // try again
        imageCache[url] = {
          status: 'not-loaded',
          retry: cacheEntry.retry + 1,
        }
        setTimeout(() => {
          void loadAvatarImage(url, onLoaded)
        }, RETRY_TIMEOUT)
      }
      break
    default:
      assertNever(cacheEntry)
  }
}
