import type { Fetcher } from '@remix-run/react'
import React from 'react'

export interface FetcherLike {
  state: Fetcher['state']
  data: unknown
}

export function useFetcherData<T>(
  fetcher: FetcherLike,
  dataValidator: (u: unknown) => u is T,
  callback: (data: T) => void,
) {
  React.useEffect(() => {
    if (fetcher.state === 'idle' && fetcher.data != null) {
      if (dataValidator(fetcher.data)) {
        callback(fetcher.data)
      }
    }
  }, [fetcher.state, fetcher.data, dataValidator, callback])
}

export function useFetcherDataUnkown(fetcher: FetcherLike, callback: (data: unknown) => void) {
  return useFetcherData(fetcher, (u: unknown): u is unknown => true, callback)
}
