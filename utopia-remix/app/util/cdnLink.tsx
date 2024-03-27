import urlJoin from 'url-join'
import { useAppStore } from '../stores/appStore'
import React from 'react'

export function cdnLink(host: string | null, relativePath: string): string {
  return urlJoin(host ?? '', relativePath)
}

export function useCDNLink() {
  const env = useAppStore((store) => store.env)
  return React.useCallback(
    (relativePath: string) => {
      return cdnLink(env?.UTOPIA_CDN_URL ?? null, relativePath)
    },
    [env],
  )
}
