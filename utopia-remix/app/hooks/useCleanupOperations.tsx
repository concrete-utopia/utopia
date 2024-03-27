import type { Fetcher } from '@remix-run/react'
import { useFetchers } from '@remix-run/react'
import { useProjectsStore } from '../stores/projectsStore'
import React from 'react'
import { isLikeApiError } from '../util/errors'
import { operationFetcherKeyPrefix } from './useFetcherWithOperation'

interface LoadingFetcher {
  key: string
  data: unknown
}

/**
 * This hook will react to fetchers changes, so that the ones tied to ongoing Operations,
 * when transitioning from loading to idle (and hence being cleared) will have their corresponding
 * Operations either removed or marked as errored, depending on the result of the fetcher data
 * received back.
 */
export function useCleanupOperations() {
  const [idleFetchers, setIdleFetchers] = useIdleFetchers()
  const processIdleFetchers = useProcessIdleFetchers()

  // when the idle fetchers change, process them
  React.useEffect(() => {
    if (idleFetchers.length > 0) {
      setIdleFetchers([])
      processIdleFetchers(idleFetchers)
    }
  }, [idleFetchers, setIdleFetchers, processIdleFetchers])
}

/**
 * React to fetchers state changes and return the list of idle fetchers.
 */
function useIdleFetchers() {
  const fetchers = useFetchers()

  // a list of fetchers that were seen as loading
  const [loadingFetchers, setLoadingFetchers] = React.useState<LoadingFetcher[]>([])

  // a list of fetchers that transitioned from loading to idle
  const idleFetchersState = React.useState<LoadingFetcher[]>([])
  const [, setIdleFetchers] = idleFetchersState

  // react to fetcher state changes and look for loading/idle ones
  React.useEffect(() => {
    const currentLoadingFetchers = fetchers.filter(isLoadingOperationFetcher)
    const newIdleFetchers = loadingFetchers.filter(
      (fetcher) => !currentLoadingFetchers.some((current) => current.key === fetcher.key),
    )

    const someFetchersWentIdle = newIdleFetchers.length > 0
    if (someFetchersWentIdle) {
      setIdleFetchers(newIdleFetchers) // important! store the idle fetchers so they can be processed separately
    }

    const loadingFetchersHaveChanged =
      loadingFetchers.length !== currentLoadingFetchers.length ||
      loadingFetchers.some(
        (fetcher) => !currentLoadingFetchers.some((current) => current.key === fetcher.key),
      )
    if (someFetchersWentIdle || loadingFetchersHaveChanged) {
      setLoadingFetchers([
        ...currentLoadingFetchers,
        ...loadingFetchers.filter((f) => !newIdleFetchers.some((idle) => f.key === idle.key)),
      ])
    }
  }, [fetchers, setIdleFetchers, loadingFetchers])

  return idleFetchersState
}

/**
 * Process idle fetchers by either removing them when successful or marking them
 * as errored otherwise.
 */
function useProcessIdleFetchers() {
  const removeOperation = useProjectsStore((store) => store.removeOperation)
  const updateOperation = useProjectsStore((store) => store.updateOperation)

  return React.useCallback(
    (targets: LoadingFetcher[]) => {
      // clean them up
      for (const fetcher of targets) {
        if (isLikeApiError(fetcher.data)) {
          updateOperation(fetcher.key, { errored: true })
        } else {
          removeOperation(fetcher.key)
        }
      }
    },
    [updateOperation, removeOperation],
  )
}

type FetcherWithKey = Fetcher & { key: string }

function isLoadingOperationFetcher(fetcher: FetcherWithKey): boolean {
  return (
    // it's an operation fetcher…
    fetcher.key.startsWith(operationFetcherKeyPrefix) &&
    // …and it has data…
    fetcher.data != null &&
    // …and it is loading results
    fetcher.state === 'loading'
  )
}
