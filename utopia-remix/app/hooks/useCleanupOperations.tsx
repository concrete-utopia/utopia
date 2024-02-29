import { useFetchers } from '@remix-run/react'
import { useProjectsStore } from '../store'
import React from 'react'
import { isLikeApiError } from '../util/errors'
import { operationFetcherKeyPrefix } from './useFetcherWithOperation'

export function useCleanupOperations() {
  const fetchers = useFetchers()
  const [loading, setLoading] = useLoadingFetchers()
  const consumeOperations = useConsumeOperations()

  React.useEffect(() => {
    const operationsToCleanup = Object.entries(loading)
      .map(([key, value]) => ({ key: key, value: value }))
      .filter((entry) => !fetchers.some((f) => f.key === entry.key))

    if (operationsToCleanup.length > 0) {
      setLoading((loading) => {
        consumeOperations(operationsToCleanup)
        for (let { key } of operationsToCleanup) {
          delete loading[key]
        }
        return loading
      })
    }
  }, [loading, fetchers, consumeOperations])
}

function useConsumeOperations() {
  const removeOperation = useProjectsStore((store) => store.removeOperation)
  const updateOperation = useProjectsStore((store) => store.updateOperation)

  return React.useCallback(
    (operations: { key: string; value: { data: unknown } }[]) => {
      for (const op of operations) {
        if (isLikeApiError(op.value.data)) {
          updateOperation(op.key, { errored: true })
        } else {
          removeOperation(op.key)
        }
      }
    },
    [updateOperation, removeOperation],
  )
}

function useLoadingFetchers() {
  const fetchers = useFetchers()

  const loadingState = React.useState<{ [key: string]: { data: unknown } }>({})
  const [, setLoading] = loadingState

  React.useEffect(() => {
    for (const fetcher of fetchers) {
      if (
        // it's an operation fetcher…
        fetcher.key.startsWith(operationFetcherKeyPrefix) &&
        // …and it has data…
        fetcher.data != null &&
        // …and it is loading results
        fetcher.state === 'loading'
      ) {
        // …then schedule it for collection
        setLoading((loading) => {
          return {
            ...loading,
            [fetcher.key]: { data: fetcher.data },
          }
        })
      }
    }
  }, [fetchers])

  return loadingState
}
