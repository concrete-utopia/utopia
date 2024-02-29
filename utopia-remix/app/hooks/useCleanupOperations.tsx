import { useFetchers } from '@remix-run/react'
import { useProjectsStore } from '../store'
import React from 'react'
import { isLikeApiError } from '../util/errors'
import { operationFetcherKeyPrefix } from './useFetcherWithOperation'

/**
 * This hook will react to fetchers changes, so that the ones tied to ongoing Operations,
 * when transitioning from loading to idle (and hence being cleared) will have their corresponding
 * Operations either removed or marked as errored, depending on the result of the fetcher data
 * received back.
 */
export function useCleanupOperations() {
  const { operationsToCleanup, resetOperationsToCleanup } = useLoadingFetchers()
  const consumeOperations = useConsumeOperations()

  // when the operations to cleanup change, consume the related operations and then reset
  // them.
  React.useEffect(() => {
    const keysToCleanup = Object.keys(operationsToCleanup)
    if (keysToCleanup.length > 0) {
      consumeOperations(operationsToCleanup)
      resetOperationsToCleanup(keysToCleanup) // explicitly send the current list of keys
    }
  }, [operationsToCleanup, consumeOperations, resetOperationsToCleanup])
}

/**
 * Consume the given operations, deleting them if successful and marking them as errored otherwise.
 */
function useConsumeOperations() {
  const removeOperation = useProjectsStore((store) => store.removeOperation)
  const updateOperation = useProjectsStore((store) => store.updateOperation)

  return React.useCallback(
    (operations: OperationFetcherData) => {
      for (const [key, value] of Object.entries(operations)) {
        if (isLikeApiError(value.data)) {
          updateOperation(key, { errored: true })
        } else {
          removeOperation(key)
        }
      }
    },
    [updateOperation, removeOperation],
  )
}

type OperationFetcherData = { [key: string]: { data: unknown } }

/**
 * Listen to fetcher changes and return a list of operations to cleanup.
 */
function useLoadingFetchers() {
  const fetchers = useFetchers()

  // accumulated fetchers that were seen in the loading state
  const [loadingFetchers, setLoadingFetchers] = React.useState<OperationFetcherData>({})
  // list of fetchers that need to be cleaned up
  const [operationsToCleanup, setOperationsToCleanup] = React.useState<OperationFetcherData>({})

  // list of keys to be cleaned up
  const keysToCleanup = React.useMemo(() => {
    return Object.keys(loadingFetchers).filter((key) => {
      return !fetchers.some((f) => f.key === key)
    })
  }, [fetchers, loadingFetchers])

  // reset operations
  const resetOperationsToCleanup = React.useCallback(
    (targets: string[]) => {
      setLoadingFetchers((loading) => {
        setOperationsToCleanup({})
        return Object.entries(loading).reduce((acc, [key, value]) => {
          if (!targets.includes(key)) {
            acc[key] = value
          }
          return acc
        }, {} as OperationFetcherData)
      })
    },
    [loadingFetchers],
  )

  // fetchers currently in the loading state
  const currentLoadingFetchers = React.useMemo(() => {
    return fetchers
      .filter((fetcher) => {
        return (
          // it's an operation fetcher…
          fetcher.key.startsWith(operationFetcherKeyPrefix) &&
          // …and it has data…
          fetcher.data != null &&
          // …and it is loading results
          fetcher.state === 'loading'
        )
      })
      .reduce((acc, current) => {
        acc[current.key] = { data: current.data }
        return acc
      }, {} as OperationFetcherData)
  }, [fetchers])

  // react to fetchers changing
  React.useEffect(() => {
    if (Object.keys(currentLoadingFetchers).length > 0) {
      setLoadingFetchers({ ...loadingFetchers, ...currentLoadingFetchers })
    }

    if (keysToCleanup.length > 0) {
      setOperationsToCleanup(() => {
        const result: OperationFetcherData = {}
        for (const key of keysToCleanup) {
          result[key] = loadingFetchers[key]
        }
        return result
      })
    }
  }, [keysToCleanup, currentLoadingFetchers])

  return { operationsToCleanup, resetOperationsToCleanup }
}
