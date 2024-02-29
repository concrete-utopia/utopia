import { useFetchers } from '@remix-run/react'
import { useProjectsStore } from '../store'
import React from 'react'
import { isLikeApiError } from '../util/errors'
import { operationFetcherKeyPrefix } from './useFetcherWithOperation'

export function useCleanupOperations() {
  const { operationsToCleanup, resetOperationsToCleanup } = useLoadingFetchers()
  const consumeOperations = useConsumeOperations()

  React.useEffect(() => {
    const operationsToCleanupList = Object.entries(operationsToCleanup).map(([key, value]) => {
      return {
        key: key,
        value: value,
      }
    })

    if (operationsToCleanupList.length > 0) {
      consumeOperations(operationsToCleanupList)
      resetOperationsToCleanup(operationsToCleanupList.map((e) => e.key))
    }
  }, [operationsToCleanup, consumeOperations, resetOperationsToCleanup])
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

type OperationFetcherData = { [key: string]: { data: unknown } }

function useLoadingFetchers() {
  const fetchers = useFetchers()

  // keep a ledger of loading fetchers
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
