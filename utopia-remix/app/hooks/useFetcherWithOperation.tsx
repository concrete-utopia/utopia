import { useFetcher, useFetchers } from '@remix-run/react'
import React from 'react'
import { useProjectsStore } from '../store'
import { Operation, OperationType } from '../types'
import { ApiError, isLikeApiError } from '../util/errors'

const operationFetcherKeyPrefix = 'operation-'

/**
 * This is a specialized that returns a fetcher that also updates a given project operation.
 */
export function useFetcherWithOperation(projectId: string, type: OperationType) {
  const key = `operation-${projectId}-${type}`

  const fetcher = useFetcher({ key: key })
  const addOperation = useProjectsStore((store) => store.addOperation)

  const submit = React.useCallback(
    (
      operation: Operation,
      data: any,
      options: { method: 'GET' | 'PUT' | 'POST' | 'DELETE'; action: string },
    ) => {
      addOperation(operation, key)
      fetcher.submit(data, options)
    },
    [fetcher, addOperation],
  )

  return {
    ...fetcher,
    submit: submit,
  }
}

export function useCleanupOperations() {
  const fetchers = useFetchers()

  const removeOperation = useProjectsStore((store) => store.removeOperation)
  const updateOperation = useProjectsStore((store) => store.updateOperation)

  const [loading, setLoading] = React.useState<{ [key: string]: { data: unknown } }>({})
  const [processed, setProcessed] = React.useState<{ [key: string]: boolean }>({})

  const process = React.useCallback(
    (ops: { key: string; value: { data: unknown } }[]) => {
      setProcessed((processed) => {
        for (const op of ops) {
          if (isLikeApiError(op.value.data)) {
            updateOperation(op.key, true)
          } else {
            removeOperation(op.key)
          }
          processed[op.key] = true
        }
        return { ...processed, key: true }
      })
    },
    [updateOperation, removeOperation],
  )

  // TODO explain this in a comment
  React.useEffect(() => {
    const operationsToCleanup = Object.entries(loading)
      .map(([key, value]) => ({ key: key, value: value }))
      .filter((entry) => !fetchers.some((f) => f.key === entry.key))

    if (operationsToCleanup.length > 0) {
      process(operationsToCleanup)
      setLoading((loading) => {
        for (let { key } of operationsToCleanup) {
          delete loading[key]
        }
        return loading
      })
    }
  }, [loading, fetchers, process])

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
  }, [fetchers, processed, process])
}
