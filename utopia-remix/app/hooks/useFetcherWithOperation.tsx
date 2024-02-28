import { useFetcher, useFetchers } from '@remix-run/react'
import React from 'react'
import { useProjectsStore } from '../store'
import { Operation } from '../types'

const operationFetcherKeyPrefix = 'operation-'

/**
 * This is a specialized that returns a fetcher that also updates a given project operation.
 */
export function useFetcherWithOperation(operation: Operation) {
  const key = `operation-${operation.projectId}-${operation.type}`

  const fetcher = useFetcher({ key: key })
  const addOperation = useProjectsStore((store) => store.addOperation)

  const submit = React.useCallback(
    (data: any, options: { method: 'GET' | 'PUT' | 'POST' | 'DELETE'; action: string }) => {
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

  React.useEffect(() => {
    for (const fetcher of fetchers) {
      const isOperationFetcher = fetcher.key.startsWith(operationFetcherKeyPrefix)
      const isNotSubmitting = fetcher.data != null && fetcher.state !== 'submitting'
      if (isOperationFetcher && isNotSubmitting) {
        removeOperation(fetcher.key)
      }
    }
  }, [fetchers])
}
