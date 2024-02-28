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

  React.useEffect(() => {
    for (const fetcher of fetchers) {
      const isOperationFetcher = fetcher.key.startsWith(operationFetcherKeyPrefix)
      const hasDataWhileNotSubmitting = fetcher.data != null && fetcher.state !== 'submitting'
      if (isOperationFetcher && hasDataWhileNotSubmitting) {
        if (isLikeApiError(fetcher.data)) {
          updateOperation(fetcher.key, true)
        } else {
          removeOperation(fetcher.key)
        }
      }
    }
  }, [fetchers])
}
