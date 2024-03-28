import { useFetcher } from '@remix-run/react'
import React from 'react'
import { useProjectsStore } from '../stores/projectsStore'
import type { Operation, OperationType } from '../types'

export const operationFetcherKeyPrefix = 'operation-'

/**
 * This is a specialized that returns a fetcher that also updates a given project operation.
 */
export function useFetcherWithOperation(projectId: string | null, type: OperationType) {
  const key = projectId == null ? undefined : `operation-${projectId}-${type}`

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
    [fetcher, key, addOperation],
  )

  return {
    ...fetcher,
    submit: submit,
  }
}
