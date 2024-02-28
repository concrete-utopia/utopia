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

  const [loading, setLoading] = React.useState<{ [key: string]: boolean }>({})

  // TODO explain this in a comment
  React.useEffect(() => {
    let newLoading = { ...loading }

    const disappeared = Object.keys(loading).filter((key) => !fetchers.some((f) => f.key === key))
    for (let key of disappeared) {
      removeOperation(key)
      delete newLoading[key]
    }

    if (disappeared.length > 0) {
      setLoading(newLoading)
    }
  }, [loading, removeOperation, fetchers])

  React.useEffect(() => {
    for (const fetcher of fetchers) {
      const isOperationFetcher = fetcher.key.startsWith(operationFetcherKeyPrefix)
      const hasData = fetcher.data != null
      if (hasData && isOperationFetcher) {
        if (fetcher.state === 'loading') {
          // it will disappear next if successful
          setLoading((loading) => {
            if (loading[fetcher.key] === true) {
              return loading
            }
            return { ...loading, [fetcher.key]: true }
          })
        } else if (fetcher.state === 'idle') {
          // it's good for collection
          if (isLikeApiError(fetcher.data)) {
            updateOperation(fetcher.key, true)
          } else {
            removeOperation(fetcher.key)
          }
        }
      }
    }
  }, [fetchers, removeOperation, updateOperation])
}
