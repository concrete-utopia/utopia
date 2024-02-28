import { useFetcher } from '@remix-run/react'
import React from 'react'
import { Operation, OperationType, ProjectWithoutContent } from '../types'
import { useProjectsStore } from '../store'

/**
 * This is a specialized that returns a fetcher that also updates a given project operation.
 */
export function useFetcherWithOperation(project: ProjectWithoutContent, type: OperationType) {
  const fetcher = useFetcher()
  const [operation, setOperation] = React.useState<Operation | null>(null)
  const addOperation = useProjectsStore((store) => store.addOperation)
  const removeOperation = useProjectsStore((store) => store.removeOperation)

  const submit = React.useCallback(
    (data: any, options: { method: 'GET' | 'PUT' | 'POST' | 'DELETE'; action: string }) => {
      setOperation(() => {
        const operation: Operation = {
          projectId: project.proj_id,
          type: type,
          projectName: project.title,
        }
        addOperation(operation)
        fetcher.submit(data, options)
        return operation
      })
    },
    [fetcher, addOperation],
  )

  React.useEffect(() => {
    if (fetcher.data != null && fetcher.state !== 'submitting' && operation != null) {
      setOperation(null)
      removeOperation(operation)
    }
  }, [fetcher, operation, removeOperation])

  return {
    ...fetcher,
    submit: submit,
  }
}
