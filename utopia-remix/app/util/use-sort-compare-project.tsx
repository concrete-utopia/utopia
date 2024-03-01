import moment from 'moment'
import React from 'react'
import { useProjectsStore } from '../store'
import { ProjectWithoutContent } from '../types'
import { assertNever } from './assertNever'

export function useSortCompareProject() {
  const sortCriteria = useProjectsStore((store) => store.sortCriteria)
  const sortAscending = useProjectsStore((store) => store.sortAscending)

  return React.useCallback(
    (a: ProjectWithoutContent, b: ProjectWithoutContent): number => {
      switch (sortCriteria) {
        case 'title':
          return sortAscending ? a.title.localeCompare(b.title) : b.title.localeCompare(a.title)
        case 'dateCreated':
          return sortAscending
            ? moment(a.created_at).unix() - moment(b.created_at).unix()
            : moment(b.created_at).unix() - moment(a.created_at).unix()
        case 'dateModified':
          return sortAscending
            ? moment(a.modified_at).unix() - moment(b.modified_at).unix()
            : moment(b.modified_at).unix() - moment(a.modified_at).unix()
        default:
          assertNever(sortCriteria)
      }
    },
    [sortCriteria, sortAscending],
  )
}

export function useProjectMatchesQuery() {
  const searchQuery = useProjectsStore((store) => store.searchQuery)
  const sanitizedQuery = React.useMemo(() => searchQuery.trim().toLowerCase(), [searchQuery])

  return React.useCallback(
    (project: ProjectWithoutContent): boolean => {
      if (sanitizedQuery.length === 0) {
        return true
      }
      return project.title.toLowerCase().includes(sanitizedQuery)
    },
    [sanitizedQuery],
  )
}

export function useProjectIsOnActiveOperation() {
  const activeOperations = useProjectsStore((store) => store.operations.filter((op) => !op.errored))
  const selectedCategory = useProjectsStore((store) => store.selectedCategory)

  return React.useCallback(
    (project: ProjectWithoutContent): boolean => {
      return !activeOperations.some((op) => {
        switch (selectedCategory) {
          case 'allProjects':
            return op.type === 'delete' && op.projectId === project.proj_id
          case 'trash':
            return (
              (op.type === 'restore' || op.type === 'destroy') && op.projectId === project.proj_id
            )
          default:
            assertNever(selectedCategory)
        }
      })
    },
    [activeOperations, selectedCategory],
  )
}
