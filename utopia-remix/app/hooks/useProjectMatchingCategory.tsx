import React from 'react'
import { useProjectsStore } from '../store'
import type { ProjectWithoutContent } from '../types'
import { AccessLevel } from '../types'
import { assertNever } from '../util/assertNever'

export function useProjectAccessMatchesSelectedCategory(project: ProjectWithoutContent | null) {
  const selectedCategory = useProjectsStore((store) => store.selectedCategory)

  return React.useMemo(() => {
    if (project == null) {
      return false
    }
    switch (selectedCategory) {
      case 'allProjects':
        return true
      case 'private':
        return (
          project.ProjectAccess == null ||
          project.ProjectAccess.access_level === AccessLevel.PRIVATE
        )
      case 'public':
        return project.ProjectAccess?.access_level === AccessLevel.PUBLIC
      case 'sharing':
        return project.ProjectAccess?.access_level === AccessLevel.COLLABORATIVE
      case 'sharedWithMe':
      case 'trash':
        return false
      default:
        assertNever(selectedCategory)
    }
  }, [selectedCategory, project])
}
