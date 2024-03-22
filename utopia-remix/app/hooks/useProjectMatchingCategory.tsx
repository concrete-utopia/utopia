import React from 'react'
import { useProjectsStore } from '../store'
import type { ProjectListing } from '../types'
import { AccessLevel } from '../types'
import { assertNever } from '../util/assertNever'

/**
 * A hook returning whether the given project's access level is compatible with the
 * currently selected category.
 */
export function useProjectAccessMatchesSelectedCategory(project: ProjectListing | null): boolean {
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
      case 'archive':
        return false
      default:
        assertNever(selectedCategory)
    }
  }, [selectedCategory, project])
}
