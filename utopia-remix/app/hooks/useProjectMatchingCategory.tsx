import React from 'react'
import { useProjectsStore } from '../stores/projectsStore'
import { AccessLevel } from '../types'
import { assertNever } from '../util/assertNever'

/**
 * A hook returning whether the given project's access level is compatible with the
 * currently selected category.
 */
export function useProjectAccessMatchesSelectedCategory(
  projectAccess: AccessLevel | null,
): boolean {
  const selectedCategory = useProjectsStore((store) => store.selectedCategory)

  return React.useMemo(() => {
    if (projectAccess == null) {
      return false
    }
    switch (selectedCategory) {
      case 'allProjects':
        return true
      case 'private':
        return projectAccess == null || projectAccess === AccessLevel.PRIVATE
      case 'public':
        return projectAccess === AccessLevel.PUBLIC
      case 'sharing':
        return projectAccess === AccessLevel.COLLABORATIVE
      case 'sharedWithMe':
      case 'archive':
        return false
      default:
        assertNever(selectedCategory)
    }
  }, [selectedCategory, projectAccess])
}
