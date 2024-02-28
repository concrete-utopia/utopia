import { create } from 'zustand'
import { devtools, persist } from 'zustand/middleware'
import { Category, SortCriteria } from './routes/projects'
import { Operation, operationsEqual } from './types'

interface ProjectsStoreStatePersisted {
  selectedProjectId: string | null
  selectedCategory: Category
  searchQuery: string
  sortCriteria: SortCriteria
  sortAscending: boolean
  gridView: boolean
}

const initialProjectsStoreStatePersisted: ProjectsStoreStatePersisted = {
  selectedCategory: 'allProjects',
  selectedProjectId: null,
  searchQuery: '',
  sortCriteria: 'dateModified',
  sortAscending: false,
  gridView: true,
}

interface ProjectsStoreStateNonPersisted {
  operations: Operation[]
}

const initialProjectsStoreStateNonPersisted: ProjectsStoreStateNonPersisted = {
  operations: [],
}

type ProjectsStoreState = ProjectsStoreStatePersisted & ProjectsStoreStateNonPersisted

interface ProjectsStoreActions {
  setSelectedProjectId: (projectId: string | null) => void
  setSelectedCategory: (category: Category) => void
  setSearchQuery: (query: string) => void
  setSortCriteria: (sortCriteria: SortCriteria) => void
  setSortAscending: (sortAscending: boolean) => void
  setGridView: (gridView: boolean) => void
  addOperation: (operation: Operation) => void
  removeOperation: (operation: Operation) => void
}

type ProjectsStore = ProjectsStoreState & ProjectsStoreActions

export const useProjectsStore = create<ProjectsStore>()(
  devtools(
    persist(
      (set) => ({
        ...initialProjectsStoreStatePersisted,
        ...initialProjectsStoreStateNonPersisted,

        setSelectedCategory: (category: Category) => {
          return set(() => ({ selectedCategory: category }))
        },
        setSelectedProjectId: (projectId: string | null) => {
          return set(() => ({ selectedProjectId: projectId }))
        },
        setSearchQuery: (query) => {
          return set(() => ({ searchQuery: query }))
        },
        setSortCriteria: (sortCriteria) => {
          return set(() => ({ sortCriteria: sortCriteria }))
        },
        setSortAscending: (sortAscending) => {
          return set(() => ({ sortAscending: sortAscending }))
        },
        setGridView: (gridView) => {
          return set(() => ({ gridView: gridView }))
        },
        addOperation: (operation) => {
          return set(({ operations }) => ({
            operations: operations
              .filter((other) => !operationsEqual(other, operation))
              .concat(operation),
          }))
        },
        removeOperation: (operation) => {
          return set(({ operations }) => ({
            operations: operations.filter((other) => !operationsEqual(other, operation)),
          }))
        },
      }),
      {
        name: 'store',
        partialize: (fullStore) => {
          const nonPersistedKeys = Object.keys(
            initialProjectsStoreStateNonPersisted,
          ) as (keyof ProjectsStoreStateNonPersisted)[]

          let persistedStore: Partial<ProjectsStore> = { ...fullStore }
          nonPersistedKeys.forEach((key) => delete persistedStore[key])

          return persistedStore
        },
      },
    ),
  ),
)
