import { create } from 'zustand'
import { devtools, persist } from 'zustand/middleware'
import { Category, SortCriteria } from './routes/projects'
import { Operation, operationsEqual } from './types'

// State portion that will be persisted
interface ProjectsStoreStatePersisted {
  sortCriteria: SortCriteria
  sortAscending: boolean
  gridView: boolean
}

const initialProjectsStoreStatePersisted: ProjectsStoreStatePersisted = {
  sortCriteria: 'dateModified',
  sortAscending: false,
  gridView: true,
}

// State portion that will not be persisted
interface ProjectsStoreStateNonPersisted {
  selectedProjectId: string | null
  selectedCategory: Category
  searchQuery: string
  operations: OperationWithKey[]
}

const initialProjectsStoreStateNonPersisted: ProjectsStoreStateNonPersisted = {
  selectedProjectId: null,
  selectedCategory: 'allProjects',
  searchQuery: '',
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
  addOperation: (operation: Operation, key: string) => void
  removeOperation: (key: string) => void
  updateOperation: (key: string, errored: boolean) => void
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
        addOperation: (operation, key) => {
          return set(({ operations }) => ({
            operations: operations
              .filter((other) => !operationsEqual(other, operation))
              .concat(operationWithKey(operation, key)),
          }))
        },
        removeOperation: (key) => {
          return set(({ operations }) => ({
            operations: operations.filter((other) => other.key !== key),
          }))
        },
        updateOperation: (key, errored) => {
          return set(({ operations }) => ({
            operations: operations.map((other) => {
              if (other.key === key) {
                return { ...other, errored: errored }
              }
              return other
            }),
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

export type OperationWithKey = Operation & {
  key: string
  startedAt: number
  errored: boolean
}

function operationWithKey(operation: Operation, key: string): OperationWithKey {
  return {
    ...operation,
    key: key,
    startedAt: Date.now(),
    errored: false,
  }
}
