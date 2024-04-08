import type { UserDetails } from 'prisma-client'
import React from 'react'
import type { StoreApi } from 'zustand'
import { createStore, useStore } from 'zustand'
import { devtools, persist } from 'zustand/middleware'
import type { Category, SortCriteria } from '../routes/projects'
import type { Operation, ProjectAccessRequestWithUserDetails } from '../types'
import { areBaseOperationsEquivalent } from '../types'

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

export type SharingProjectAccessRequestsState = 'loading' | 'ready'

export type SharingProjectAccessRequests = {
  state: SharingProjectAccessRequestsState
  requests: ProjectAccessRequestWithUserDetails[]
}

// State portion that will not be persisted
interface ProjectsStoreStateNonPersisted {
  selectedProjectId: string | null
  selectedCategory: Category
  searchQuery: string
  operations: OperationWithKey[]
  sharingProjectId: string | null
  sharingProjectAccessRequests: SharingProjectAccessRequests
  myUser: UserDetails | null
}

const initialProjectsStoreStateNonPersisted: ProjectsStoreStateNonPersisted = {
  selectedProjectId: null,
  selectedCategory: 'allProjects',
  searchQuery: '',
  operations: [],
  sharingProjectId: null,
  sharingProjectAccessRequests: { state: 'ready', requests: [] },
  myUser: null,
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
  updateOperation: (key: string, data: { errored: boolean }) => void
  setSharingProjectId: (projectId: string | null) => void
  setSharingProjectAccessRequests: (requests: SharingProjectAccessRequests) => void
  setMyUser: (user: UserDetails) => void
}

type ProjectsStore = ProjectsStoreState & ProjectsStoreActions

export const createProjectsStore = (
  initialContextState: Partial<ProjectsStoreState>,
): StoreApi<ProjectsStore> =>
  createStore<ProjectsStore>()(
    devtools(
      persist(
        (set) => ({
          ...initialProjectsStoreStatePersisted,
          ...initialProjectsStoreStateNonPersisted,
          ...initialContextState,

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
                .filter((other) => !areBaseOperationsEquivalent(other, operation))
                .concat(operationWithKey(operation, key)),
            }))
          },
          removeOperation: (key) => {
            return set(({ operations }) => ({
              operations: operations.filter((other) => other.key !== key),
            }))
          },
          updateOperation: (key, data) => {
            return set(({ operations }) => ({
              operations: operations.map((other) => {
                if (other.key === key) {
                  return {
                    ...other,
                    errored: data.errored,
                  }
                }
                return other
              }),
            }))
          },
          setSharingProjectId: (projectId) => {
            return set(() => ({ sharingProjectId: projectId }))
          },
          setSharingProjectAccessRequests: (requests) => {
            return set(() => ({ sharingProjectAccessRequests: requests }))
          },
          setMyUser: (user) => {
            return set(() => ({ myUser: user }))
          },
        }),
        {
          name: 'projects-store',
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

export const ProjectsContext = React.createContext<StoreApi<ProjectsStore> | null>(null)

export function useProjectsStore<T>(selector: (store: ProjectsStore) => T): T {
  const store = React.useContext(ProjectsContext)
  if (store == null) {
    throw new Error('missing projects store context')
  }
  return useStore(store, selector)
}
