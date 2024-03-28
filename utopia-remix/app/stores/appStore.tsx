import React from 'react'
import type { StoreApi } from 'zustand'
import { createStore, useStore } from 'zustand'
import { devtools, persist } from 'zustand/middleware'
import type { BrowserEnvironmentType } from '../env.server'

interface AppStoreStateNonPersisted {
  env: BrowserEnvironmentType | null
}

interface AppStoreStatePersisted {
  env: BrowserEnvironmentType | null
}

type AppStoreState = AppStoreStateNonPersisted & AppStoreStatePersisted

const initialAppStoreStateNonPersisted = {
  env: null,
}

type AppStore = AppStoreState

export const createAppStore = (initialContextState: Partial<AppStoreState>): StoreApi<AppStore> =>
  createStore<AppStore>()(
    devtools(
      persist(
        () => ({
          ...initialAppStoreStateNonPersisted,
          ...initialContextState,
        }),
        {
          name: 'app-store',
          partialize: (fullStore) => {
            const nonPersistedKeys = Object.keys(
              initialAppStoreStateNonPersisted,
            ) as (keyof AppStoreStateNonPersisted)[]

            let persistedStore: Partial<AppStore> = { ...fullStore }
            nonPersistedKeys.forEach((key) => delete persistedStore[key])

            return persistedStore
          },
        },
      ),
    ),
  )

export const AppContext = React.createContext<StoreApi<AppStore> | null>(null)

export function useAppStore<T>(selector: (store: AppStore) => T): T {
  const store = React.useContext(AppContext)
  if (store == null) {
    throw new Error('missing app context')
  }
  return useStore(store, selector)
}
