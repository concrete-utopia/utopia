import * as localforage from 'localforage'
import type { Either } from '../lite-either'
import { left, mapEither, right } from '../lite-either'
import { stripTrailingSlash } from '../path-utils'
import type { FSNode } from './fs-types'
import { defer } from './fs-utils'

let dbHeartbeatsStore: LocalForage // There is no way to request a list of existing stores, so we have to explicitly track them

let store: LocalForage | null
let thisDBName: string

const StoreExistsKey = '.store-exists'

const firstInitialize = defer<void>()
let initializeStoreChain: Promise<void> = Promise.resolve()

export async function initializeStore(
  storeName: string,
  driver: string = localforage.INDEXEDDB,
): Promise<void> {
  async function innerInitialize(): Promise<void> {
    thisDBName = `utopia-project-${storeName}`

    store = localforage.createInstance({
      name: thisDBName,
      driver: driver,
    })

    await store.ready()
    await store.setItem(StoreExistsKey, true)

    dbHeartbeatsStore = localforage.createInstance({
      name: 'utopia-all-store-heartbeats',
      driver: localforage.INDEXEDDB,
    })

    await dbHeartbeatsStore.ready()

    triggerHeartbeat().then(dropOldStores)
  }
  initializeStoreChain = initializeStoreChain.then(innerInitialize)
  firstInitialize.resolve()
  return initializeStoreChain
}

export interface StoreDoesNotExist {
  type: 'StoreDoesNotExist'
}

const StoreDoesNotExistConst: StoreDoesNotExist = {
  type: 'StoreDoesNotExist',
}

export function isStoreDoesNotExist(t: unknown): t is StoreDoesNotExist {
  return (t as any)?.type === 'StoreDoesNotExist'
}

export type AsyncFSResult<T> = Promise<Either<StoreDoesNotExist, T>>

const StoreExistsKeyInterval = 1000

interface StoreKeyExistsCheck {
  lastCheckedTime: number
  exists: boolean
}

let lastCheckedForStoreKeyExists: StoreKeyExistsCheck | null = null

async function checkStoreKeyExists(): Promise<boolean> {
  if (store == null) {
    return false
  } else {
    const now = Date.now()
    if (
      lastCheckedForStoreKeyExists == null ||
      lastCheckedForStoreKeyExists.lastCheckedTime + StoreExistsKeyInterval < now
    ) {
      const exists = (await store.getItem<boolean>(StoreExistsKey)) ?? false
      lastCheckedForStoreKeyExists = {
        lastCheckedTime: now,
        exists: exists,
      }
      return exists
    } else {
      return lastCheckedForStoreKeyExists.exists
    }
  }
}

async function withSanityCheckedStore<T>(
  withStore: (sanityCheckedStore: LocalForage) => Promise<T>,
): AsyncFSResult<T> {
  await firstInitialize
  await initializeStoreChain
  const storeExists = await checkStoreKeyExists()
  if (store != null && storeExists) {
    const result = await withStore(store)
    return right(result)
  } else {
    store = null
    return left(StoreDoesNotExistConst)
  }
}

export async function keys(): AsyncFSResult<string[]> {
  return withSanityCheckedStore((sanityCheckedStore: LocalForage) => sanityCheckedStore.keys())
}

export async function getItem(path: string): AsyncFSResult<FSNode | null> {
  return withSanityCheckedStore((sanityCheckedStore: LocalForage) =>
    sanityCheckedStore.getItem<FSNode>(stripTrailingSlash(path)),
  )
}

export async function setItem(path: string, value: FSNode): AsyncFSResult<FSNode> {
  return withSanityCheckedStore((sanityCheckedStore: LocalForage) =>
    sanityCheckedStore.setItem(stripTrailingSlash(path), value),
  )
}

export async function removeItem(path: string): AsyncFSResult<void> {
  return withSanityCheckedStore((sanityCheckedStore: LocalForage) =>
    sanityCheckedStore.removeItem(stripTrailingSlash(path)),
  )
}

const ONE_HOUR = 1000 * 60 * 60
const ONE_DAY = ONE_HOUR * 24

async function triggerHeartbeat(): Promise<void> {
  await dbHeartbeatsStore.setItem(thisDBName, Date.now())
  setTimeout(triggerHeartbeat, ONE_HOUR)
}

async function dropOldStores(): Promise<void> {
  const now = Date.now()
  const allStores = await dbHeartbeatsStore.keys()
  const allDBsWithLastHeartbeatTS = await Promise.all(
    allStores.map(async (k) => {
      const ts = await dbHeartbeatsStore.getItem<number>(k)
      return {
        dbName: k,
        ts: ts ?? now,
      }
    }),
  )
  const dbsToDrop = allDBsWithLastHeartbeatTS.filter((v) => now - v.ts > ONE_DAY)
  if (dbsToDrop.length > 0) {
    const dbNamesToDrop = dbsToDrop.map((v) => v.dbName)
    dbNamesToDrop.forEach((dbName) => {
      dbHeartbeatsStore.removeItem(dbName)
      localforage.dropInstance({
        name: dbName,
      })
    })
  }
}
