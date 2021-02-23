import * as localforage from 'localforage'
import { stripTrailingSlash } from '../path-utils'
import { FSNode } from './fs-types'

let storesLastAccessedStore: LocalForage // There is no way to request a list of existing stores, so we have to explicitly track them

let store: LocalForage
let thisStoreName: string

export async function initializeStore(
  storeName: string,
  driver: string = localforage.INDEXEDDB,
): Promise<void> {
  thisStoreName = storeName

  store = localforage.createInstance({
    name: 'utopia',
    storeName: storeName,
    driver: driver,
  })

  storesLastAccessedStore = localforage.createInstance({
    name: 'utopia',
    storeName: 'AllStoresLastAccessedTSs',
    driver: localforage.INDEXEDDB,
  })

  updateLastAccessed()
  dropOldStores()
}

export async function keys(): Promise<string[]> {
  return store.keys()
}

export async function getItem(path: string): Promise<FSNode | null> {
  return store.getItem<FSNode>(stripTrailingSlash(path))
}

export async function setItem(path: string, value: FSNode): Promise<FSNode> {
  return store.setItem(stripTrailingSlash(path), value)
}

export async function removeItem(path: string): Promise<void> {
  return store.removeItem(stripTrailingSlash(path))
}

const ONE_HOUR = 1000 * 60 * 60 
const ONE_DAY = ONE_HOUR * 24

async function updateLastAccessed(): Promise<void> {
  await storesLastAccessedStore.setItem(thisStoreName, Date.now())
  setTimeout(updateLastAccessed, ONE_HOUR)
}

export async function checkLastAccessed(): Promise<number> {
  const lastAccessedFile = await storesLastAccessedStore.getItem<number>(thisStoreName)
  return lastAccessedFile ?? 0
}

async function dropOldStores(): Promise<void> {
  const now = Date.now()
  const allStores = await storesLastAccessedStore.keys()
  const allStoresWithLastAccessedTS = await Promise.all(allStores.map(async k => {
    const ts = await storesLastAccessedStore.getItem<number>(k)
    return {
      store: k,
      ts: ts ?? now
    }
  }))
  const storesToDrop = allStoresWithLastAccessedTS.filter(v => (now - v.ts) > ONE_DAY)
  if (storesToDrop.length > 0) {
    const storeNamesToDrop = storesToDrop.map(v => v.store)
    console.log(`Dropping stores ${JSON.stringify(storeNamesToDrop)}`)
    storeNamesToDrop.forEach(storeName => {
      localforage.dropInstance({
        name: 'utopia',
        storeName: storeName
      }).then(() => console.log(`Dropped ${storeName}`))
    })
  }
}