import * as localforage from 'localforage'
import { stripTrailingSlash } from '../path-utils'
import { FSNode } from './fs-types'

let dbHeartbeatsStore: LocalForage // There is no way to request a list of existing stores, so we have to explicitly track them

let store: LocalForage
let thisDBName: string

export async function initializeStore(
  storeName: string,
  driver: string = localforage.INDEXEDDB,
): Promise<void> {
  thisDBName = `utopia-project-${storeName}`

  store = localforage.createInstance({
    name: thisDBName,
    driver: driver,
  })

  await store.ready()

  dbHeartbeatsStore = localforage.createInstance({
    name: 'utopia-all-store-heartbeats',
    driver: localforage.INDEXEDDB,
  })

  await dbHeartbeatsStore.ready()

  triggerHeartbeat().then(dropOldStores)
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

async function triggerHeartbeat(): Promise<void> {
  await dbHeartbeatsStore.setItem(thisDBName, Date.now())
  setTimeout(triggerHeartbeat, ONE_HOUR)
}

async function dropOldStores(): Promise<void> {
  const now = Date.now()
  const allStores = await dbHeartbeatsStore.keys()
  const allDBsWithLastHeartbeatTS = await Promise.all(allStores.map(async k => {
    const ts = await dbHeartbeatsStore.getItem<number>(k)
    return {
      dbName: k,
      ts: ts ?? now
    }
  }))
  const dbsToDrop = allDBsWithLastHeartbeatTS.filter(v => (now - v.ts) > ONE_DAY)
  if (dbsToDrop.length > 0) {
    const dbNamesToDrop = dbsToDrop.map(v => v.dbName)
    dbNamesToDrop.forEach(dbName => {
      dbHeartbeatsStore.removeItem(dbName)
      localforage.dropInstance({
        name: dbName,
      })
    })
  }
}