import * as localforage from 'localforage'
import { stripTrailingSlash } from '../path-utils'
import { FSNode } from './fs-types'

let store: LocalForage

export async function initializeStore(
  storeName: string,
  driver: string = localforage.INDEXEDDB,
): Promise<void> {
  store = localforage.createInstance({
    name: 'utopia',
    storeName: storeName,
    driver: driver,
  })
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
