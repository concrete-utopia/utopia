import * as localforage from 'localforage'
import { StoredEditorState } from './store/editor-state'

const StoredStateKey = 'stored-state'

interface StoredStateOnDisk {
  projectId: string
  storedState: StoredEditorState
}

export async function loadStoredState(projectId: string | null): Promise<StoredEditorState | null> {
  if (projectId == null) {
    return Promise.resolve(null)
  } else {
    const asStored = await localforage.getItem<StoredStateOnDisk | null>(StoredStateKey)
    if (asStored == null) {
      return Promise.resolve(null)
    } else {
      if (asStored.projectId === projectId) {
        return Promise.resolve(asStored.storedState)
      } else {
        return Promise.resolve(null)
      }
    }
  }
}

export async function saveStoredState(
  projectId: string | null,
  storedState: StoredEditorState,
): Promise<void> {
  if (projectId != null) {
    await localforage.setItem(StoredStateKey, {
      projectId: projectId,
      storedState: storedState,
    })
  }
  return Promise.resolve(undefined)
}
