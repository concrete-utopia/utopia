import localforage from 'localforage'
import type { StoredEditorState } from './store/editor-state'
import { StoredStateVersion } from './store/editor-state'

const StoredStateKey = 'stored-state'

let EnableLocalStoredState = true
export function disableStoredStateforTests() {
  EnableLocalStoredState = false
}

interface StoredStateOnDisk {
  projectId: string
  storedState: StoredEditorState
}

export async function loadStoredState(projectId: string): Promise<StoredEditorState | null> {
  if (EnableLocalStoredState) {
    const asStored = await localforage.getItem<StoredStateOnDisk | null>(StoredStateKey)
    if (asStored == null) {
      return Promise.resolve(null)
    } else {
      if (asStored.projectId === projectId && asStored.storedState.version === StoredStateVersion) {
        return Promise.resolve(asStored.storedState)
      } else {
        return Promise.resolve(null)
      }
    }
  } else {
    return null
  }
}

export async function saveStoredState(
  projectId: string | null,
  storedState: StoredEditorState,
): Promise<void> {
  if (EnableLocalStoredState && projectId != null) {
    await localforage.setItem(StoredStateKey, {
      projectId: projectId,
      storedState: storedState,
    })
  }
  return Promise.resolve(undefined)
}
