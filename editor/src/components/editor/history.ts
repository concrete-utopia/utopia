import { assertNever } from '../../core/shared/utils'
import { updateAssetFileName } from './server'
import type { DerivedState, EditorState } from './store/editor-state'

const MAX_HISTORY = 50

export interface AssetRename {
  filenameChangedFrom: string
  filenameChangedTo: string
}

export type HistoryItem = {
  editor: EditorState
  derived: DerivedState
  assetRenames: Array<AssetRename>
}

export interface StateHistory {
  previous: HistoryItem[]
  current: HistoryItem
  next: HistoryItem[]
}

export type AssetFilenameUpdate = (
  projectId: string,
  oldFileName: string,
  newFileName: string,
) => Promise<void>

// Chain promises off of this so that there's not 50 running at once.
export let baseAssetPromise: Promise<void> = Promise.resolve()

export function triggerAssetRenames(
  projectId: string | null,
  assetRenames: Array<AssetRename>,
  undoOrRedo: 'undo' | 'redo',
): void {
  if (projectId != null) {
    switch (undoOrRedo) {
      case 'undo':
        baseAssetPromise = assetRenames.reduceRight(async (working, rename) => {
          return working.then(() => {
            return updateAssetFileName(
              projectId,
              rename.filenameChangedTo,
              rename.filenameChangedFrom,
            )
          })
        }, baseAssetPromise)
        break
      case 'redo':
        baseAssetPromise = assetRenames.reduce(async (working, rename) => {
          return working.then(() => {
            return updateAssetFileName(
              projectId,
              rename.filenameChangedFrom,
              rename.filenameChangedTo,
            )
          })
        }, baseAssetPromise)
        break
      default:
        assertNever(undoOrRedo)
    }
  }
}

type RunEffects = 'run-side-effects' | 'no-side-effects'

export function undo(
  projectId: string | null,
  stateHistory: StateHistory,
  runSideEffects: RunEffects,
): StateHistory {
  // console.log('undo!')
  // FIXME Undo should update the time stamp of any affected files
  if (runSideEffects === 'run-side-effects') {
    // Unwind any asset renames.
    triggerAssetRenames(projectId, stateHistory.current.assetRenames, 'undo')
  }

  // Create the new state history.
  const newCurrent = stateHistory.previous[0]
  return {
    previous: stateHistory.previous.slice(1),
    current: newCurrent,
    next: [stateHistory.current, ...stateHistory.next],
  }
}

export function redo(
  projectId: string | null,
  stateHistory: StateHistory,
  runSideEffects: RunEffects,
): StateHistory {
  // FIXME Redo should update the time stamp of any affected files
  const newCurrent = stateHistory.next[0]
  if (runSideEffects === 'run-side-effects') {
    // Playback any asset renames.
    triggerAssetRenames(projectId, newCurrent.assetRenames, 'redo')
  }

  // Create the new state history.
  return {
    previous: [stateHistory.current, ...stateHistory.previous],
    current: newCurrent,
    next: stateHistory.next.slice(1),
  }
}

export function canUndo(stateHistory: StateHistory): boolean {
  if (stateHistory.previous.length > 0) {
    return true
  } else {
    return false
  }
}

export function canRedo(stateHistory: StateHistory): boolean {
  if (stateHistory.next.length > 0) {
    return true
  } else {
    return false
  }
}

export function add(
  stateHistory: StateHistory,
  editor: EditorState,
  derived: DerivedState,
  assetRenames: Array<AssetRename>,
): StateHistory {
  // console.log('added to history!')
  let previous =
    stateHistory.previous.length > MAX_HISTORY
      ? stateHistory.previous.slice(0, MAX_HISTORY)
      : stateHistory.previous

  const newHistory: StateHistory = {
    previous: [stateHistory.current, ...previous],
    current: {
      editor: editor,
      derived: derived,
      assetRenames: assetRenames,
    },
    next: [],
  }
  return newHistory
}

export function replaceLast(
  stateHistory: StateHistory,
  editor: EditorState,
  derived: DerivedState,
  assetRenames: Array<AssetRename>,
): StateHistory {
  const newHistory: StateHistory = {
    previous: stateHistory.previous,
    current: {
      editor: editor,
      derived: derived,
      assetRenames: stateHistory.current.assetRenames.concat(assetRenames),
    },
    next: stateHistory.next,
  }
  return newHistory
}

export function replaceLastWithUpdate(
  stateHistory: StateHistory,
  updateEditor: (editorState: EditorState) => EditorState,
): StateHistory {
  const newHistory: StateHistory = {
    previous: stateHistory.previous,
    current: {
      ...stateHistory.current,
      editor: updateEditor(stateHistory.current.editor),
    },
    next: stateHistory.next,
  }
  return newHistory
}

export function init(editor: EditorState, derived: DerivedState): StateHistory {
  return {
    previous: [],
    current: {
      editor: editor,
      derived: derived,
      assetRenames: [],
    },
    next: [],
  }
}
