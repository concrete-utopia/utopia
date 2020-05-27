import { DerivedState, EditorState } from './store/editor-state'

const MAX_HISTORY = 50

export type HistoryItem = {
  editor: EditorState
  derived: DerivedState
}

export interface StateHistory {
  previous: HistoryItem[]
  current: HistoryItem
  next: HistoryItem[]
}

export function undo(stateHistory: StateHistory): StateHistory {
  const newCurrent = stateHistory.previous[0]
  return {
    previous: stateHistory.previous.slice(1),
    current: newCurrent,
    next: [stateHistory.current, ...stateHistory.next],
  }
}

export function redo(stateHistory: StateHistory): StateHistory {
  return {
    previous: [stateHistory.current, ...stateHistory.previous],
    current: stateHistory.next[0],
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
): StateHistory {
  let previous =
    stateHistory.previous.length > MAX_HISTORY
      ? stateHistory.previous.slice(0, MAX_HISTORY)
      : stateHistory.previous

  const newHistory = {
    previous: [stateHistory.current, ...previous],
    current: {
      editor: editor,
      derived: derived,
    },
    next: [],
  }
  return newHistory
}

export function init(editor: EditorState, derived: DerivedState): StateHistory {
  return {
    previous: [],
    current: {
      editor: editor,
      derived: derived,
    },
    next: [],
  }
}
