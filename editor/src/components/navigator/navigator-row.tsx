import type { NavigatorEntry } from '../editor/store/editor-state'

export type NavigatorRow = RegularNavigatorRow | CondensedNavigatorRow

interface RegularNavigatorRow {
  type: 'regular-row'
  entry: NavigatorEntry
}

interface CondensedNavigatorRow {
  type: 'condensed-row'
  entries: Array<NavigatorEntry>
}

export function isRegulaNavigatorrRow(
  row: NavigatorRow,
): row is { type: 'regular-row'; entry: NavigatorEntry } {
  return row.type === 'regular-row'
}

export function getEntriesForRow(row: NavigatorRow): Array<NavigatorEntry> {
  if (isRegulaNavigatorrRow(row)) {
    return [row.entry]
  } else {
    return row.entries
  }
}
