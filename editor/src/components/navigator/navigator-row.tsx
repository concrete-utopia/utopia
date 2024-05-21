import type { NavigatorEntry } from '../editor/store/editor-state'

export type NavigatorRow = RegularNavigatorRow | CondensedNavigatorRow

export interface RegularNavigatorRow {
  type: 'regular-row'
  entry: NavigatorEntry
}

export function regularNavigatorRow(entry: NavigatorEntry): RegularNavigatorRow {
  return {
    type: 'regular-row',
    entry: entry,
  }
}

export function isRegulaNavigatorrRow(row: NavigatorRow): row is RegularNavigatorRow {
  return row.type === 'regular-row'
}

export interface CondensedNavigatorRow {
  type: 'condensed-row'
  variant: 'trunk' | 'leaf'
  entries: Array<NavigatorEntry>
}

export function condensedNavigatorRow(
  entries: Array<NavigatorEntry>,
  variant: 'trunk' | 'leaf',
): CondensedNavigatorRow {
  return {
    type: 'condensed-row',
    variant: variant,
    entries: entries,
  }
}

export function isCondensedNavigatorRow(row: NavigatorRow): row is CondensedNavigatorRow {
  return row.type === 'condensed-row'
}

export function getEntriesForRow(row: NavigatorRow): Array<NavigatorEntry> {
  if (isRegulaNavigatorrRow(row)) {
    return [row.entry]
  } else {
    return row.entries
  }
}
