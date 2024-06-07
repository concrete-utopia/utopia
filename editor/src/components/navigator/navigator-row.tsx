import type { NavigatorEntry } from '../editor/store/editor-state'

export type NavigatorRow = RegularNavigatorRow | CondensedNavigatorRow

export interface RegularNavigatorRow {
  type: 'regular-row'
  indentation: number
  entry: NavigatorEntry
}

export function regularNavigatorRow(
  entry: NavigatorEntry,
  indentation: number,
): RegularNavigatorRow {
  return {
    type: 'regular-row',
    entry: entry,
    indentation: indentation,
  }
}

export function isRegulaNavigatorRow(row: NavigatorRow): row is RegularNavigatorRow {
  return row.type === 'regular-row'
}

export interface CondensedNavigatorRow {
  type: 'condensed-row'
  indentation: number
  variant: CondensedNavigatorRowVariant
  entries: Array<NavigatorEntry>
}

export type CondensedNavigatorRowVariant = 'trunk' | 'leaf'

export function condensedNavigatorRow(
  entries: Array<NavigatorEntry>,
  variant: 'trunk' | 'leaf',
  indentation: number,
): CondensedNavigatorRow {
  return {
    type: 'condensed-row',
    variant: variant,
    entries: entries,
    indentation: indentation,
  }
}

export function isCondensedNavigatorRow(row: NavigatorRow): row is CondensedNavigatorRow {
  return row.type === 'condensed-row'
}

export function getEntriesForRow(row: NavigatorRow): Array<NavigatorEntry> {
  if (isRegulaNavigatorRow(row)) {
    return [row.entry]
  } else {
    return row.entries
  }
}
