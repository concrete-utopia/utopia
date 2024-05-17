import type { NavigatorEntry } from '../editor/store/editor-state'

export type NavigatorRow = RegularNavigatorRow | CondensedNavigatorRow

export interface RegularNavigatorRow {
  type: 'regular-row'
  entry: NavigatorEntry
}

export interface CondensedNavigatorRow {
  type: 'condensed-row'
  entries: Array<NavigatorEntry>
}

export function isRegulaNavigatorrRow(row: NavigatorRow): row is RegularNavigatorRow {
  return row.type === 'regular-row'
}

export function getEntriesForRow(row: NavigatorRow): Array<NavigatorEntry> {
  if (isRegulaNavigatorrRow(row)) {
    return [row.entry]
  } else {
    return row.entries
  }
}

type RowWithIndentation<R extends NavigatorRow> = R & { indentation: number }
