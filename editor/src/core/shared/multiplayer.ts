import type { User } from '@liveblocks/client'
import type { Presence, UserMeta } from '../../../liveblocks.config'
import { safeIndex, uniqBy } from './array-utils'
import { getPreferredColorScheme } from '../../uuiui'

export type MultiplayerColor = {
  background: string
  foreground: string
}

export const multiplayerColors = {
  light: [
    { background: '#0092C0', foreground: 'white' },
    { background: '#128400', foreground: 'white' },
    { background: '#2A00D1', foreground: 'white' },
    { background: '#5C00D1', foreground: 'white' },
    { background: '#87C700', foreground: 'black' },
    { background: '#8C6D00', foreground: 'white' },
    { background: '#9C0000', foreground: 'white' },
    { background: '#B16CB7', foreground: 'white' },
    { background: '#B60479', foreground: 'white' },
    { background: '#E7B400', foreground: 'black' },
  ],
  dark: [
    { background: '#007DA4', foreground: 'white' },
    { background: '#19B500', foreground: 'black' },
    { background: '#3EFFF3', foreground: 'black' },
    { background: '#6842FF', foreground: 'white' },
    { background: '#C4FF46', foreground: 'black' },
    { background: '#CE00D3', foreground: 'white' },
    { background: '#DD0000', foreground: 'white' },
    { background: '#FF00A8', foreground: 'white' },
    { background: '#FFBA08', foreground: 'black' },
    { background: '#FFFF00', foreground: 'black' },
  ],
}

export function multiplayerColorFromIndex(colorIndex: number | null): MultiplayerColor {
  const defaultColor = { background: '#000', foreground: '#fff' }
  if (colorIndex == null) {
    return defaultColor
  }
  return (
    (getPreferredColorScheme() === 'dark'
      ? safeIndex(multiplayerColors.dark, colorIndex)
      : safeIndex(multiplayerColors.light, colorIndex)) ?? defaultColor
  )
}

/**
 * Returns the initials from the given name, to be displayed on e.g. avatars or comments.
 * If the name is made of multiple words, the first letter of the first two words will be returned ('John Doe' -> 'JD').
 * If the name is made of a single word, the first two letters of the word will be returned ('John' -> 'JO').
 * If the name is shorter than two letters, the result will be padded with 'X' characters ('F' -> 'FX').
 */
export function multiplayerInitialsFromName(name: string | null): string {
  const trimmed = (name ?? 'unknown').trim().toUpperCase()
  const words = trimmed.split(/\s+/)
  if (words.length >= 2) {
    return words[0].charAt(0) + words[1].charAt(0)
  } else if (trimmed.length > 1) {
    return trimmed.charAt(0) + trimmed.charAt(1)
  } else {
    return trimmed.padEnd(2, 'X')
  }
}

export function normalizeMultiplayerName(name: string | null): string {
  if (name == null) {
    return 'Unknown'
  }
  return name.trim().replace(/@.+$/, '')
}

export function normalizeOthersList(
  selfId: string,
  others: readonly User<Presence, UserMeta>[],
): User<Presence, UserMeta>[] {
  return uniqBy(
    others.filter((other) => other.id !== selfId),
    (a, b) => a.id === b.id,
  )
}
