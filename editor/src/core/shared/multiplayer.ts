import type { User } from '@liveblocks/client'
import type { Presence, UserMeta } from '../../../liveblocks.config'
import { possiblyUniqueInArray, safeIndex, uniqBy } from './array-utils'
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

function randomMultiplayerColor(): number {
  return Math.floor(Math.random() * multiplayerColors.light.length)
}

export function possiblyUniqueColor(existing: (number | null)[]): number {
  return possiblyUniqueInArray(
    multiplayerColors.light.map((_, index) => index),
    existing,
    randomMultiplayerColor(),
  )
}

export function multiplayerColorFromIndex(colorIndex: number | null): MultiplayerColor {
  const fallbackColor = {
    background: '#000',
    foreground: '#fff',
  }
  if (colorIndex == null) {
    return fallbackColor
  }

  const colors =
    getPreferredColorScheme() === 'dark' ? multiplayerColors.dark : multiplayerColors.light
  return safeIndex(colors, colorIndex) ?? fallbackColor
}

/**
 * Returns the initials from the given name, to be displayed on e.g. avatars or comments.
 * If the name is made of multiple words, the first letter of the first two words will be returned ('John Doe' -> 'JD').
 * If the name is made of a single word, the first two letters of the word will be returned ('John' -> 'JO').
 * If the name is shorter than two letters, the result will be padded with 'X' characters ('F' -> 'FX').
 */
export function multiplayerInitialsFromName(name: string): string {
  const baseName = name.trim().toUpperCase()

  const words = baseName.split(/\s+/)
  if (words.length >= 2) {
    return words[0].charAt(0) + words[1].charAt(0)
  } else if (baseName.length > 1) {
    return baseName.charAt(0) + baseName.charAt(1)
  } else {
    return baseName.padEnd(2, 'X')
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
