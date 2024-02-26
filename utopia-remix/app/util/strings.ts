export const UnknownPlayerName = 'Unknown'

export function multiplayerInitialsFromName(name: string | null): string {
  const nameOrUnknown = name ?? UnknownPlayerName
  const baseName = nameOrUnknown.trim().toUpperCase()

  const words = baseName.split(/\s+/)
  if (words.length >= 2) {
    return words[0].charAt(0) + words[1].charAt(0)
  } else if (baseName.length > 1) {
    return baseName.charAt(0) + baseName.charAt(1)
  } else {
    return baseName.padEnd(2, 'X')
  }
}
