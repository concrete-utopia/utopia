export interface CursorPosition {
  line: number
  column: number
}

export function cursorPositionsEqual(l: CursorPosition, r: CursorPosition): boolean {
  return l.line === r.line && l.column === r.column
}

export function rawOffsetToCursorPosition(code: string, rawOffset: number): CursorPosition {
  const lines = code.split(/\r?\n/)
  let lineFound = false
  let lineNumber = 1 // Line and column aren't zero-based
  let column = 0
  let currentOffset = 0
  while (!lineFound && lineNumber <= lines.length) {
    const line = lines[lineNumber - 1]
    if (rawOffset > currentOffset + line.length) {
      currentOffset += line.length + 1 // + 1 for each new line
      lineNumber++
    } else {
      column = rawOffset + 1 - currentOffset
      lineFound = true
    }
  }

  return {
    line: lineNumber,
    column: column,
  }
}

export function cursorPositionToRawOffset(code: string, position: CursorPosition): number {
  const lines = code.split(/\r?\n/)
  let rawOffset = position.column - 1
  for (let i = 0; i < position.line - 1; i++) {
    const line = lines[i]
    rawOffset += line.length + 1 // + 1 for each new line
  }

  return rawOffset
}
