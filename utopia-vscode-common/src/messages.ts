export interface OpenFileMessage {
  type: 'OPEN_FILE'
  filePath: string
}

export function openFileMessage(filePath: string): OpenFileMessage {
  return {
    type: 'OPEN_FILE',
    filePath: filePath,
  }
}

export interface HighlightRange {
  startLine: number
  startCol: number
  endLine: number
  endCol: number
}

export interface SelectElementMessage {
  type: 'SELECT_ELEMENT'
  filePath: string
  range: HighlightRange
}

export function selectElementMessage(
  filePath: string,
  highlightRange: HighlightRange,
): SelectElementMessage {
  return {
    type: 'SELECT_ELEMENT',
    filePath: filePath,
    range: highlightRange,
  }
}

export interface HighlightElementMessage {
  type: 'HIGHLIGHT_ELEMENT'
  filePath: string
  range: HighlightRange
}

export function highlightElementMessage(
  filePath: string,
  highlightRange: HighlightRange,
): HighlightElementMessage {
  return {
    type: 'HIGHLIGHT_ELEMENT',
    filePath: filePath,
    range: highlightRange,
  }
}

export type UtopiaVSCodeMessage = OpenFileMessage | SelectElementMessage | HighlightElementMessage

export function isOpenFileMessage(message: unknown): message is OpenFileMessage {
  return (
    typeof message === 'object' && !Array.isArray(message) && (message as any).type === 'OPEN_FILE'
  )
}

export function isSelectElementMessage(message: unknown): message is SelectElementMessage {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as any).type === 'SELECT_ELEMENT'
  )
}

export function isHighlightElementMessage(message: unknown): message is HighlightElementMessage {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as any).type === 'HIGHLIGHT_ELEMENT'
  )
}

export function parseMessage(unparsed: string): UtopiaVSCodeMessage {
  const message = JSON.parse(unparsed)
  if (
    isOpenFileMessage(message) ||
    isSelectElementMessage(message) ||
    isHighlightElementMessage(message)
  ) {
    return message
  } else {
    // FIXME This should return an Either
    throw new Error(`Invalid message type ${JSON.stringify(message)}`)
  }
}
