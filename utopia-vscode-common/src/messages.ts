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

export type DecorationRangeType = 'selection' | 'highlight'

export interface DecorationRange {
  rangeType: DecorationRangeType
  filePath: string
  startLine: number
  startCol: number
  endLine: number
  endCol: number
}

export function decorationRange(
  rangeType: DecorationRangeType,
  filePath: string,
  startLine: number,
  startCol: number,
  endLine: number,
  endCol: number,
): DecorationRange {
  return {
    rangeType: rangeType,
    filePath: filePath,
    startLine: startLine,
    startCol: startCol,
    endLine: endLine,
    endCol: endCol,
  }
}

export interface UpdateDecorationsMessage {
  type: 'UPDATE_DECORATIONS'
  decorations: Array<DecorationRange>
}

export function updateDecorationsMessage(
  decorations: Array<DecorationRange>,
): UpdateDecorationsMessage {
  return {
    type: 'UPDATE_DECORATIONS',
    decorations: decorations,
  }
}

export type UtopiaVSCodeMessage = OpenFileMessage | UpdateDecorationsMessage

export function isOpenFileMessage(message: unknown): message is OpenFileMessage {
  return (
    typeof message === 'object' && !Array.isArray(message) && (message as any).type === 'OPEN_FILE'
  )
}

export function isUpdateDecorationsMessage(message: unknown): message is UpdateDecorationsMessage {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as any).type === 'UPDATE_DECORATIONS'
  )
}

export function parseMessage(unparsed: string): UtopiaVSCodeMessage {
  const message = JSON.parse(unparsed)
  if (isOpenFileMessage(message) || isUpdateDecorationsMessage(message)) {
    return message
  } else {
    // FIXME This should return an Either
    throw new Error(`Invalid message type ${JSON.stringify(message)}`)
  }
}
