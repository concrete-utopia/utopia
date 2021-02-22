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

export interface Bounds {
  startLine: number
  startCol: number
  endLine: number
  endCol: number
}

export interface BoundsInFile extends Bounds {
  filePath: string
}

export function boundsInFile(
  filePath: string,
  startLine: number,
  startCol: number,
  endLine: number,
  endCol: number,
): BoundsInFile {
  return {
    filePath: filePath,
    startLine: startLine,
    startCol: startCol,
    endLine: endLine,
    endCol: endCol,
  }
}

export interface DecorationRange extends BoundsInFile {
  rangeType: DecorationRangeType
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

export interface SelectedElementChanged {
  type: 'SELECTED_ELEMENT_CHANGED'
  boundsInFile: BoundsInFile
}

export function selectedElementChanged(boundsInFile: BoundsInFile): SelectedElementChanged {
  return {
    type: 'SELECTED_ELEMENT_CHANGED',
    boundsInFile: boundsInFile
  }
}

export type UtopiaVSCodeMessage = OpenFileMessage | UpdateDecorationsMessage | SelectedElementChanged

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

export function isSelectedElementChanged(message: unknown): message is SelectedElementChanged {
  return (
    typeof message === 'object' &&
    !Array.isArray(message) &&
    (message as any).type === 'SELECTED_ELEMENT_CHANGED'
  )
}

export function parseMessage(unparsed: string): UtopiaVSCodeMessage {
  const message = JSON.parse(unparsed)
  if (isOpenFileMessage(message) || isUpdateDecorationsMessage(message) || isSelectedElementChanged(message)) {
    return message
  } else {
    // FIXME This should return an Either
    throw new Error(`Invalid message type ${JSON.stringify(message)}`)
  }
}
