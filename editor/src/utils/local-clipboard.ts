import { CopyData } from './clipboard'
import { PasteResult } from './clipboard-utils'

let _localClipboard: Array<CopyData> = []

export function setLocalClipboardData(
  copyData: {
    data: Array<CopyData>
    plaintext: string
    imageFilenames: Array<string>
  } | null,
): void {
  if (copyData != null) {
    _localClipboard = copyData.data
  } else {
    _localClipboard = []
  }
}

export function getLocalClipboardData(): PasteResult {
  return {
    utopiaData: _localClipboard,
    files: [], // local image copy is not supported yet, as it is not needed for the Style Clipboard
  }
}
