import { vercelStegaCombine, vercelStegaDecodeAll } from '@vercel/stega'
import { EditorAction } from '../../components/editor/action-types'

export interface SteganoTextData {
  originalString: string
  filePath: string
  startPosition: number
  endPosition: number
}

export function steganoTextData(
  originalString: string,
  filePath: string,
  startPosition: number,
  endPosition: number,
): SteganoTextData {
  return {
    originalString: originalString,
    filePath: filePath,
    startPosition: startPosition,
    endPosition: endPosition,
  }
}

export function encodeSteganoData(originalString: string, data: SteganoTextData): string {
  return vercelStegaCombine(originalString, data)
}

export function decodeSteganoData(encodedString: string): Array<SteganoTextData> {
  return vercelStegaDecodeAll(encodedString)
}
