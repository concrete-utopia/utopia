import { vercelStegaCombine, vercelStegaDecode, vercelStegaSplit } from '@vercel/stega'
import type { SteganographyMode } from '../workers/parser-printer/parser-printer'
import { STEGANOGRAPHY_ENABLED } from '../../utils/feature-switches'

export interface SteganoTextData {
  filePath: string
  startPosition: number
  endPosition: number
  originalString: string
}

export function encodeSteganoData(text: string, data: SteganoTextData): string {
  const { cleaned } = vercelStegaSplit(text)
  return vercelStegaCombine(cleaned, data)
}
function isStegoObject(data: unknown): data is Partial<SteganoTextData> {
  return typeof data === 'object' && data != null
}

export function decodeSteganoData(encodedString: string): SteganoTextData | null {
  const data = vercelStegaDecode(encodedString)
  if (
    !isStegoObject(data) ||
    data.endPosition == null ||
    data.filePath == null ||
    data.startPosition == null ||
    data.originalString == null
  ) {
    return null
  }

  const steganoData: SteganoTextData = {
    filePath: data['filePath'],
    startPosition: data['startPosition'],
    endPosition: data['endPosition'],
    originalString: data['originalString'],
  }

  return steganoData
}

export function cleanSteganoTextData(text: string): { cleaned: string } {
  const { cleaned } = vercelStegaSplit(text)
  return { cleaned }
}

export function isSteganographyEnabled(): SteganographyMode {
  if (STEGANOGRAPHY_ENABLED) {
    return 'apply-steganography'
  }
  return 'do-not-apply-steganography'
}
