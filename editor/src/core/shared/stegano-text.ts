import { vercelStegaCombine, vercelStegaDecode, vercelStegaSplit } from '@vercel/stega'
import type { SteganographyMode } from '../workers/parser-printer/parser-printer'
import { isFeatureEnabled } from '../../utils/feature-switches'

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
    data.originalString == null ||
    data.startPosition == null
  ) {
    return null
  }

  const steganoData: SteganoTextData = {
    originalString: data['originalString'],
    filePath: data['filePath'],
    startPosition: data['startPosition'],
    endPosition: data['endPosition'],
  }

  return steganoData
}

export function cleanSteganoTextData(text: string): { cleaned: string } {
  const { cleaned } = vercelStegaSplit(text)
  return { cleaned }
}

export function isSteganographyEnabled(): SteganographyMode {
  if (isFeatureEnabled('Steganography')) {
    return 'apply-steganography'
  }
  return 'do-not-apply-steganography'
}
