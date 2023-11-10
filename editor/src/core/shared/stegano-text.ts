import { vercelStegaCombine, vercelStegaDecode, vercelStegaSplit } from '@vercel/stega'

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

  const { originalString, filePath, startPosition, endPosition } = data
  const dataToEncode = {
    originalString: originalString,
    filePath: filePath,
    startPosition: `${startPosition}`,
    endPosition: `${endPosition}`,
  }
  return vercelStegaCombine(cleaned, dataToEncode)
}

export function decodeSteganoData(encodedString: string): Array<SteganoTextData> {
  const data = vercelStegaDecode(encodedString)
  if (!(typeof data === 'object')) {
    return []
  }
  const steganoData: SteganoTextData = {
    originalString: data['originalString'],
    filePath: data['filePath'],
    startPosition: parseInt(data['startPosition']),
    endPosition: parseInt(data['endPosition']),
  }

  return [steganoData]
}
