import { vercelStegaCombine, vercelStegaDecode, vercelStegaSplit } from '@vercel/stega'
import type { SteganographyMode } from '../workers/parser-printer/parser-printer'
import { isFeatureEnabled } from '../../utils/feature-switches'

export type SteganoData = SteganoTextData | SteganoDataFromCMS

export interface SteganoTextData {
  type: 'defined-in-source'
  filePath: string
  startPosition: number
  endPosition: number
  originalString: string
}

// TODO: this is very much tailored to Jurassic
export interface SteganoDataFromCMS {
  type: 'from-cms'
  project_id: string
  key: string
}

export function encodeSteganoTextData(text: string, data: SteganoTextData): string {
  const { cleaned } = vercelStegaSplit(text)
  return vercelStegaCombine(cleaned, data)
}

function isStegoObject(data: unknown): data is Record<string, unknown> {
  return typeof data === 'object' && data != null
}

function decodeSteganoDataFromCMS(encodedString: string): SteganoDataFromCMS | null {
  const data = vercelStegaDecode(encodedString)
  if (!isStegoObject(data) || data.key == null) {
    return null
  }

  return {
    type: 'from-cms',
    project_id: data.project_id as string,
    key: data.key as string,
  }
}

function decodeSteganoTextData(encodedString: string): SteganoTextData | null {
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
    type: 'defined-in-source',
    filePath: data['filePath'] as string,
    startPosition: data['startPosition'] as number,
    endPosition: data['endPosition'] as number,
    originalString: data['originalString'] as string,
  }

  return steganoData
}

export function decodeSteganoData(encodedString: string): SteganoData | null {
  return decodeSteganoTextData(encodedString) ?? decodeSteganoDataFromCMS(encodedString)
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
