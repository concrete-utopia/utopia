import * as stringHash from 'string-hash'
import { Size } from './math-utils'

export interface ImageResult {
  type: 'IMAGE_RESULT'
  filename: string
  dataUrl: string
  size: Size
  fileType: string
  hash: string
}

export interface AssetResult {
  type: 'ASSET_RESULT'
  filename: string
  dataUrl: string
  hash: string
}

export interface TextResult {
  type: 'TEXT_RESULT'
  filename: string
  content: string
}

export type FileResult = ImageResult | AssetResult | TextResult

export function extractText(file: File): Promise<TextResult> {
  return new Promise((resolve, reject) => {
    const reader = new FileReader()
    reader.onload = async () => {
      const result: string = reader.result as string
      resolve({
        type: 'TEXT_RESULT',
        filename: file.name,
        content: result,
      })
    }
    reader.onerror = (error) => {
      reject(error)
    }
    reader.readAsText(file)
  })
}

export function extractAsset(file: File): Promise<AssetResult> {
  return new Promise((resolve, reject) => {
    const reader = new FileReader()
    reader.onload = async () => {
      const result = assetResultForBase64(file.name, reader.result as string)
      resolve(result)
    }
    reader.onerror = (error) => {
      reject(error)
    }
    reader.readAsDataURL(file)
  })
}

export function assetResultForBase64(filename: string, base64: string): AssetResult {
  const mimeStrippedBase64 = base64.split(',')[1]
  const hash = stringHash(mimeStrippedBase64)
  return {
    type: 'ASSET_RESULT',
    filename: filename,
    dataUrl: mimeStrippedBase64,
    hash: hash,
  }
}

export function extractImage(file: File): Promise<ImageResult> {
  return new Promise((resolve, reject) => {
    const reader = new FileReader()
    reader.onload = async () => {
      const result = await imageResultForBase64(file.name, file.type, reader.result as string)
      resolve(result)
    }
    reader.onerror = (error) => {
      reject(error)
    }
    reader.readAsDataURL(file)
  })
}

export async function imageResultForBase64(
  filename: string,
  fileType: string,
  base64: string,
): Promise<ImageResult> {
  const mimeStrippedBase64 = base64.split(',')[1]
  const hash = stringHash(mimeStrippedBase64)
  const imageSize = await getImageSize(base64)
  return {
    type: 'IMAGE_RESULT',
    filename: filename,
    dataUrl: mimeStrippedBase64,
    size: imageSize,
    fileType: fileType,
    hash: hash,
  }
}

function getImageSize(imageDataUrl: string): Promise<Size> {
  return new Promise((resolve, reject) => {
    const image = new Image()
    image.onload = () => {
      resolve({
        width: image.naturalWidth,
        height: image.naturalHeight,
      })
    }
    image.src = imageDataUrl
  })
}

export function dropFileExtension(filename: string): string {
  const lastDotIndex = filename.lastIndexOf('.')
  if (lastDotIndex > 0) {
    return filename.slice(0, lastDotIndex)
  } else {
    return filename
  }
}

export function getFileExtension(filename: string): string {
  const lastDotIndex = filename.lastIndexOf('.')
  if (lastDotIndex > 0) {
    return filename.slice(lastDotIndex)
  } else {
    return '' // TODO How do we handle files with no extension?
  }
}
