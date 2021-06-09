import * as stringHash from 'string-hash'
import { size, Size } from './math-utils'

export interface ImageResult {
  type: 'IMAGE_RESULT'
  filename: string
  base64Bytes: string
  size: Size
  fileType: string
  hash: number
}

export interface AssetResult {
  type: 'ASSET_RESULT'
  filename: string
  base64Bytes: string
  hash: number
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

function getMimeStrippedBase64(base64: string): string {
  const splitBase64 = base64.split(',')
  switch (splitBase64.length) {
    case 1:
      // No mime prefix.
      return base64
    case 2:
      // Mime prefix.
      return splitBase64[1]
    default:
      throw new Error('Invalid Base64 content for asset.')
  }
}

export function assetResultForBase64(filename: string, base64: string): AssetResult {
  const mimeStrippedBase64 = getMimeStrippedBase64(base64)
  const hash = stringHash(mimeStrippedBase64)
  return {
    type: 'ASSET_RESULT',
    filename: filename,
    base64Bytes: mimeStrippedBase64,
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
  const mimeStrippedBase64 = getMimeStrippedBase64(base64)
  const hash = stringHash(mimeStrippedBase64)
  let imageDataUrl: string
  if (mimeStrippedBase64 === base64) {
    imageDataUrl = `data:;base64,${base64}`
  } else {
    imageDataUrl = base64
  }
  let imageSize: Size
  if (fileType === '.svg') {
    // FIXME: SVGs may not have a viewbox or it may be specified
    // as a ratio, so put in a token value for now.
    imageSize = size(100, 100)
  } else {
    imageSize = await getImageSize(imageDataUrl)
  }
  return {
    type: 'IMAGE_RESULT',
    filename: filename,
    base64Bytes: mimeStrippedBase64,
    size: imageSize,
    fileType: fileType,
    hash: hash,
  }
}

export function svgToBase64(rawSVG: string): string {
  return `data:image/svg+xml;base64,${btoa(rawSVG)}`
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
    image.onerror = (error) => {
      if (typeof error === 'string') {
        reject(error)
      } else if (error instanceof ErrorEvent) {
        reject(error.message)
      } else {
        console.error('Image failure.', error)
        reject('Failed for unknown reason.')
      }
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
