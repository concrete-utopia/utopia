import stringHash from 'string-hash'
import type { Size } from './math-utils'
import { size } from './math-utils'

export interface ImageResult {
  type: 'IMAGE_RESULT'
  filename: string
  base64Bytes: string
  size: Size
  fileType: string
  hash: number
}

export function imageResult(
  filename: string,
  base64Bytes: string,
  imageSize: Size,
  fileType: string,
  hash: number,
): ImageResult {
  return {
    type: 'IMAGE_RESULT',
    filename: filename,
    base64Bytes: base64Bytes,
    size: imageSize,
    fileType: fileType,
    hash: hash,
  }
}

export interface AssetResult {
  type: 'ASSET_RESULT'
  filename: string
  base64Bytes: string
  hash: number
}

export function assetResult(filename: string, base64Bytes: string, hash: number): AssetResult {
  return {
    type: 'ASSET_RESULT',
    filename: filename,
    base64Bytes: base64Bytes,
    hash: hash,
  }
}

export interface TextResult {
  type: 'TEXT_RESULT'
  filename: string
  content: string
}

export function textResult(filename: string, content: string): TextResult {
  return {
    type: 'TEXT_RESULT',
    filename: filename,
    content: content,
  }
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
  const hash = stringHash(base64)
  return {
    type: 'ASSET_RESULT',
    filename: filename,
    base64Bytes: base64,
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
  const hash = stringHash(base64)
  const imageDataUrl: string = base64
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
    base64Bytes: base64,
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

export function isTsFile(filename: string): boolean {
  return filename.endsWith('.ts') || filename.endsWith('.tsx')
}

export function isJsFile(filename: string): boolean {
  return filename.endsWith('.js') || filename.endsWith('.jsx')
}

export function isCssFile(filename: string): boolean {
  return filename.endsWith('.css')
}

export function isJsOrTsFile(filename: string): boolean {
  return isJsFile(filename) || isTsFile(filename)
}
