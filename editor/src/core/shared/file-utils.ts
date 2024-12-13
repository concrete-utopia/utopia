import { memoize } from './memoize'
import { sha1 } from 'sha.js'
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
  gitBlobSha: string
}

export function imageResult(
  filename: string,
  base64Bytes: string,
  imageSize: Size,
  fileType: string,
  hash: number,
  gitBlobSha: string,
): ImageResult {
  return {
    type: 'IMAGE_RESULT',
    filename: filename,
    base64Bytes: base64Bytes,
    size: imageSize,
    fileType: fileType,
    hash: hash,
    gitBlobSha: gitBlobSha,
  }
}

export interface AssetResult {
  type: 'ASSET_RESULT'
  filename: string
  base64Bytes: string
  hash: number
  gitBlobSha: string
}

export function assetResult(
  filename: string,
  base64Bytes: string,
  hash: number,
  gitBlobSha: string,
): AssetResult {
  return {
    type: 'ASSET_RESULT',
    filename: filename,
    base64Bytes: base64Bytes,
    hash: hash,
    gitBlobSha: gitBlobSha,
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

export async function extractAsset(file: File): Promise<AssetResult> {
  const base64ContentPromise: Promise<string> = new Promise((resolve, reject) => {
    const reader = new FileReader()
    reader.onload = async () => {
      resolve(reader.result as string)
    }
    reader.onerror = (error) => {
      reject(error)
    }
    reader.readAsDataURL(file)
  })

  return base64ContentPromise.then((base64String) => {
    return new Promise((resolve, reject) => {
      const reader = new FileReader()
      reader.onload = async () => {
        const result = assetResultForBase64(
          file.name,
          base64String,
          gitBlobChecksumFromBuffer(Buffer.from(reader.result as ArrayBuffer)),
        )
        resolve(result)
      }
      reader.onerror = (error) => {
        reject(error)
      }
      reader.readAsArrayBuffer(file)
    })
  })
}

export function assetResultForBase64(
  filename: string,
  base64: string,
  gitBlobSha: string,
): AssetResult {
  const hash = stringHash(base64)
  return {
    type: 'ASSET_RESULT',
    filename: filename,
    base64Bytes: base64,
    hash: hash,
    gitBlobSha: gitBlobSha,
  }
}

export async function extractImage(file: File): Promise<ImageResult> {
  const base64ContentPromise: Promise<string> = new Promise((resolve, reject) => {
    const reader = new FileReader()
    reader.onload = async () => {
      resolve(reader.result as string)
    }
    reader.onerror = (error) => {
      reject(error)
    }
    reader.readAsDataURL(file)
  })

  return base64ContentPromise.then((base64String) => {
    return new Promise((resolve, reject) => {
      const reader = new FileReader()
      reader.onload = async () => {
        const result = await imageResultForBase64(
          file.name,
          file.type,
          base64String,
          Buffer.from(reader.result as ArrayBuffer),
        )
        resolve(result)
      }
      reader.onerror = (error) => {
        reject(error)
      }
      reader.readAsArrayBuffer(file)
    })
  })
}

export async function imageResultForBase64(
  filename: string,
  fileType: string,
  base64: string,
  buffer: Buffer,
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
    gitBlobSha: gitBlobChecksumFromBuffer(buffer),
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
  return (
    filename.endsWith('.js') ||
    filename.endsWith('.jsx') ||
    filename.endsWith('.mjs') ||
    filename.endsWith('.cjs')
  )
}

export function isCssFile(filename: string): boolean {
  return filename.endsWith('.css')
}

export function isJsOrTsFile(filename: string): boolean {
  return isJsFile(filename) || isTsFile(filename)
}

export function isParseableFile(filename: string): boolean {
  return isJsOrTsFile(filename)
}

export function gitBlobChecksumFromBuffer(buffer: Buffer): string {
  // This function returns the same SHA1 checksum string that git would return for the same contents.
  // Given the contents in the buffer variable, the final checksum is calculated by hashing
  // a string built as "<prefix><contents>". The prefix looks like "blob <contents_length_in_bytes><null_character>".
  // Ref: https://git-scm.com/book/en/v2/Git-Internals-Git-Objects
  const prefix = Buffer.from(`blob ${buffer.byteLength}\0`)
  const wrapped = Buffer.concat([prefix, buffer])
  return getSHA1Checksum(wrapped)
}

function getSHA1ChecksumInner(contents: string | Buffer): string {
  return new sha1().update(contents).digest('hex')
}

// Memoized because it can be called for the same piece of code more than once before the
// checksum gets cached. For example in the canvas strategies and the regular dispatch flow, which don't share
// those cached checksum objects.
export const getSHA1Checksum = memoize(getSHA1ChecksumInner, {
  maxSize: 10,
  matchesArg: (first, second) => first === second,
})
