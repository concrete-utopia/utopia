import { CopyData } from './clipboard'
import { Size } from '../core/shared/math-utils'

export interface ImageResult {
  type: 'IMAGE_RESULT'
  filename: string
  dataUrl: string
  size: Size
  fileType: string
}

export interface AssetResult {
  type: 'ASSET_RESULT'
  filename: string
  dataUrl: string
}

export interface TextResult {
  type: 'TEXT_RESULT'
  filename: string
  content: string
}

export type FileResult = ImageResult | AssetResult | TextResult

export interface PasteResult {
  utopiaData: CopyData[]
  files: Array<FileResult>
}

export async function parsePasteEvent(clipboardData: DataTransfer | null): Promise<PasteResult> {
  if (clipboardData == null) {
    return {
      files: [],
      utopiaData: [],
    }
  }
  const utopiaData = extractUtopiaDataFromClipboardData(clipboardData)
  if (utopiaData.length > 0) {
    return {
      files: [],
      utopiaData: utopiaData,
    }
  } else {
    const items = clipboardData.items
    const imageArray = await extractFiles(items)
    return {
      files: imageArray,
      utopiaData: [],
    }
  }
}

function extractFiles(items: DataTransferItemList): Promise<Array<FileResult>> {
  const fileItems = Array.from(items).filter((item) => item.kind === 'file')
  return Promise.all<FileResult>(
    fileItems.map((item) => {
      const file = item.getAsFile()
      if (file == null) {
        return Promise.reject('Could not extract file.')
      } else {
        if (item.type.startsWith('text/')) {
          return extractText(file)
        } else if (item.type.startsWith('image/')) {
          return extractImage(file)
        } else {
          return extractAsset(file)
        }
      }
    }),
  )
}

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

export function extractImage(file: File): Promise<ImageResult> {
  return new Promise((resolve, reject) => {
    const reader = new FileReader()
    reader.onload = async () => {
      const result: string = reader.result as string
      const imageSize = await getImageSize(result)
      resolve({
        type: 'IMAGE_RESULT',
        filename: file.name,
        dataUrl: result,
        size: imageSize,
        fileType: file.type,
      })
    }
    reader.onerror = (error) => {
      reject(error)
    }
    reader.readAsDataURL(file)
  })
}

export function extractAsset(file: File): Promise<AssetResult> {
  return new Promise((resolve, reject) => {
    const reader = new FileReader()
    reader.onload = async () => {
      const result: string = reader.result as string
      resolve({
        type: 'ASSET_RESULT',
        filename: file.name,
        dataUrl: result,
      })
    }
    reader.onerror = (error) => {
      reject(error)
    }
    reader.readAsDataURL(file)
  })
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

function filterNone(node: Node) {
  return NodeFilter.FILTER_ACCEPT
}

const UtopiaDataPrefix = '(utopia)'
const UtopiaPrefixLength = UtopiaDataPrefix.length
const UtopiaDataPostfix = '(/utopia)'
const UtopiaPostfixLength = UtopiaDataPostfix.length

function extractUtopiaDataFromClipboardData(data: DataTransfer): Array<CopyData> {
  const htmlString = data.getData('text/html')
  if (htmlString !== '') {
    return extractUtopiaDataFromHtml(htmlString)
  } else {
    return []
  }
}

function extractUtopiaDataFromHtml(htmlString: string): Array<CopyData> {
  const comments: string[] = []
  // parse them html
  const htmlElement = document.createElement('html')
  htmlElement.innerHTML = htmlString

  // extract comments
  const iterator = document.createNodeIterator(
    htmlElement,
    NodeFilter.SHOW_COMMENT,
    filterNone as any,
  )
  let currentNode: Node | null
  // tslint:disable-next-line:no-conditional-assignment
  while ((currentNode = iterator.nextNode())) {
    if (currentNode != null && currentNode.nodeValue) {
      comments.push(currentNode.nodeValue)
    }
  }

  // parse comments, look for Utopia Data
  // HACK we only take the first comment! (we are assuming there's only one comment)
  const utopiaDataString = comments
    .filter((comment) => comment.indexOf('(utopia)') === 0)
    .map((utopiaComment) =>
      utopiaComment.substring(UtopiaPrefixLength, utopiaComment.length - UtopiaPostfixLength),
    )[0]
  if (utopiaDataString != null) {
    try {
      const decodedString = decodeURIComponent(utopiaDataString)
      const utopiaData = JSON.parse(decodedString)
      return utopiaData
    } catch (e) {
      console.error('error parsing pasted JSON', e)
    }
  }
  return []
}

export function encodeUtopiaDataToHtml(data: Array<CopyData>): string {
  const utopiaDataString = JSON.stringify(data)
  const encodedData = encodeURIComponent(utopiaDataString)
  const htmlWithData = `<meta charset="utf-8"><!--${UtopiaDataPrefix}${encodedData}${UtopiaDataPostfix}-->`
  return htmlWithData
}
