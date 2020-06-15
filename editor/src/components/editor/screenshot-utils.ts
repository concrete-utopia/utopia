import * as TP from '../../core/shared/template-path'
import { TemplatePath } from '../../core/shared/project-file-types'

const domtoimage = require('domtoimage')

const BASE64_PREFIX = 'data:image/png;base64,'

type Dom2ImageOptions = { width?: number; height?: number }

let ScreenshotByPath: { [path: string]: string } = {}

export function captureScreenshotForPath(path: TemplatePath, element: HTMLElement | undefined) {
  delete ScreenshotByPath[TP.toString(path)] // Clear the old one
  if (element != null) {
    getPNGOfElement(element).then((png) => {
      if (png != null) {
        ScreenshotByPath[TP.toString(path)] = png
      }
    })
  }
}

export function getScreenshotForPath(path: TemplatePath): string | undefined {
  return ScreenshotByPath[TP.toString(path)]
}

export async function getPNGOfElement(
  element: HTMLElement,
  options: Dom2ImageOptions = {},
): Promise<string | null> {
  return domtoimage.toPng(element, options)
}

export async function getPNGOfElementWithID(
  elementID: string,
  options: Dom2ImageOptions = {},
): Promise<string | null> {
  const domNode = document.getElementById(elementID)
  if (domNode == null) {
    return Promise.resolve(null)
  } else {
    return getPNGOfElement(domNode, options)
  }
}

export async function getPNGBufferOfElementWithID(
  elementID: string,
  options: Dom2ImageOptions = {},
): Promise<Buffer | null> {
  const png = await getPNGOfElementWithID(elementID, options)
  if (png == null) {
    return null
  } else {
    // Kill me now
    const stripped = png.replace(BASE64_PREFIX, '')
    const pngBuffer = Buffer.from(stripped, 'base64')
    return pngBuffer
  }
}
