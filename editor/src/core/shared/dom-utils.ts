import { ReactDOM } from 'react'
import { canvasRectangle, CanvasRectangle, roundToNearestHalf, scaleRect } from './math-utils'
import { URL_HASH } from '../../common/env-vars'

export const intrinsicHTMLElementNames: Array<keyof ReactDOM> = [
  'a',
  'abbr',
  'address',
  'area',
  'article',
  'aside',
  'audio',
  'b',
  'base',
  'bdi',
  'bdo',
  'big',
  'blockquote',
  'body',
  'br',
  'button',
  'canvas',
  'caption',
  'cite',
  'code',
  'col',
  'colgroup',
  'data',
  'datalist',
  'dd',
  'del',
  'details',
  'dfn',
  'dialog',
  'div',
  'dl',
  'dt',
  'em',
  'embed',
  'fieldset',
  'figcaption',
  'figure',
  'footer',
  'form',
  'h1',
  'h2',
  'h3',
  'h4',
  'h5',
  'h6',
  'head',
  'header',
  'hgroup',
  'hr',
  'html',
  'i',
  'iframe',
  'img',
  'input',
  'ins',
  'kbd',
  'keygen',
  'label',
  'legend',
  'li',
  'link',
  'main',
  'map',
  'mark',
  'menu',
  'menuitem',
  'meta',
  'meter',
  'nav',
  'noscript',
  'object',
  'ol',
  'optgroup',
  'option',
  'output',
  'p',
  'param',
  'picture',
  'pre',
  'progress',
  'q',
  'rp',
  'rt',
  'ruby',
  's',
  'samp',
  'script',
  'section',
  'select',
  'small',
  'source',
  'span',
  'strong',
  'style',
  'sub',
  'summary',
  'sup',
  'table',
  'template',
  'tbody',
  'td',
  'textarea',
  'tfoot',
  'th',
  'thead',
  'time',
  'title',
  'tr',
  'track',
  'u',
  'ul',
  'video',
  'wbr',
  'webview',
  'animate',
  'circle',
  'clipPath',
  'defs',
  'desc',
  'ellipse',
  'feBlend',
  'feColorMatrix',
  'feComponentTransfer',
  'feComposite',
  'feConvolveMatrix',
  'feDiffuseLighting',
  'feDisplacementMap',
  'feDistantLight',
  'feDropShadow',
  'feFlood',
  'feFuncA',
  'feFuncB',
  'feFuncG',
  'feFuncR',
  'feGaussianBlur',
  'feImage',
  'feMerge',
  'feMergeNode',
  'feMorphology',
  'feOffset',
  'fePointLight',
  'feSpecularLighting',
  'feSpotLight',
  'feTile',
  'feTurbulence',
  'filter',
  'foreignObject',
  'g',
  'image',
  'line',
  'linearGradient',
  'marker',
  'mask',
  'metadata',
  'path',
  'pattern',
  'polygon',
  'polyline',
  'radialGradient',
  'rect',
  'stop',
  'svg',
  'switch',
  'symbol',
  'text',
  'textPath',
  'tspan',
  'use',
  'view',
]

export const intrinsicHTMLElementNamesAsStrings: Array<string> = intrinsicHTMLElementNames
export const intrinsicHTMLElementNamesThatSupportChildren: Array<string> = [
  // Based on a elements from https://developer.mozilla.org/en-US/docs/Web/Guide/HTML/Content_categories#Flow_content
  // which also supports flow content
  'a',
  'address',
  'article',
  'aside',
  'b',
  'bdo',
  'bdi',
  'blockquote',
  'button',
  'details',
  'dfn',
  'div',
  'dl',
  'fieldset',
  'figure',
  'footer',
  'form',
  'header',
  'main',
  'menu',
  'nav',
  'section',
]

export function getDOMAttribute(element: Element, attributeName: string): string | null {
  const attr = element.attributes.getNamedItemNS(null, attributeName)
  if (attr == null) {
    return null
  } else {
    return attr.value
  }
}

export function setDOMAttribute(element: Element, attributeName: string, value: string): void {
  const attr = document.createAttributeNS(null, attributeName)
  attr.value = value
  element.attributes.setNamedItemNS(attr)
}

export function getCanvasRectangleFromElement(
  element: HTMLElement,
  canvasScale: number,
): CanvasRectangle {
  const boundingRect = element.getBoundingClientRect()

  // canvas container uses scale for <1 zoom level, it should not affect the frame of the element.
  const scale = canvasScale < 1 ? 1 / canvasScale : 1
  return scaleRect(
    canvasRectangle({
      x: roundToNearestHalf(boundingRect.left),
      y: roundToNearestHalf(boundingRect.top),
      width: roundToNearestHalf(boundingRect.width),
      height: roundToNearestHalf(boundingRect.height),
    }),
    scale,
  )
}

export function addStyleSheetToPage(url: string, shouldAppendHash: boolean = true) {
  const cssElement = document.createElement('link')
  cssElement.rel = 'stylesheet'
  cssElement.type = 'text/css'
  cssElement.href = shouldAppendHash ? appendHash(url) : url
  document.getElementsByTagName('head')[0].appendChild(cssElement)
}

function appendHash(url: string): string {
  return `${url}?hash=${URL_HASH}`
}

export function addScriptToPage(
  url: string,
  checksum: string | null = null,
  shouldAppendHash: boolean = true,
) {
  const script = document.createElement('script')
  script.src = shouldAppendHash ? appendHash(url) : url
  script.async = true
  script.crossOrigin = 'anonymous'
  if (checksum != null) {
    script.integrity = checksum
  }
  document.body.appendChild(script)
}
