import type { ReactDOM } from 'react'
import type { CanvasRectangle, MaybeInfinityCanvasRectangle } from './math-utils'
import {
  boundingRectangle,
  canvasRectangle,
  isNotNullFiniteRectangle,
  roundToNearestHalf,
  scaleRect,
} from './math-utils'
import { URL_HASH } from '../../common/env-vars'
import { blockLevelHtmlElements, inlineHtmlElements } from '../../utils/html-elements'
import { assertNever, identity } from './utils'
import type { HugProperty, HugPropertyWidthHeight } from './element-template'

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
  'span',
]

export const TextElements = ['text', 'p', 'span', 'h1', 'h2', 'h2', 'h3', 'h5', 'h6']

export const VoidElementsToFilter = ['br', 'wbr']

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

function getRoundingFn(rounding: 'nearest-half' | 'no-rounding') {
  switch (rounding) {
    case 'nearest-half':
      return roundToNearestHalf
    case 'no-rounding':
      return identity
    default:
      assertNever(rounding)
  }
}

export function getCanvasRectangleFromElement(
  element: HTMLElement,
  canvasScale: number,
  withContent: 'without-content' | 'with-content' | 'only-content',
  rounding: 'nearest-half' | 'no-rounding',
): CanvasRectangle {
  const scale = canvasScale < 1 ? 1 / canvasScale : 1

  const roundingFn = getRoundingFn(rounding)

  const domRectToScaledCanvasRectangle = (rect: DOMRect) => {
    // canvas container uses scale for <1 zoom level, it should not affect the frame of the element.
    return scaleRect(
      canvasRectangle({
        x: roundingFn(rect.left),
        y: roundingFn(rect.top),
        width: roundingFn(rect.width),
        height: roundingFn(rect.height),
      }),
      scale,
    )
  }

  const boundingRect = element.getBoundingClientRect()
  const elementRect = domRectToScaledCanvasRectangle(boundingRect)
  if (withContent === 'without-content') {
    return elementRect
  }

  const range = document.createRange()
  switch (withContent) {
    case 'only-content':
      range.selectNodeContents(element)
      break
    case 'with-content':
      range.selectNode(element)
      break
    default:
      assertNever(withContent)
  }
  const rangeBounding =
    // this is needed because jsdom can throw an error on the range.getBoundingClientRect() call, see https://github.com/jsdom/jsdom/issues/3002
    typeof range.getBoundingClientRect === 'function' ? range.getBoundingClientRect() : boundingRect
  const contentRect = domRectToScaledCanvasRectangle(rangeBounding)

  switch (withContent) {
    case 'only-content':
      return contentRect
    case 'with-content':
      return boundingRectangle(elementRect, contentRect)
    default:
      assertNever(withContent)
  }
}

export function addStyleSheetToPage(url: string, shouldAppendHash: boolean = true): void {
  const cssElement = document.createElement('link')
  cssElement.rel = 'stylesheet'
  cssElement.type = 'text/css'
  cssElement.href = shouldAppendHash ? appendHash(url) : url
  document.getElementsByTagName('head')[0].appendChild(cssElement)
}

export function appendHash(url: string): string {
  const asUrl = new URL(url)
  asUrl.searchParams.append('hash', URL_HASH)
  return asUrl.toString()
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

export const JSX_CANVAS_LOOKUP_FUNCTION_NAME = 'utopiaCanvasJSXLookup'

export function getDomRectCenter(rect: DOMRect): { x: number; y: number } {
  return {
    x: rect.x + rect.width / 2,
    y: rect.y + rect.height / 2,
  }
}

export function defaultDisplayTypeForHTMLElement(elementName: string): 'inline' | 'block' | null {
  // TODO global css overrides can change these defaults
  if (inlineHtmlElements.includes(elementName)) {
    return 'inline'
  } else if (blockLevelHtmlElements.includes(elementName)) {
    return 'block'
  } else {
    return null
  }
}

export function hugPropertiesFromComputedStyleMap(
  styleMap: StylePropertyMapReadOnly | null,
  display: string,
  globalFrame: MaybeInfinityCanvasRectangle | null,
): HugPropertyWidthHeight {
  if (styleMap == null) {
    return {
      width: null,
      height: null,
    }
  }

  return {
    width: hugPropertyFromStyleValue(
      styleMap.get('width')?.toString() ?? 'auto',
      'width',
      display,
      globalFrame,
    ),
    height: hugPropertyFromStyleValue(
      styleMap.get('height')?.toString() ?? 'auto',
      'height',
      display,
      globalFrame,
    ),
  }
}

export function hugPropertyFromStyleValue(
  value: string,
  property: 'width' | 'height',
  display: string,
  globalFrame: MaybeInfinityCanvasRectangle | null,
): HugProperty | null {
  const hugProp = (() => {
    if (value === 'auto' && property === 'width' && display === 'block') {
      return null // TODO: in this case this is a fill, unify this with fill detection
    }
    if (value === 'auto' || value === 'max-content') {
      return 'hug'
    }
    if (value === 'min-content') {
      return 'squeeze'
    }

    return null
  })()

  if (isNotNullFiniteRectangle(globalFrame) && globalFrame[property] === 0 && hugProp != null) {
    return 'collapsed'
  }
  return hugProp
}
