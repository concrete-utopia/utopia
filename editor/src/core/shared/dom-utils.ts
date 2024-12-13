import type { ReactDOM } from 'react'
import type { CanvasRectangle, MaybeInfinityCanvasRectangle, SimpleRectangle } from './math-utils'
import {
  boundingRectangle,
  boundingRectangleArray,
  canvasRectangle,
  isNotNullFiniteRectangle,
  roundToNearestHalf,
  scaleRect,
} from './math-utils'
import { URL_HASH } from '../../common/env-vars'
import { blockLevelHtmlElements, inlineHtmlElements } from '../../utils/html-elements'
import { assertNever, identity } from './utils'
import {
  arbitraryBlockRanToEnd,
  earlyReturnResult,
  earlyReturnVoid,
  type HugProperty,
  type HugPropertyWidthHeight,
} from './element-template'
import type { AbsolutePin } from '../../components/canvas/canvas-strategies/strategies/resize-helpers'
import type { MapLike } from 'typescript'

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

export const TextElements = [
  'text',
  'p',
  'span',
  'h1',
  'h2',
  'h3',
  'h4',
  'h5',
  'h6',
  'strong',
  'small',
  'mark',
  'i',
  'em',
  'b',
  'del',
  'ins',
  'sub',
  'sup',
]

export const PossibleTextElements = [
  ...intrinsicHTMLElementNamesThatSupportChildren,
  ...TextElements,
]

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

export function getRoundingFn(rounding: 'nearest-half' | 'no-rounding') {
  switch (rounding) {
    case 'nearest-half':
      return roundToNearestHalf
    case 'no-rounding':
      return identity
    default:
      assertNever(rounding)
  }
}

export type ElementCanvasRectangleCache = Map<HTMLElement, { [key: string]: CanvasRectangle }>

export function domRectToScaledCanvasRectangle(
  rect: SimpleRectangle,
  scale: number,
  roundingFn: (value: number) => number,
): CanvasRectangle {
  // canvas container uses scale for <1 zoom level, it should not affect the frame of the element.
  return scaleRect(
    canvasRectangle({
      x: roundingFn(rect.x),
      y: roundingFn(rect.y),
      width: roundingFn(rect.width),
      height: roundingFn(rect.height),
    }),
    scale,
  )
}

export function getCanvasRectangleFromElement(
  element: HTMLElement,
  canvasScale: number,
  withContent: 'without-text-content' | 'with-text-content' | 'only-text-content',
  rounding: 'nearest-half' | 'no-rounding',
  elementCanvasRectangleCache: ElementCanvasRectangleCache,
): CanvasRectangle {
  const cacheKey: string = `${canvasScale}-${withContent}-${rounding}`
  const elementCacheValue = elementCanvasRectangleCache.get(element)
  if (elementCacheValue != null) {
    const cachedRectangle = elementCacheValue[cacheKey]
    if (cachedRectangle != null) {
      return cachedRectangle
    }
  }

  function returnAddToCache(rect: CanvasRectangle): CanvasRectangle {
    if (elementCacheValue == null) {
      elementCanvasRectangleCache.set(element, { [cacheKey]: rect })
    } else {
      elementCacheValue[cacheKey] = rect
    }
    return rect
  }

  const scale = 1 / canvasScale

  const roundingFn = getRoundingFn(rounding)

  switch (withContent) {
    case 'without-text-content': {
      const boundingRect = element.getBoundingClientRect()
      const elementRect = domRectToScaledCanvasRectangle(boundingRect, scale, roundingFn)
      return returnAddToCache(elementRect)
    }
    case 'only-text-content':
    case 'with-text-content':
      let rectangles: Array<CanvasRectangle> = []
      for (const childNode of element.childNodes) {
        if (childNode.nodeType === Node.TEXT_NODE) {
          const range = document.createRange()
          // this is needed because jsdom can throw an error on the range.getBoundingClientRect() call, see https://github.com/jsdom/jsdom/issues/3002
          if (typeof range.getBoundingClientRect === 'function') {
            range.selectNode(childNode)
            rectangles.push(
              domRectToScaledCanvasRectangle(range.getBoundingClientRect(), scale, roundingFn),
            )
          }
        }
      }
      if (withContent === 'with-text-content') {
        rectangles.push(
          domRectToScaledCanvasRectangle(element.getBoundingClientRect(), scale, roundingFn),
        )
      }
      return returnAddToCache(
        boundingRectangleArray(rectangles) ?? canvasRectangle({ x: 0, y: 0, width: 0, height: 0 }),
      )
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

export const BLOCK_RAN_TO_END_FUNCTION_NAME = 'utopiaCanvasBlockRanToEnd'

export const EARLY_RETURN_RESULT_FUNCTION_NAME = 'utopiaCanvasEarlyReturnResult'

export const EARLY_RETURN_VOID_FUNCTION_NAME = 'utopiaCanvasEarlyReturnVoid'

export function applyBlockReturnFunctions(scope: MapLike<any>): void {
  scope[BLOCK_RAN_TO_END_FUNCTION_NAME] = arbitraryBlockRanToEnd
  scope[EARLY_RETURN_RESULT_FUNCTION_NAME] = earlyReturnResult
  scope[EARLY_RETURN_VOID_FUNCTION_NAME] = earlyReturnVoid
}

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

export function hugPropertiesFromStyleMap(
  getStyleValue: (key: AbsolutePin | 'display') => string | null,
  globalFrame: MaybeInfinityCanvasRectangle | null,
): HugPropertyWidthHeight {
  const pins = (
    ['left', 'top', 'right', 'bottom'] as Array<'left' | 'top' | 'right' | 'bottom'>
  ).filter((pin) => {
    const pinValue = getStyleValue(pin)
    return pinValue != null && pinValue != 'auto'
  })

  const display = getStyleValue('display')
  return {
    width: hugPropertyFromStyleValue(
      getStyleValue('width') ?? 'auto',
      'width',
      pins,
      display,
      globalFrame,
    ),
    height: hugPropertyFromStyleValue(
      getStyleValue('height') ?? 'auto',
      'height',
      pins,
      display,
      globalFrame,
    ),
  }
}

function hugPropertyFromStyleValue(
  value: string,
  property: 'width' | 'height',
  pins: Array<'left' | 'top' | 'right' | 'bottom'>,
  display: string | null,
  globalFrame: MaybeInfinityCanvasRectangle | null,
): HugProperty | null {
  const hugProp = (() => {
    // width/height max-content and min-content are efective even when the dimensions are overspecified, e.g. left, right and width are all set
    if (value === 'max-content') {
      return 'hug'
    }
    if (value === 'min-content') {
      return 'squeeze'
    }
    // when the pins specify the width/height and no width/height is not set (or set to auto), it is not hugging
    if (property === 'width' && pins.includes('left') && pins.includes('right')) {
      return null
    }
    if (property === 'height' && pins.includes('top') && pins.includes('bottom')) {
      return null
    }
    // width is not set neither explicitly nor by the pins, but the display is block, then it is not hugging
    if (value === 'auto' && property === 'width' && display === 'block') {
      return null // TODO: in this case this is a fill, unify this with fill detection
    }
    // width/height is not set neither explicitly nor by the pins, in this case it hugs
    if (value === 'auto') {
      return 'hug'
    }

    return null
  })()

  if (isNotNullFiniteRectangle(globalFrame) && globalFrame[property] === 0 && hugProp != null) {
    return 'collapsed'
  }
  return hugProp
}

export function parseHtml(html: string): Document | null {
  try {
    const parser = new DOMParser()
    const doc = parser.parseFromString(html, 'text/html')
    return doc
  } catch (e) {
    return null
  }
}

export function getRootElement(doc: Document): Element | null {
  const body = doc.body
  if (body != null) {
    return body.firstElementChild
  }
  return null
}

export function getMainScriptElement(doc: Document): Element | null {
  const body = doc.body
  if (body != null) {
    const scriptElement = body.querySelector('script[type="module"]')
    if (scriptElement != null) {
      return scriptElement
    }
  }
  return null
}
